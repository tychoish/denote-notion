;;; denote-sync.el --- Multi-backend synchronization engine for Denote notes -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (denote "3.0.0") (annotated-completing-read "0.1.0") (ox-gfm "1.0") (gen "0.1.0"))
;; Keywords: convenience, files, tools
;; URL: https://github.com/tychoish/denote-sync

;;; Commentary:
;; A backend-agnostic synchronization engine for Denote notes.
;; Provides content-hash change detection, three-way ediff conflict resolution,
;; bounded concurrent batch sync, and denote-dash saved views, parameterized
;; across pluggable backends (such as Notion and Google Docs).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'generator)
(require 'denote)
(require 'annotated-completing-read)
(require 'gen)
(require 'ox)
(require 'ediff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend protocol struct & registry

(cl-defstruct (denote-sync-backend
               (:constructor denote-sync-backend-create)
               (:copier nil))
  "Protocol structure defining a `denote-sync' backend."
  name                  ; symbol, e.g. 'notion, 'google-docs
  frontmatter-prefix    ; string, e.g. "notion", "gdoc"
  create-fn             ; (file parent content) -> plist (:id ... :url ... :edited-time ... [:created-time ...])
  update-fn             ; (file id content force) -> plist (:id ... :url ... :edited-time ...)
  fetch-remote-fn       ; (id &optional fetch-body-p) -> plist (:id ... :edited-time ... [:body ...])
  link-url-fn           ; (id) -> string url
  extract-id-fn         ; (id-or-url) -> bare id string
  ;; Async primitives for batch sync & async dirty markers
  async-fetch-remote-fn ; (id callback &optional fetch-body-p) -> (funcall callback error plist)
  async-push-fn         ; (file id content callback) -> (funcall callback error plist)
  async-refresh-fn      ; (file id callback) -> (funcall callback error)
  ;; Interactive & refresh operations
  refresh-fn            ; (file id) -> refreshes tracked file
  import-fn             ; (id) -> creates a new denote note and returns created file
  read-parent-fn        ; (optional) -> prompts and returns backend-specific parent
  format-parent-fn      ; (optional: parent) -> string for ACR annotation
  parse-parent-fn       ; (optional: string) -> parsed parent
  )

(defvar denote-sync-backends nil
  "Alist mapping backend symbol (e.g. \\='notion) to `denote-sync-backend' struct.")

(defun denote-sync-register-backend (backend)
  "Register BACKEND in `denote-sync-backends'."
  (setf (alist-get (denote-sync-backend-name backend) denote-sync-backends) backend))

(defun denote-sync-get-backend (name)
  "Return backend registered under symbol NAME, or signal an error."
  (or (alist-get name denote-sync-backends)
      (error "Unknown sync backend: %s" name)))

(defun denote-sync-registered-backends ()
  "Return list of all registered `denote-sync-backend' instances."
  (mapcar #'cdr denote-sync-backends))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables

(defgroup denote-sync nil
  "Two-way synchronization between Denote notes and remote services."
  :group 'denote
  :prefix "denote-sync-")

(defcustom denote-sync-cache-directory
  (locate-user-emacs-file "denote-sync-cache/")
  "Directory where last-synced Markdown bodies are cached for merge ancestors."
  :type 'directory
  :group 'denote-sync)

(defcustom denote-sync-default-backend 'notion
  "Default backend for `denote-sync' operations when unspecified."
  :type 'symbol
  :group 'denote-sync)

(defcustom denote-sync-default-parent nil
  "Default parent target used when pushing an untracked note.
Can be an entry name in `denote-sync-parent-registry', or a cons of
(BACKEND . PARENT)."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Registry entry name")
                 (cons :tag "Explicit backend and parent" symbol sexp))
  :group 'denote-sync)

(defcustom denote-sync-parent-registry nil
  "Alist of named parent targets.
Each entry is of the form:
  (NAME . (BACKEND . (PARENT . PROPERTIES)))
where NAME is a human-readable string, BACKEND is a symbol (e.g. \\='notion
or \\='google-docs), PARENT is backend-specific parent target data, and
PROPERTIES is an optional alist of metadata properties."
  :type '(alist :key-type string
                :value-type (cons symbol (cons sexp (alist :key-type symbol :value-type sexp))))
  :group 'denote-sync)

(defcustom denote-sync-export-auto-push-linked-notes nil
  "Whether to push untracked target notes when rewriting denote: links."
  :type 'boolean
  :group 'denote-sync)

(defcustom denote-sync-batch-max-concurrent-processes 4
  "Maximum number of concurrent subprocesses in `denote-sync-all'."
  :type 'integer
  :group 'denote-sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Front-matter helpers

(defun denote-sync--frontmatter-line-regexp (key)
  "Return a regexp matching a front-matter line for KEY."
  (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote key)))

(defun denote-sync-frontmatter-get (file key)
  "Return the raw string value of KEY from FILE's front matter, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (denote-sync--frontmatter-line-regexp key) nil t)
        (string-trim (match-string 1))))))

(defun denote-sync--frontmatter-format-value (value)
  "Format VALUE as a string suitable for YAML front matter."
  (cond
   ((null value) "\"\"")
   ((listp value)
    (concat "[" (string-join (mapcar (lambda (v) (format "%S" v)) value) ", ") "]"))
   (t (format "%S" value))))

(defun denote-sync-frontmatter-set (file key value)
  "Set KEY to VALUE in FILE's front matter.
Replaces the line if KEY exists; inserts it before the closing delimiter
if missing."
  (let ((formatted (denote-sync--frontmatter-format-value value)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (denote-sync--frontmatter-line-regexp key) nil t)
            (replace-match (concat key ": " formatted))
          (goto-char (point-min))
          (if (re-search-forward "^---[ \t]*$" nil t 2)
              (progn
                (goto-char (match-beginning 0))
                (insert key ": " formatted "\n"))
            (goto-char (point-min))
            (if (re-search-forward "^---[ \t]*$" nil t 1)
                (progn
                  (forward-line 1)
                  (insert key ": " formatted "\n"))
              (goto-char (point-min))
              (insert "---\n" key ": " formatted "\n---\n")))))
      (save-buffer))))

(defun denote-sync--frontmatter-nonempty-value-regexp (key)
  "Return a regexp matching KEY with a non-empty, non-quoted-empty value."
  (concat "^" (regexp-quote key) ":[ \t]*\\([^ \t\r\n\"']+\\|\"[^\"]+\"\\|'[^']+'\\)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend-specific frontmatter queries

(defun denote-sync--backend-prop-key (backend field)
  "Return the frontmatter key for FIELD in BACKEND."
  (format "%s_%s" (denote-sync-backend-frontmatter-prefix backend) field))

(defun denote-sync--get-id (file backend)
  "Return the remote document ID for FILE under BACKEND, or nil."
  (let ((val (denote-sync-frontmatter-get file (denote-sync--backend-prop-key backend "id"))))
    (when (and val (not (string-empty-p val)) (not (equal val "\"\"")))
      (string-trim val "\"" "\""))))

(defun denote-sync--get-sync-hash (file backend)
  "Return the stored sync content hash for FILE under BACKEND, or nil."
  (let ((val (denote-sync-frontmatter-get file (denote-sync--backend-prop-key backend "sync_hash"))))
    (when (and val (not (string-empty-p val)) (not (equal val "\"\"")))
      (string-trim val "\"" "\""))))

(defun denote-sync--get-edited (file backend)
  "Return the stored edited timestamp for FILE under BACKEND, or nil."
  (let ((val (denote-sync-frontmatter-get file (denote-sync--backend-prop-key backend "edited"))))
    (when (and val (not (string-empty-p val)) (not (equal val "\"\"")))
      (string-trim val "\"" "\""))))

(defun denote-sync--get-conflict (file backend)
  "Return non-nil if FILE has a conflict flag set for BACKEND."
  (let ((val (denote-sync-frontmatter-get file (denote-sync--backend-prop-key backend "conflict"))))
    (and val (string= val "t"))))

(defun denote-sync--get-remote-dirty (file backend)
  "Return non-nil if FILE is marked remotely dirty for BACKEND."
  (let ((val (denote-sync-frontmatter-get file (denote-sync--backend-prop-key backend "remote_dirty"))))
    (and val (string= val "t"))))

(defun denote-sync-tracked-p (file &optional backend)
  "Return non-nil if FILE is tracked by BACKEND (or any registered backend)."
  (if backend
      (let ((b (if (symbolp backend) (denote-sync-get-backend backend) backend)))
        (let ((id (denote-sync--get-id file b)))
          (and id (not (string-empty-p id)))))
    (seq-some (lambda (b) (denote-sync-tracked-p file b))
              (denote-sync-registered-backends))))

(defun denote-sync--tracked-backends (file)
  "Return a list of registered `denote-sync-backend' instances tracking FILE."
  (seq-filter (lambda (b) (denote-sync-tracked-p file b))
              (denote-sync-registered-backends)))

(defun denote-sync--conflicted-p (file &optional backend)
  "Return non-nil if FILE has a conflict flag set for BACKEND (or any backend)."
  (if backend
      (let ((b (if (symbolp backend) (denote-sync-get-backend backend) backend)))
        (denote-sync--get-conflict file b))
    (seq-some (lambda (b) (denote-sync--conflicted-p file b))
              (denote-sync-registered-backends))))

(defun denote-sync--conflicted-backends (file)
  "Return a list of registered `denote-sync-backend' instances for which
FILE is conflicted."
  (seq-filter (lambda (b) (denote-sync--conflicted-p file b))
              (denote-sync-registered-backends)))

(defun denote-sync--find-tracked-file (id &optional backend)
  "Return the denote file already tracking ID for BACKEND, or nil.
If BACKEND is nil, searches across all registered backends."
  (let ((backends (if backend
                      (list (if (symbolp backend) (denote-sync-get-backend backend) backend))
                    (denote-sync-registered-backends))))
    (seq-find (lambda (f)
                (seq-some (lambda (b)
                            (equal (denote-sync--get-id f b) id))
                          backends))
              (denote-directory-files))))

(defun denote-sync--detect-backend-from-id (id-or-url)
  "Infer the target backend from ID-OR-URL, or nil."
  (cond
   ((string-match-p "notion\\.\\(so\\|site\\)" id-or-url)
    (alist-get 'notion denote-sync-backends))
   ((string-match-p "docs\\.google\\.com" id-or-url)
    (alist-get 'google-docs denote-sync-backends))
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Body extraction, markdown conversion, link rewriting

(defun denote-sync--file-at-point ()
  "Return the denote file implied by the current point/buffer context, or nil."
  (cond
   ((and (fboundp 'denote-sequence-hierarchy-find-file)
         (derived-mode-p 'denote-sequence-hierarchy-mode))
    (denote-sequence-hierarchy-find-file))
   ((fboundp 'denote-dash--file-at-point)
    (denote-dash--file-at-point))
   ((fboundp 'denote-dash-file-at-point)
    (denote-dash-file-at-point))
   (t (buffer-file-name))))

(defun denote-sync--body-without-front-matter (file)
  "Return FILE's content with its Denote front matter block stripped."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (cond
     ((looking-at "^---[ \t]*$")
      (forward-line 1)
      (re-search-forward "^---[ \t]*$" nil t)
      (forward-line 1))
     (t
      (while (looking-at "^#\\+")
        (forward-line 1))))
    (skip-chars-forward "\n")
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun denote-sync--write-body (file body)
  "Replace FILE's body (everything after its front matter) with BODY."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (cond
     ((looking-at "^---[ \t]*$")
      (forward-line 1)
      (re-search-forward "^---[ \t]*$" nil t)
      (forward-line 1))
     (t
      (while (looking-at "^#\\+")
        (forward-line 1))))
    (skip-chars-forward "\n")
    (delete-region (point) (point-max))
    (insert body "\n")
    (save-buffer)))

(defun denote-sync--export-title (file)
  "Return FILE's title from Denote metadata or filename."
  (or (denote-retrieve-filename-title file)
      (file-name-base file)))

(defun denote-sync--org-to-markdown (org-body)
  "Convert ORG-BODY (a string of Org markup) to Markdown via `ox-gfm'."
  (require 'ox-gfm)
  (with-temp-buffer
    (insert org-body)
    (org-mode)
    (let ((md-buffer
           (cl-letf (((symbol-function 'denote-link-ol-export)
                      (lambda (link description _format)
                        (pcase-let ((`(,_path ,query ,_search)
                                     (denote-link--ol-resolve-link-to-target link :full-data)))
                          (format "[%s](denote:%s)" description query)))))
             (org-export-to-buffer 'gfm (generate-new-buffer-name "*denote-sync-md*")
                                   nil nil nil nil '(:with-toc nil)))))
      (unwind-protect
          (with-current-buffer md-buffer
            (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (when (buffer-live-p md-buffer)
          (kill-buffer md-buffer))))))

(defvar denote-sync--auto-push-in-flight nil
  "Hash table of Denote identifiers mid-push in the current call chain.")

(defun denote-sync--format-backend-link (desc target backend)
  "Format a markdown link for DESC to TARGET under BACKEND."
  (let* ((id (denote-sync--get-id target backend))
         (link-url (funcall (denote-sync-backend-link-url-fn backend) id)))
    (format "[%s](%s)" desc link-url)))

(defun denote-sync--auto-push-dependency (id target-file backend)
  "Push TARGET-FILE (Denote identifier ID) if untracked, breaking cycles."
  (cond
   ((denote-sync-tracked-p target-file backend) t)
   ((and denote-sync--auto-push-in-flight
         (gethash id denote-sync--auto-push-in-flight))
    nil)
   (t
    (when denote-sync--auto-push-in-flight
      (puthash id t denote-sync--auto-push-in-flight))
    (denote-sync-push target-file nil nil backend)
    t)))

(defun denote-sync--rewrite-denote-links (body source-file backend)
  "Rewrite `denote:' Markdown links in BODY to BACKEND's URLs."
  (let* (dangling
         (rewritten
          (replace-regexp-in-string
           denote-md-link-in-context-regexp
           (lambda (whole-match)
             (save-match-data
               (string-match denote-md-link-in-context-regexp whole-match)
               (let* ((id (match-string 1 whole-match))
                      (desc (match-string 2 whole-match))
                      (target (denote-get-path-by-id id)))
                 (cond
                  ((and target
                        (or (denote-sync-tracked-p target backend)
                            (and denote-sync-export-auto-push-linked-notes
                                 (if (and (eq (denote-sync-backend-name backend) 'notion)
                                          (fboundp 'denote-notion--auto-push-dependency))
                                     (denote-notion--auto-push-dependency id target)
                                   (denote-sync--auto-push-dependency id target backend)))))
                   (denote-sync--format-backend-link desc target backend))
                  (t
                   (push (list desc id
                               (cond
                                ((not target) 'missing-file)
                                (denote-sync-export-auto-push-linked-notes 'cycle-detected)
                                (t 'not-yet-pushed))
                               source-file)
                         dangling)
                   desc)))))
           body)))
    (cons rewritten (nreverse dangling))))

(defun denote-sync--export-body (file backend)
  "Extract FILE's exportable Markdown body for BACKEND."
  (let* ((body (denote-sync--body-without-front-matter file))
         (converted (if (eq (denote-filetype-heuristics file) 'org)
                        (denote-sync--org-to-markdown body)
                      body)))
    (denote-sync--rewrite-denote-links converted file backend)))

(defun denote-sync--report-dangling-links (dangling-links)
  "Report DANGLING-LINKS encounterd during push."
  (when dangling-links
    (let ((buf (get-buffer-create "*denote-sync-debug*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (dolist (link dangling-links)
          (pcase-let ((`(,desc ,id ,reason ,source-file) link))
            (insert (format "Dangling link in %s: %s (id: %s) - reason: %s\n"
                            (file-name-nondirectory source-file) desc id reason)))))
      (message "Push: %d denote: link(s) did not resolve to a remote document; see *denote-sync-debug*"
               (length dangling-links)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content cache & change detection

(defun denote-sync--cache-file-for (backend-name remote-id)
  "Return absolute path to cached last-synced body for BACKEND-NAME and REMOTE-ID."
  (unless (file-directory-p denote-sync-cache-directory)
    (make-directory denote-sync-cache-directory t))
  (expand-file-name (format "%s__%s" backend-name remote-id) denote-sync-cache-directory))

(defun denote-sync--cache-read (backend-name remote-id)
  "Return cached last-synced Markdown body for BACKEND-NAME and REMOTE-ID, or nil."
  (let ((cache-file (denote-sync--cache-file-for backend-name remote-id)))
    (when (file-exists-p cache-file)
      (with-temp-buffer
        (insert-file-contents cache-file)
        (buffer-string)))))

(defun denote-sync--cache-write (backend-name remote-id content)
  "Write CONTENT as cached last-synced body for BACKEND-NAME and REMOTE-ID."
  (with-temp-file (denote-sync--cache-file-for backend-name remote-id)
    (insert content)))

(defun denote-sync--content-hash (content)
  "Return SHA-1 content hash of CONTENT."
  (secure-hash 'sha1 content))

(defun denote-sync--record-synced-content (file backend id content &optional edited-time)
  "Record CONTENT and EDITED-TIME for FILE under BACKEND and ID."
  (let ((p (denote-sync-backend-frontmatter-prefix backend)))
    (denote-sync-frontmatter-set file (format "%s_sync_hash" p) (denote-sync--content-hash content))
    (when (and edited-time (not (string-empty-p edited-time)))
      (denote-sync-frontmatter-set file (format "%s_edited" p) edited-time))
    (denote-sync--cache-write (denote-sync-backend-name backend) id content)))

(defun denote-sync--classify-sync-state (local-changed-p remote-changed-p)
  "Classify sync state from LOCAL-CHANGED-P and REMOTE-CHANGED-P."
  (cond
   ((and local-changed-p remote-changed-p) 'both-changed)
   (local-changed-p 'local-only)
   (remote-changed-p 'remote-only)
   (t 'unchanged)))

(defun denote-sync--remote-timestamp-stale-p (stored-edited remote-edited)
  "Return non-nil if REMOTE-EDITED is newer than STORED-EDITED."
  (and remote-edited stored-edited
       (not (string-empty-p stored-edited))
       (string> remote-edited stored-edited)))

(defun denote-sync--sync-state (file backend)
  "Classify FILE's sync state relative to BACKEND."
  (unless (denote-sync-tracked-p file backend)
    (user-error "File is not tracked by backend %s: %s"
                (denote-sync-backend-name backend) file))
  (let* ((id (denote-sync--get-id file backend))
         (stored-hash (or (denote-sync--get-sync-hash file backend) ""))
         (stored-edited (or (denote-sync--get-edited file backend) ""))
         (local-hash (denote-sync--content-hash (car (denote-sync--export-body file backend))))
         (local-changed-p (or (string-empty-p stored-hash) (not (equal local-hash stored-hash))))
         (remote (funcall (denote-sync-backend-fetch-remote-fn backend) id nil))
         (remote-edited (plist-get remote :edited-time))
         (remote-changed-p (denote-sync--remote-timestamp-stale-p stored-edited remote-edited)))
    (denote-sync--classify-sync-state local-changed-p remote-changed-p)))

(defun denote-sync--sync-state-async (file backend callback)
  "Asynchronously classify FILE's sync state relative to BACKEND."
  (unless (denote-sync-tracked-p file backend)
    (user-error "File is not tracked by backend %s: %s"
                (denote-sync-backend-name backend) file))
  (let* ((id (denote-sync--get-id file backend))
         (stored-hash (or (denote-sync--get-sync-hash file backend) ""))
         (stored-edited (or (denote-sync--get-edited file backend) ""))
         (local-hash (denote-sync--content-hash (car (denote-sync--export-body file backend))))
         (local-changed-p (or (string-empty-p stored-hash) (not (equal local-hash stored-hash)))))
    (funcall (denote-sync-backend-async-fetch-remote-fn backend)
             id
             (lambda (error remote)
               (if error
                   (funcall callback error nil)
                 (let* ((remote-edited (plist-get remote :edited-time))
                        (remote-changed-p (denote-sync--remote-timestamp-stale-p stored-edited remote-edited)))
                   (funcall callback nil (denote-sync--classify-sync-state local-changed-p remote-changed-p)))))
             nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conflict resolution

(defun denote-sync--build-conflict-buffers (file backend id)
  "Return plist of conflict buffers (:local, :remote, :ancestor) for FILE."
  (let* ((bname (denote-sync-backend-name backend))
         (local-content (car (denote-sync--export-body file backend)))
         (remote-result (funcall (denote-sync-backend-fetch-remote-fn backend) id t))
         (remote-content (or (plist-get remote-result :body) ""))
         (ancestor-content (denote-sync--cache-read bname id))
         (local-buffer (generate-new-buffer (format "*denote-sync-conflict-local-%s-%s*" bname id)))
         (remote-buffer (generate-new-buffer (format "*denote-sync-conflict-remote-%s-%s*" bname id)))
         (ancestor-buffer (and ancestor-content
                               (generate-new-buffer
                                (format "*denote-sync-conflict-ancestor-%s-%s*" bname id)))))
    (with-current-buffer local-buffer (insert local-content))
    (with-current-buffer remote-buffer (insert remote-content))
    (when ancestor-buffer
      (with-current-buffer ancestor-buffer (insert ancestor-content)))
    (list :local local-buffer :remote remote-buffer :ancestor ancestor-buffer)))

(defun denote-sync--finish-conflict-resolution (file backend merged-content)
  "Write MERGED-CONTENT back into FILE, clear conflict flag, and force-push."
  (let ((p (denote-sync-backend-frontmatter-prefix backend)))
    (denote-sync--write-body file merged-content)
    (denote-sync-frontmatter-set file (format "%s_conflict" p) "")
    (if (and (null (cdr (denote-sync--tracked-backends file)))
             (not (null (denote-sync--tracked-backends file))))
        (denote-sync-push file nil t)
      (denote-sync-push file nil t backend))))

;;;###autoload
(defun denote-sync-resolve-conflict (&optional file backend)
  "Interactively resolve FILE's conflict flag via an ediff merge session.
FILE defaults to `denote-sync--file-at-point'.
If BACKEND is nil and FILE is conflicted for multiple backends, prompt
the user to pick which backend conflict to resolve."
  (interactive)
  (let* ((file (or file (denote-sync--file-at-point) (user-error "No file to resolve")))
         (conflicted (denote-sync--conflicted-backends file))
         (backend (cond
                   (backend (if (symbolp backend) (denote-sync-get-backend backend) backend))
                   ((null conflicted)
                    (if (denote-sync-tracked-p file)
                        (user-error "File has no conflict to resolve: %s" file)
                      (user-error "File is not tracked: %s" file)))
                   ((= (length conflicted) 1) (car conflicted))
                   (t
                    (let ((choice (completing-read
                                   "Resolve conflict for backend: "
                                   (mapcar (lambda (b) (symbol-name (denote-sync-backend-name b))) conflicted)
                                   nil t)))
                      (denote-sync-get-backend (intern choice))))))
         (p (denote-sync-backend-frontmatter-prefix backend))
         (id (denote-sync--get-id file backend)))
    (unless (denote-sync-tracked-p file backend)
      (user-error "File is not tracked by backend %s: %s" (denote-sync-backend-name backend) file))
    (unless (denote-sync--conflicted-p file backend)
      (user-error "File has no %s_conflict to resolve: %s" p file))
    (let* ((buffers (denote-sync--build-conflict-buffers file backend id))
           (local-buffer (plist-get buffers :local))
           (remote-buffer (plist-get buffers :remote))
           (ancestor-buffer (plist-get buffers :ancestor))
           (cleanup
            (lambda ()
              (dolist (buf (list local-buffer remote-buffer ancestor-buffer))
                (when (buffer-live-p buf) (kill-buffer buf)))))
           (finish
            (lambda ()
              (let ((merged (with-current-buffer (if ancestor-buffer ediff-buffer-C local-buffer)
                              (buffer-string))))
                (if (and (eq (denote-sync-backend-name backend) 'notion)
                         (fboundp 'denote-notion--finish-conflict-resolution))
                    (denote-notion--finish-conflict-resolution file merged)
                  (denote-sync--finish-conflict-resolution file backend merged))))))
      (if ancestor-buffer
          (ediff-merge-buffers-with-ancestor
           local-buffer remote-buffer ancestor-buffer
           (list (lambda ()
                   (add-hook 'ediff-quit-hook
                             (lambda ()
                               (funcall finish)
                               (funcall cleanup))
                             nil t))))
        (ediff-buffers
         local-buffer remote-buffer
         (list (lambda ()
                 (add-hook 'ediff-quit-hook
                           (lambda ()
                             (funcall finish)
                             (funcall cleanup))
                           nil t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parent selection & registry

(defun denote-sync--acr-select-parent (&optional backend-filter)
  "Select a parent target from `denote-sync-parent-registry' using ACR.
If BACKEND-FILTER is non-nil, only show entries for that backend."
  (let* ((candidates
          (seq-filter
           (lambda (entry)
             (if backend-filter
                 (eq (cadr entry) (denote-sync-backend-name backend-filter))
               t))
           denote-sync-parent-registry))
         (candidate-names (mapcar #'car candidates))
         (lookup-table (make-hash-table :test 'equal)))
    (dolist (entry candidates)
      (let* ((name (car entry))
             (bname (cadr entry))
             (pdata (cddr entry))
             (b (alist-get bname denote-sync-backends))
             (p-str (if (and b (denote-sync-backend-format-parent-fn b))
                        (funcall (denote-sync-backend-format-parent-fn b) (car pdata))
                      (format "%S" (car pdata)))))
        (puthash name (format " [%s: %s]" bname p-str) lookup-table)))
    (let ((chosen (annotated-completing-read
                   "Parent target: " candidate-names
                   :annotation-function (lambda (cand) (gethash cand lookup-table "")))))
      (assoc chosen denote-sync-parent-registry))))

(defun denote-sync--backend-for-parent (parent)
  "Determine the backend for PARENT if possible."
  (cond
   ((and (stringp parent) (assoc parent denote-sync-parent-registry))
    (denote-sync-get-backend (cadr (assoc parent denote-sync-parent-registry))))
   ((and (consp parent) (symbolp (car parent)) (alist-get (car parent) denote-sync-backends))
    (denote-sync-get-backend (car parent)))
   ((= (length (denote-sync-registered-backends)) 1)
    (car (denote-sync-registered-backends)))
   (t nil)))

(defun denote-sync--read-parent (&optional backend)
  "Prompt for and return a cons of (BACKEND . PARENT).
Honors `denote-sync-default-parent' and `denote-sync-parent-registry'."
  (cond
   ((and denote-sync-default-parent (stringp denote-sync-default-parent)
         (assoc denote-sync-default-parent denote-sync-parent-registry))
    (let ((entry (assoc denote-sync-default-parent denote-sync-parent-registry)))
      (cons (denote-sync-get-backend (cadr entry)) (caddr entry))))
   ((and denote-sync-default-parent (consp denote-sync-default-parent))
    (if (alist-get (car denote-sync-default-parent) denote-sync-backends)
        (cons (denote-sync-get-backend (car denote-sync-default-parent))
              (cdr denote-sync-default-parent))
      (let ((b (or backend
                   (and (boundp 'denote-sync-default-backend)
                        denote-sync-default-backend
                        (alist-get denote-sync-default-backend denote-sync-backends))
                   (car (denote-sync-registered-backends))
                   (denote-sync-get-backend 'notion))))
        (cons b denote-sync-default-parent))))
   ((and denote-sync-parent-registry
         (or (null backend)
             (seq-some (lambda (e) (eq (cadr e) (denote-sync-backend-name backend)))
                       denote-sync-parent-registry)))
    (let ((entry (denote-sync--acr-select-parent backend)))
      (cons (denote-sync-get-backend (cadr entry)) (caddr entry))))
   (t
    (let* ((b (or backend
                  (let ((registered (denote-sync-registered-backends)))
                    (cond
                     ((= (length registered) 1) (car registered))
                     (t
                      (let ((choice (completing-read
                                     "Sync backend: "
                                     (mapcar (lambda (bk) (symbol-name (denote-sync-backend-name bk)))
                                             registered)
                                     nil t)))
                        (denote-sync-get-backend (intern choice))))))))
           (p (if (denote-sync-backend-read-parent-fn b)
                  (funcall (denote-sync-backend-read-parent-fn b))
                (read-string (format "%s parent target: " (denote-sync-backend-name b))))))
      (cons b p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export & Push

(defun denote-sync--export-update (file backend force)
  "Update FILE's remote document for BACKEND.
With FORCE non-nil, overwrite without checking sync-state."
  (let* ((p (denote-sync-backend-frontmatter-prefix backend))
         (id (denote-sync--get-id file backend))
         (state (unless force (denote-sync--sync-state file backend))))
    (when (eq state 'both-changed)
      (denote-sync-frontmatter-set file (format "%s_conflict" p) t)
      (message
       "%s item %s changed since the last sync (remote and local both changed); marked %s_conflict -- run `denote-sync-resolve-conflict' to reconcile"
       (capitalize (symbol-name (denote-sync-backend-name backend))) id p))
    (if (memq state '(unchanged both-changed))
        (progn
          (unless (eq state 'both-changed)
            (message "%s item %s already in sync; nothing to push"
                     (capitalize (symbol-name (denote-sync-backend-name backend))) id))
          (cons (funcall (denote-sync-backend-link-url-fn backend) id) nil))
      (pcase-let ((`(,content . ,dangling) (denote-sync--export-body file backend)))
        (let ((result (funcall (denote-sync-backend-update-fn backend) file id content force)))
          (denote-sync--record-synced-content file backend id content (plist-get result :edited-time))
          (cons (plist-get result :url) dangling))))))

(defun denote-sync--export-create (file backend parent)
  "Create a new remote document for FILE under BACKEND and PARENT."
  (pcase-let ((`(,content . ,dangling) (denote-sync--export-body file backend)))
    (let* ((result (funcall (denote-sync-backend-create-fn backend) file parent content))
           (id (plist-get result :id))
           (url (plist-get result :url))
           (edited (plist-get result :edited-time))
           (created (plist-get result :created-time))
           (p (denote-sync-backend-frontmatter-prefix backend)))
      (denote-sync-frontmatter-set file (format "%s_id" p) id)
      (when (and created (not (string-empty-p created)))
        (denote-sync-frontmatter-set file (format "%s_created" p) created))
      (denote-sync--record-synced-content file backend id content edited)
      (cons url dangling))))

;;;###autoload
(defun denote-sync-push (&optional file parent force backend)
  "Push FILE (default current buffer note) to remote.
If FILE is tracked, updates its remote document.
If FILE is untracked, PARENT is required (prompted if nil).
If BACKEND is nil:
- If FILE is tracked by 1 backend, uses it.
- If FILE is tracked by multiple backends, prompts to select backend.
- If FILE is untracked, uses registry selection or prompts for backend."
  (interactive (list nil nil current-prefix-arg nil))
  (let ((denote-sync--auto-push-in-flight
         (or denote-sync--auto-push-in-flight (make-hash-table :test 'equal)))
        (file (or file (denote-sync--file-at-point) (user-error "No file to export"))))
    (puthash (denote-retrieve-filename-identifier file) t denote-sync--auto-push-in-flight)
    (let* ((tracked (denote-sync--tracked-backends file))
           (target-backend
            (cond
             (backend (if (symbolp backend) (denote-sync-get-backend backend) backend))
             ((= (length tracked) 1) (car tracked))
             ((> (length tracked) 1)
              (if noninteractive
                  (car tracked)
                (let ((choice (completing-read
                               "Push to backend: "
                               (mapcar (lambda (b) (symbol-name (denote-sync-backend-name b))) tracked)
                               nil t)))
                  (denote-sync-get-backend (intern choice)))))
             (parent (denote-sync--backend-for-parent parent))
             (t nil))))
      (if (and target-backend (denote-sync-tracked-p file target-backend))
          (pcase-let ((`(,url . ,dangling) (denote-sync--export-update file target-backend force)))
            (message "Exported to %s" url)
            (denote-sync--report-dangling-links dangling))
        (let* ((target-info (if (and parent target-backend)
                                (cons target-backend parent)
                              (denote-sync--read-parent target-backend)))
               (b (car target-info))
               (p (cdr target-info)))
          (pcase-let ((`(,url . ,dangling) (denote-sync--export-create file b p)))
            (message "Exported to %s" url)
            (denote-sync--report-dangling-links dangling)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import & Pull

;;;###autoload
(defun denote-sync-pull (&optional remote-id target-file backend)
  "Pull a remote document into Denote, creating or refreshing as needed.
Without REMOTE-ID, refreshes TARGET-FILE (or current buffer file).
With REMOTE-ID (an id or URL):
- If already tracked by any note, refreshes that note.
- Otherwise imports a new note from the remote document."
  (interactive
   (list (let ((file (denote-sync--file-at-point)))
           (unless (and file (denote-sync-tracked-p file))
             (read-string "Remote document ID or URL: ")))))
  (let* ((file (or target-file (denote-sync--file-at-point)))
         (target-backend (when backend
                           (if (symbolp backend) (denote-sync-get-backend backend) backend))))
    (if (not remote-id)
        (progn
          (unless (and file (denote-sync-tracked-p file target-backend))
            (user-error "No file to refresh: not tracked, and no document ID given"))
          (let* ((tracked (if target-backend
                              (list target-backend)
                            (denote-sync--tracked-backends file)))
                 (b (cond
                     ((= (length tracked) 1) (car tracked))
                     (noninteractive (car tracked))
                     (t
                      (let ((choice (completing-read
                                     "Refresh backend: "
                                     (mapcar (lambda (bk) (symbol-name (denote-sync-backend-name bk))) tracked)
                                     nil t)))
                        (denote-sync-get-backend (intern choice))))))
                 (id (denote-sync--get-id file b)))
            (funcall (denote-sync-backend-refresh-fn b) file id)
            (message "Refreshed %s from %s"
                     (file-name-nondirectory file)
                     (capitalize (symbol-name (denote-sync-backend-name b))))))
      (let* ((b (or target-backend
                    (denote-sync--detect-backend-from-id remote-id)
                    (let ((registered (denote-sync-registered-backends)))
                      (cond
                       ((= (length registered) 1) (car registered))
                       ((and (boundp 'denote-sync-default-backend)
                             denote-sync-default-backend
                             (alist-get denote-sync-default-backend denote-sync-backends))
                        (alist-get denote-sync-default-backend denote-sync-backends))
                       (noninteractive (car registered))
                       (t
                        (let ((choice (completing-read
                                       "Remote backend: "
                                       (mapcar (lambda (bk) (symbol-name (denote-sync-backend-name bk)))
                                               registered)
                                       nil t)))
                          (denote-sync-get-backend (intern choice))))))))
             (id (funcall (denote-sync-backend-extract-id-fn b) remote-id))
             (target (or target-file
                         (when (and file (equal (denote-sync--get-id file b) id))
                           file)
                         (denote-sync--find-tracked-file id b))))
        (if target
            (progn
              (funcall (denote-sync-backend-refresh-fn b) target id)
              (message "Refreshed %s from %s"
                       (file-name-nondirectory target)
                       (capitalize (symbol-name (denote-sync-backend-name b)))))
          (funcall (denote-sync-backend-import-fn b) id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Batch sync runner

(defun denote-sync--batch-run (generator max-concurrent process-fn on-complete)
  "Drive PROCESS-FN over GENERATOR with at most MAX-CONCURRENT items in flight."
  (let ((in-flight 0)
        (exhausted nil)
        (iter (gen-iter generator)))
    (letrec
        ((maybe-finish
          (lambda ()
            (when (and exhausted (zerop in-flight))
              (funcall on-complete))))
         (dispatch-one
          (lambda ()
            (unless exhausted
              (let (item got-item)
                (condition-case nil
                    (progn (setq item (iter-next iter)) (setq got-item t))
                  (iter-end-of-sequence (setq exhausted t)))
                (when got-item
                  (setq in-flight (1+ in-flight))
                  (funcall process-fn
                           item
                           (lambda (&rest _ignored)
                             (setq in-flight (1- in-flight))
                             (funcall dispatch-one)
                             (funcall maybe-finish)))))))))
      (if (<= max-concurrent 0)
          (funcall on-complete)
        (dotimes (_ max-concurrent) (funcall dispatch-one))
        (funcall maybe-finish)))))

(defun denote-sync--sync-all-process-note (item counts &optional backend-counts-or-done done-fn)
  "Process ITEM (a (FILE . BACKEND) cons or FILE) for batch sync."
  (let* ((done (if done-fn done-fn backend-counts-or-done))
         (backend-counts (when done-fn backend-counts-or-done))
         (file (if (consp item) (car item) item))
         (backend (cond
                   ((and (consp item) (denote-sync-backend-p (cdr item))) (cdr item))
                   (t (or (car (denote-sync--tracked-backends file))
                          (denote-sync-get-backend 'notion)))))
         (bname (denote-sync-backend-name backend))
         (p (denote-sync-backend-frontmatter-prefix backend))
         (b-counts (when backend-counts (alist-get bname backend-counts))))
    (cl-flet ((record (key)
                (cl-incf (cdr (assq key counts)))
                (when b-counts (cl-incf (cdr (assq key b-counts))))
                (funcall done)))
      (condition-case nil
          (let ((state-async-fn
                 (if (and (eq bname 'notion)
                          (fboundp 'denote-notion--sync-state-async))
                     (lambda (cb) (denote-notion--sync-state-async file cb))
                   (lambda (cb) (denote-sync--sync-state-async file backend cb)))))
            (funcall
             state-async-fn
             (lambda (error state)
               (cond
                (error (record 'errored))
                (t
                 (pcase state
                   ('unchanged (record 'unchanged))
                   ('local-only
                    (let ((id (denote-sync--get-id file backend)))
                      (condition-case nil
                          (pcase-let ((`(,content . ,_dangling) (denote-sync--export-body file backend)))
                            (if (and (eq bname 'notion)
                                     (fboundp 'denote-notion--export-push-async))
                                (denote-notion--export-push-async
                                 file
                                 (lambda (err _res)
                                   (if err (record 'errored) (record 'pushed))))
                              (funcall (denote-sync-backend-async-push-fn backend)
                                       file id content
                                       (lambda (err res)
                                         (if err
                                             (record 'errored)
                                           (denote-sync--record-synced-content
                                            file backend id content (plist-get res :edited-time))
                                           (record 'pushed))))))
                        (error (record 'errored)))))
                   ('remote-only
                    (let ((id (denote-sync--get-id file backend)))
                      (condition-case nil
                          (funcall (denote-sync-backend-async-refresh-fn backend)
                                   file id
                                   (lambda (err)
                                     (record (if err 'errored 'pulled))))
                        (error (record 'errored)))))
                   ('both-changed
                    (denote-sync-frontmatter-set file (format "%s_conflict" p) t)
                    (record 'conflicted))))))))
        (error (record 'errored))))))

;;;###autoload
(defun denote-sync-all ()
  "Batch-sync every tracked note across all backends via a bounded async pool."
  (interactive)
  (let* ((counts (list (cons 'unchanged 0)
                       (cons 'pushed 0)
                       (cons 'pulled 0)
                       (cons 'conflicted 0)
                       (cons 'errored 0)))
         (backends (denote-sync-registered-backends))
         (backend-counts (mapcar (lambda (b)
                                   (cons (denote-sync-backend-name b)
                                         (list (cons 'unchanged 0)
                                               (cons 'pushed 0)
                                               (cons 'pulled 0)
                                               (cons 'conflicted 0)
                                               (cons 'errored 0))))
                                 backends))
         (files (denote-directory-files))
         (tracked-pairs nil))
    (dolist (f files)
      (dolist (b backends)
        (when (denote-sync-tracked-p f b)
          (push (cons f b) tracked-pairs))))
    (setq tracked-pairs (nreverse tracked-pairs))
    (let* ((pairs-iter (iter-make (while tracked-pairs (iter-yield (pop tracked-pairs)))))
           (generator (gen-wrap pairs-iter)))
      (denote-sync--batch-run
       generator
       denote-sync-batch-max-concurrent-processes
       (lambda (item done)
         (denote-sync--sync-all-process-note item counts backend-counts done))
       (lambda ()
         (let* ((total-msg
                 (format "denote-sync-all: %d unchanged, %d pushed, %d pulled, %d conflicted, %d errored"
                         (cdr (assq 'unchanged counts))
                         (cdr (assq 'pushed counts))
                         (cdr (assq 'pulled counts))
                         (cdr (assq 'conflicted counts))
                         (cdr (assq 'errored counts))))
                (lines (list total-msg)))
           (dolist (b-entry backend-counts)
             (let* ((bname (car b-entry))
                    (bc (cdr b-entry)))
               (push (format "  (%s: %d unchanged, %d pushed, %d pulled, %d conflicted, %d errored)"
                             bname
                             (cdr (assq 'unchanged bc))
                             (cdr (assq 'pushed bc))
                             (cdr (assq 'pulled bc))
                             (cdr (assq 'conflicted bc))
                             (cdr (assq 'errored bc)))
                     lines)))
           (message "%s" (string-join (nreverse lines) "\n"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote dirty markers & denote-dash views

(defun denote-sync--remote-dirty-p (file &optional backend)
  "Return non-nil if FILE is marked remotely dirty for BACKEND (or any backend)."
  (if backend
      (denote-sync--get-remote-dirty file (if (symbolp backend) (denote-sync-get-backend backend) backend))
    (seq-some (lambda (b) (denote-sync--remote-dirty-p file b))
              (denote-sync-registered-backends))))

(defun denote-sync--mark-remote-dirty (file backend dirty)
  "Set FILE's remote dirty marker for BACKEND to DIRTY."
  (let ((p (denote-sync-backend-frontmatter-prefix backend)))
    (denote-sync-frontmatter-set file (format "%s_remote_dirty" p) (if dirty t ""))))

(defun denote-sync--refresh-remote-dirty-marker-async (file backend on-done)
  "Asynchronously check if FILE's remote document in BACKEND has advanced."
  (let ((id (denote-sync--get-id file backend))
        (stored-edited (denote-sync--get-edited file backend)))
    (if (or (null id) (null stored-edited) (string-empty-p stored-edited))
        (funcall on-done)
      (funcall (denote-sync-backend-async-fetch-remote-fn backend)
               id
               (lambda (error remote)
                 (if error
                     (funcall on-done)
                   (let* ((remote-edited (plist-get remote :edited-time))
                          (stale (denote-sync--remote-timestamp-stale-p stored-edited remote-edited)))
                     (denote-sync--mark-remote-dirty file backend stale)
                     (funcall on-done))))
               nil))))

(defvar denote-dash-saved-views)
(declare-function make-denote-dash-view "denote-dash")
(declare-function denote-dash-view-name "denote-dash")
(declare-function denote-dash-open-view "denote-dash")

(defun denote-sync--dash-register-view (name grep-filter)
  "Register or update a saved view named NAME in `denote-dash-saved-views'."
  (require 'denote-dash)
  (setq denote-dash-saved-views
        (cons (make-denote-dash-view :name name :grep-filter grep-filter)
              (seq-remove (lambda (v) (equal (denote-dash-view-name v) name))
                          denote-dash-saved-views))))

;;;###autoload
(defun denote-sync-dash-view-tracked (&optional backend)
  "Register and open a `denote-dash' saved view for notes tracked by BACKEND (or default)."
  (interactive)
  (let* ((b (if backend
                (if (symbolp backend) (denote-sync-get-backend backend) backend)
              (if (and (boundp 'denote-sync-default-backend)
                       denote-sync-default-backend
                       (alist-get denote-sync-default-backend denote-sync-backends))
                  (denote-sync-get-backend denote-sync-default-backend)
                (car (denote-sync-registered-backends))))))
    (when b
      (let* ((name (format "%s: tracked" (denote-sync-backend-name b)))
             (p (denote-sync-backend-frontmatter-prefix b))
             (regex (denote-sync--frontmatter-nonempty-value-regexp (format "%s_id" p))))
        (denote-sync--dash-register-view name regex)
        (denote-dash-open-view name)))))

;;;###autoload
(defun denote-sync-dash-view-conflicts (&optional backend)
  "Register and open a `denote-dash' saved view for conflicted notes under BACKEND (or default)."
  (interactive)
  (let* ((b (if backend
                (if (symbolp backend) (denote-sync-get-backend backend) backend)
              (if (and (boundp 'denote-sync-default-backend)
                       denote-sync-default-backend
                       (alist-get denote-sync-default-backend denote-sync-backends))
                  (denote-sync-get-backend denote-sync-default-backend)
                (car (denote-sync-registered-backends))))))
    (when b
      (let* ((name (format "%s: conflicts" (denote-sync-backend-name b)))
             (p (denote-sync-backend-frontmatter-prefix b))
             (regex (format "^%s_conflict:[ \t]*t[ \t]*$" p)))
        (denote-sync--dash-register-view name regex)
        (denote-dash-open-view name)))))

;;;###autoload
(defun denote-sync-dash-view-remote-updated (&optional backend)
  "Register and open a `denote-dash' saved view for remote-updated notes under BACKEND (or default)."
  (interactive)
  (let* ((b (if backend
                (if (symbolp backend) (denote-sync-get-backend backend) backend)
              (if (and (boundp 'denote-sync-default-backend)
                       denote-sync-default-backend
                       (alist-get denote-sync-default-backend denote-sync-backends))
                  (denote-sync-get-backend denote-sync-default-backend)
                (car (denote-sync-registered-backends))))))
    (when b
      (let* ((name (format "%s: remote-updated" (denote-sync-backend-name b)))
             (p (denote-sync-backend-frontmatter-prefix b))
             (regex (format "^%s_remote_dirty:[ \t]*t[ \t]*$" p)))
        (denote-sync--dash-register-view name regex)
        (dolist (file (denote-directory-files))
          (when (denote-sync-tracked-p file b)
            (if (and (eq (denote-sync-backend-name b) 'notion)
                     (fboundp 'denote-notion--refresh-remote-dirty-marker-async))
                (denote-notion--refresh-remote-dirty-marker-async file #'ignore)
              (denote-sync--refresh-remote-dirty-marker-async file b #'ignore))))
        (denote-dash-open-view name)))))

(provide 'denote-sync)
;;; denote-sync.el ends here
