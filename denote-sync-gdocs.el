;;; denote-sync-gdocs.el --- Google Docs backend for denote-sync -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (denote "3.0.0") (annotated-completing-read "0.1.0"))
;; Keywords: convenience, files, tools
;; URL: https://github.com/tychoish/denote-sync

;;; Commentary:
;; Google Docs backend for denote-sync, wrapping the `gog' CLI.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'denote)
(require 'denote-sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables

(defgroup denote-sync-gdocs nil
  "Google Docs backend for denote-sync."
  :group 'denote-sync
  :prefix "denote-sync-gdocs-")

(defcustom denote-sync-gdocs-gog-executable "gog"
  "Executable name or path for the gog CLI."
  :type 'string
  :group 'denote-sync-gdocs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process execution

(defun denote-sync-gdocs--debug-log (cmd-args stdout stderr)
  "Log CMD-ARGS, STDOUT, and STDERR to the `*denote-sync-debug*' buffer."
  (let ((buf (get-buffer-create "*denote-sync-debug*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "=== %s ===\ncmd: %s\nstdout:\n%s\nstderr:\n%s\n\n"
                      (format-time-string "%Y-%m-%dT%T")
                      (string-join cmd-args " ")
                      stdout stderr)))))

(defun denote-sync-gdocs--build-command (args &optional account)
  "Build the complete argv list for gog with ARGS and optional ACCOUNT."
  (let ((cmd (list denote-sync-gdocs-gog-executable)))
    (when (and account (not (string-empty-p account)))
      (setq cmd (append cmd (list (format "--account=%s" account)))))
    (append cmd args)))

(defun denote-sync-gdocs--run (args &optional account)
  "Run gog with ARGS synchronously; return (EXIT-CODE STDOUT STDERR).
Checks exit code 2 to report auth failure actionable errors."
  (let* ((cmd (denote-sync-gdocs--build-command args account))
         (stdout-buf (generate-new-buffer " *gog-sync-stdout*"))
         (stderr-file (make-temp-file "gog-stderr-"))
         (exit-code
          (unwind-protect
              (apply #'call-process
                     (car cmd)
                     nil
                     (list stdout-buf stderr-file)
                     nil
                     (cdr cmd))
            nil))
         (stdout (with-current-buffer stdout-buf (buffer-string)))
         (stderr (with-temp-buffer
                   (insert-file-contents stderr-file)
                   (delete-file stderr-file)
                   (buffer-string))))
    (kill-buffer stdout-buf)
    (denote-sync-gdocs--debug-log cmd stdout stderr)
    (cond
     ((= exit-code 2)
      (user-error "gog authentication failed for account %s: run 'gog auth login'"
                  (or account "default")))
     ((not (zerop exit-code))
      (user-error "gog %s failed (exit %d): %s"
                  (car args) exit-code
                  (if (string-empty-p stderr) stdout stderr))))
    (list exit-code stdout stderr)))

(defun denote-sync-gdocs--run-json (args &optional account)
  "Run gog with ARGS (ensuring `--json') synchronously; return parsed JSON."
  (let* ((json-args (if (member "--json" args) args (append args '("--json"))))
         (result (denote-sync-gdocs--run json-args account))
         (stdout (nth 1 result)))
    (condition-case err
        (json-parse-string stdout :object-type 'alist :array-type 'list)
      (json-parse-error
       (user-error "Failed to parse gog output as JSON: %s\nOutput was: %s"
                   (error-message-string err) stdout)))))

(defun denote-sync-gdocs--run-async (args callback &optional account)
  "Run gog with ARGS asynchronously, invoking (CALLBACK EXIT-CODE STDOUT STDERR)."
  (let* ((cmd (denote-sync-gdocs--build-command args account))
         (stdout-buf (generate-new-buffer " *gog-async-stdout*"))
         (stderr-buf (generate-new-buffer " *gog-async-stderr*"))
         (proc (make-process
                :name "denote-sync-gdocs-async"
                :buffer stdout-buf
                :stderr stderr-buf
                :command cmd
                :noquery t
                :sentinel
                (lambda (p _event)
                  (unless (process-live-p p)
                    (let ((exit-code (process-exit-status p))
                          (stdout (with-current-buffer stdout-buf (buffer-string)))
                          (stderr (with-current-buffer stderr-buf (buffer-string))))
                      (kill-buffer stdout-buf)
                      (kill-buffer stderr-buf)
                      (denote-sync-gdocs--debug-log cmd stdout stderr)
                      (if (= exit-code 2)
                          (funcall callback
                                   (format "gog authentication failed for account %s: run 'gog auth login'"
                                           (or account "default"))
                                   stdout stderr)
                        (funcall callback exit-code stdout stderr))))))))
    proc))

(defun denote-sync-gdocs--run-json-async (args callback &optional account)
  "Run gog with ARGS (ensuring `--json') asynchronously.
Calls (CALLBACK ERROR RESULT)."
  (let ((json-args (if (member "--json" args) args (append args '("--json")))))
    (denote-sync-gdocs--run-async
     json-args
     (lambda (exit-or-err stdout stderr)
       (if (stringp exit-or-err)
           (funcall callback exit-or-err nil)
         (if (not (zerop exit-or-err))
             (funcall callback
                      (format "gog %s failed (exit %d): %s"
                              (car args) exit-or-err
                              (if (string-empty-p stderr) stdout stderr))
                      nil)
           (condition-case err
               (let ((parsed (json-parse-string stdout :object-type 'alist :array-type 'list)))
                 (funcall callback nil parsed))
             (json-parse-error
              (funcall callback
                       (format "Failed to parse gog output as JSON: %s\nOutput was: %s"
                               (error-message-string err) stdout)
                       nil))))))
     account)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google Docs / Drive helpers

(defun denote-sync-gdocs--extract-doc-id (id-or-url)
  "Return the document ID from ID-OR-URL."
  (let ((trimmed (string-trim id-or-url)))
    (if (string-match "docs\\.google\\.com/document/d/\\([a-zA-Z0-9_-]+\\)" trimmed)
        (match-string 1 trimmed)
      trimmed)))

(defun denote-sync-gdocs--link-url (id)
  "Return the Google Docs edit URL for ID."
  (format "https://docs.google.com/document/d/%s/edit" id))

(defun denote-sync-gdocs--format-parent (parent)
  "Format PARENT, an (ACCOUNT . FOLDER-ID) cons, for human display."
  (format "account: %s, folder: %s"
          (or (car parent) "default")
          (or (cdr parent) "root")))

;;;###autoload
(defun denote-sync-gdocs-add-parent (name account folder-id)
  "Add a Google Docs parent target to `denote-sync-parent-registry'."
  (interactive
   (list (read-string "Registry entry name: ")
         (let ((acct (read-string "Google account (email, or empty for default): ")))
           (unless (string-empty-p acct) acct))
         (let ((folder (read-string "Google Drive folder ID (or empty for root): ")))
           (unless (string-empty-p folder) folder))))
  (let* ((parent (cons account folder-id))
         (entry (cons name (list 'google-docs parent)))
         (form (format "(%S . (google-docs . (%S . nil)))" name parent)))
    (setq denote-sync-parent-registry
          (cons entry (assoc-delete-all name (copy-sequence denote-sync-parent-registry))))
    (kill-new form)
    (message "Added %s -> google-docs (%s) to denote-sync-parent-registry (form copied to kill ring)"
             name (denote-sync-gdocs--format-parent parent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend protocol implementation

(defun denote-sync-gdocs--file-account (file)
  "Return the Google account associated with FILE, or nil."
  (let ((acct (denote-sync-frontmatter-get file "gdoc_account")))
    (when (and acct (not (string-empty-p acct)) (not (equal acct "\"\"")))
      (string-trim acct "\"" "\""))))

(defun denote-sync-gdocs--create (file parent content)
  "Create a new Google Doc for FILE under PARENT with CONTENT."
  (let* ((actual-parent (cond
                         ((and (consp parent) (consp (car parent))) (car parent))
                         ((consp parent) parent)
                         (t (cons nil nil))))
         (account (car actual-parent))
         (folder (cdr actual-parent))
         (title (denote-sync--export-title file))
         (temp-file (make-temp-file "gdoc-create-" nil ".md")))
    (with-temp-file temp-file (insert content))
    (unwind-protect
        (let* ((args (list "docs" "create" title "--file" temp-file "--json"))
               (args (if (and folder (not (string-empty-p folder)))
                         (append args (list (format "--parent=%s" folder)))
                       args))
               (result (denote-sync-gdocs--run-json args account))
               (created (map-elt result 'file))
               (id (map-elt created 'id))
               (meta (denote-sync-gdocs--fetch-remote id nil account))
               (created-time (plist-get meta :created-time))
               (edited-time (plist-get meta :edited-time))
               (url (or (map-elt created 'webViewLink) (denote-sync-gdocs--link-url id))))
          (when (and account (not (string-empty-p account)))
            (denote-sync-frontmatter-set file "gdoc_account" account))
          (when (and folder (not (string-empty-p folder)))
            (denote-sync-frontmatter-set file "gdoc_folder" folder))
          (list :id id :url url :created-time created-time :edited-time edited-time))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun denote-sync-gdocs--update (file id content _force)
  "Update existing Google Doc ID for FILE with CONTENT."
  (let* ((account (denote-sync-gdocs--file-account file))
         (title (denote-sync--export-title file))
         (temp-file (make-temp-file "gdoc-write-" nil ".md")))
    (with-temp-file temp-file (insert content))
    (unwind-protect
        (progn
          (denote-sync-gdocs--run
           (list "docs" "write" id "--replace" "--markdown" "--file" temp-file)
           account)
          ;; Update title if renamed
          (condition-case nil
              (denote-sync-gdocs--run (list "drive" "rename" id title) account)
            (error nil))
          (let* ((meta (denote-sync-gdocs--fetch-remote id nil account))
                 (edited (plist-get meta :edited-time)))
            (list :id id :url (denote-sync-gdocs--link-url id) :edited-time edited)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun denote-sync-gdocs--fetch-remote (id &optional fetch-body-p account)
  "Fetch remote metadata and optional Markdown body for Google Doc ID."
  (let* ((meta (denote-sync-gdocs--run-json (list "drive" "get" id) account))
         (f (map-elt meta 'file))
         (name (map-elt f 'name))
         (created (map-elt f 'createdTime))
         (edited (map-elt f 'modifiedTime))
         (body
          (when fetch-body-p
            (let ((temp-out (make-temp-file "gdoc-export-" nil ".md")))
              (unwind-protect
                  (progn
                    (denote-sync-gdocs--run
                     (list "docs" "export" id "--format" "md" "--out" temp-out "--overwrite")
                     account)
                    (with-temp-buffer
                      (insert-file-contents temp-out)
                      (buffer-string)))
                (when (file-exists-p temp-out)
                  (delete-file temp-out)))))))
    (list :id id :name name :created-time created :edited-time edited :body body)))

(defun denote-sync-gdocs--async-fetch-remote (id callback &optional fetch-body-p account)
  "Asynchronously fetch remote metadata and optional body for Google Doc ID."
  (denote-sync-gdocs--run-json-async
   (list "drive" "get" id)
   (lambda (error result)
     (if error
         (funcall callback error nil)
       (let* ((f (map-elt result 'file))
              (name (map-elt f 'name))
              (created (map-elt f 'createdTime))
              (edited (map-elt f 'modifiedTime)))
         (if (not fetch-body-p)
             (funcall callback nil (list :id id :name name :created-time created :edited-time edited))
           ;; If body needed, run docs export
           (let ((temp-out (make-temp-file "gdoc-export-" nil ".md")))
             (denote-sync-gdocs--run-async
              (list "docs" "export" id "--format" "md" "--out" temp-out "--overwrite")
              (lambda (err-or-code _stdout _stderr)
                (unwind-protect
                    (if (or (stringp err-or-code) (not (zerop err-or-code)))
                        (funcall callback (format "Export failed: %s" err-or-code) nil)
                      (let ((body (with-temp-buffer
                                    (insert-file-contents temp-out)
                                    (buffer-string))))
                        (funcall callback nil (list :id id :name name :created-time created :edited-time edited :body body))))
                  (when (file-exists-p temp-out)
                    (delete-file temp-out))))
              account))))))
   account))

(defun denote-sync-gdocs--async-push (file id content callback)
  "Asynchronously push CONTENT to Google Doc ID for FILE."
  (let* ((account (denote-sync-gdocs--file-account file))
         (temp-file (make-temp-file "gdoc-async-write-" nil ".md")))
    (with-temp-file temp-file (insert content))
    (denote-sync-gdocs--run-async
     (list "docs" "write" id "--replace" "--markdown" "--file" temp-file)
     (lambda (err-or-code _stdout _stderr)
       (when (file-exists-p temp-file) (delete-file temp-file))
       (if (or (stringp err-or-code) (not (zerop err-or-code)))
           (funcall callback (format "docs write failed: %s" err-or-code) nil)
         (denote-sync-gdocs--async-fetch-remote
          id
          (lambda (err2 meta)
            (if err2
                (funcall callback err2 nil)
              (funcall callback nil (list :id id
                                          :url (denote-sync-gdocs--link-url id)
                                          :edited-time (plist-get meta :edited-time)))))
          nil
          account)))
     account)))

(defun denote-sync-gdocs--apply-refresh (file id meta)
  "Apply META (with :body and :edited-time) to FILE for ID."
  (let ((body (plist-get meta :body))
        (edited (plist-get meta :edited-time)))
    (when body
      (denote-sync--write-body file body)
      (denote-sync--record-synced-content file (alist-get 'google-docs denote-sync-backends) id body edited))
    (when edited
      (denote-sync-frontmatter-set file "gdoc_edited" edited))))

(defun denote-sync-gdocs--refresh-file (file id)
  "Synchronously refresh FILE from Google Doc ID."
  (let* ((account (denote-sync-gdocs--file-account file))
         (meta (denote-sync-gdocs--fetch-remote id t account)))
    (denote-sync-gdocs--apply-refresh file id meta)))

(defun denote-sync-gdocs--async-refresh (file id callback)
  "Asynchronously refresh FILE from Google Doc ID."
  (let ((account (denote-sync-gdocs--file-account file)))
    (denote-sync-gdocs--async-fetch-remote
     id
     (lambda (error meta)
       (if error
           (funcall callback error)
         (denote-sync-gdocs--apply-refresh file id meta)
         (funcall callback nil)))
     t
     account)))

(defun denote-sync-gdocs--import-doc (id)
  "Import a new Denote note from Google Doc ID."
  (let* ((meta (denote-sync-gdocs--fetch-remote id t))
         (name (plist-get meta :name))
         (created (plist-get meta :created-time))
         (edited (plist-get meta :edited-time))
         (body (or (plist-get meta :body) "")))
    (denote (if (and name (not (string-empty-p name))) name "Untitled Google Doc import")
            '("gdoc") 'markdown-yaml nil created)
    (let ((new-file (buffer-file-name)))
      (denote-sync--write-body new-file body)
      (denote-sync-frontmatter-set new-file "gdoc_id" id)
      (denote-sync-frontmatter-set new-file "gdoc_created" (or created ""))
      (denote-sync-frontmatter-set new-file "gdoc_edited" (or edited ""))
      (denote-sync--record-synced-content new-file (alist-get 'google-docs denote-sync-backends) id body edited)
      (save-buffer)
      (message "Imported %s" (file-name-nondirectory new-file))
      new-file)))

(defun denote-sync-gdocs--read-parent ()
  "Interactively prompt for a Google Docs parent."
  (let* ((acct (read-string "Google account (email, or empty for default): "))
         (folder (read-string "Google Drive folder ID (or empty for root): ")))
    (cons (unless (string-empty-p acct) acct)
          (unless (string-empty-p folder) folder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend registration

(denote-sync-register-backend
 (denote-sync-backend-create
  :name 'google-docs
  :frontmatter-prefix "gdoc"
  :create-fn #'denote-sync-gdocs--create
  :update-fn #'denote-sync-gdocs--update
  :fetch-remote-fn #'denote-sync-gdocs--fetch-remote
  :link-url-fn #'denote-sync-gdocs--link-url
  :extract-id-fn #'denote-sync-gdocs--extract-doc-id
  :async-fetch-remote-fn #'denote-sync-gdocs--async-fetch-remote
  :async-push-fn #'denote-sync-gdocs--async-push
  :async-refresh-fn #'denote-sync-gdocs--async-refresh
  :refresh-fn #'denote-sync-gdocs--refresh-file
  :import-fn #'denote-sync-gdocs--import-doc
  :read-parent-fn #'denote-sync-gdocs--read-parent
  :format-parent-fn #'denote-sync-gdocs--format-parent
  :parse-parent-fn nil))

(provide 'denote-sync-gdocs)
;;; denote-sync-gdocs.el ends here
