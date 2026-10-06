;;; denote-sync-notion.el --- Notion backend for denote-sync -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (denote "3.0.0") (annotated-completing-read "0.1.0"))
;; Keywords: convenience, files, tools
;; URL: https://github.com/tychoish/denote-sync

;;; Commentary:
;; Notion backend for denote-sync, wrapping the `npx ntn' CLI.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'denote)
(require 'denote-sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables

(defgroup denote-sync-notion nil
  "Notion backend for denote-sync."
  :group 'denote-sync
  :prefix "denote-sync-notion-")

(defcustom denote-sync-notion-ntn-executable "npx"
  "Executable name or path for the ntn CLI wrapper."
  :type 'string
  :group 'denote-sync-notion)

(defcustom denote-sync-notion-ntn-args '("ntn")
  "Arguments passed before the subcommand to `denote-sync-notion-ntn-executable'."
  :type '(repeat string)
  :group 'denote-sync-notion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process execution

(defun denote-sync-notion--debug-log (args stdout stderr)
  "Log ARGS, STDOUT, and STDERR to the `*denote-sync-debug*' buffer."
  (let ((buf (get-buffer-create "*denote-sync-debug*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "=== %s ===\ncmd: %s\nstdout:\n%s\nstderr:\n%s\n\n"
                      (format-time-string "%Y-%m-%dT%T")
                      (string-join (append (list denote-sync-notion-ntn-executable)
                                           denote-sync-notion-ntn-args
                                           args)
                                   " ")
                      stdout stderr)))))

(defun denote-sync-notion--run (args)
  "Run the ntn CLI with ARGS synchronously; return (EXIT-CODE STDOUT STDERR)."
  (with-temp-buffer
    (let* ((stdout-buf (current-buffer))
           (stderr-file (make-temp-file "ntn-stderr-"))
           (cmd-args (append denote-sync-notion-ntn-args args))
           (exit-code
            (unwind-protect
                (apply #'call-process
                       denote-sync-notion-ntn-executable
                       nil
                       (list stdout-buf stderr-file)
                       nil
                       cmd-args)
              nil))
           (stdout (buffer-string))
           (stderr (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (delete-file stderr-file)
                     (buffer-string))))
      (denote-sync-notion--debug-log args stdout stderr)
      (list exit-code stdout stderr))))

(defun denote-sync-notion--run-json (args)
  "Run ntn with ARGS (adding `--json'); return parsed JSON alist, or signal error."
  (let* ((json-args (if (member "--json" args) args (append args '("--json"))))
         (result (denote-sync-notion--run json-args))
         (exit-code (nth 0 result))
         (stdout (nth 1 result))
         (stderr (nth 2 result)))
    (unless (zerop exit-code)
      (user-error "ntn %s failed (exit %d): %s"
                  (car args) exit-code
                  (if (string-empty-p stderr) stdout stderr)))
    (condition-case err
        (json-parse-string stdout :object-type 'alist :array-type 'list)
      (json-parse-error
       (user-error "Failed to parse ntn output as JSON: %s\nOutput was: %s"
                   (error-message-string err) stdout)))))

(defun denote-sync-notion--run-async (args callback)
  "Run ntn with ARGS asynchronously, invoking (CALLBACK EXIT-CODE STDOUT STDERR)."
  (let* ((stdout-buf (generate-new-buffer " *ntn-async-stdout*"))
         (stderr-buf (generate-new-buffer " *ntn-async-stderr*"))
         (cmd-args (append denote-sync-notion-ntn-args args))
         (cmd (append (list denote-sync-notion-ntn-executable) cmd-args))
         (proc (make-process
                :name "denote-sync-notion-async"
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
                      (denote-sync-notion--debug-log args stdout stderr)
                      (funcall callback exit-code stdout stderr)))))))
    proc))

(defun denote-sync-notion--run-json-async (args callback)
  "Run ntn with ARGS (adding `--json') asynchronously.
Calls (CALLBACK ERROR RESULT)."
  (let ((json-args (if (member "--json" args) args (append args '("--json")))))
    (denote-sync-notion--run-async
     json-args
     (lambda (exit-code stdout stderr)
       (if (not (zerop exit-code))
           (funcall callback
                    (format "ntn %s failed (exit %d): %s"
                            (car args) exit-code
                            (if (string-empty-p stderr) stdout stderr))
                    nil)
         (condition-case err
             (let ((parsed (json-parse-string stdout :object-type 'alist :array-type 'list)))
               (funcall callback nil parsed))
           (json-parse-error
            (funcall callback
                     (format "Failed to parse ntn output as JSON: %s\nOutput was: %s"
                             (error-message-string err) stdout)
                     nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notion API helpers

(defun denote-sync-notion--rich-text-plain (rich-text-array)
  "Return the concatenated `plain_text' of RICH-TEXT-ARRAY."
  (mapconcat (lambda (segment) (or (map-elt segment 'plain_text) ""))
             rich-text-array ""))

(defun denote-sync-notion--rich-text-value (text)
  "Return a Notion rich_text property value array for the plain string TEXT."
  (vector (list (cons 'text (list (cons 'content text))))))

(defconst denote-sync-notion--property-sentinels
  `(("<today>" . ,(lambda () (format-time-string "%Y-%m-%d"))))
  "Alist of (SENTINEL . NILADIC-FN) recognized in Notion property values.")

(defun denote-sync-notion--merge-properties (&rest alists)
  "Merge ALISTS of Notion property values; earlier alists' keys win."
  (let (result seen)
    (dolist (alist alists)
      (dolist (kv alist)
        (unless (member (car kv) seen)
          (push (car kv) seen)
          (push kv result))))
    (nreverse result)))

(defun denote-sync-notion--resolve-property-sentinels (value)
  "Recursively replace sentinel strings in VALUE."
  (cond
   ((stringp value)
    (if-let* ((fn (cdr (assoc value denote-sync-notion--property-sentinels))))
        (funcall fn)
      value))
   ((and (consp value) (consp (car value)))
    (mapcar (lambda (kv) (cons (car kv) (denote-sync-notion--resolve-property-sentinels (cdr kv)))) value))
   ((listp value)
    (mapcar #'denote-sync-notion--resolve-property-sentinels value))
   (t value)))

(defconst denote-sync-notion--default-export-properties
  '((Timestamp (date (start . "<today>"))))
  "Built-in default Notion property values applied to newly created pages.")

(defun denote-sync-notion--export-properties (file)
  "Return FILE's `notion_properties' front-matter value, parsed but unresolved."
  (when-let* ((raw (denote-sync-frontmatter-get file "notion_properties"))
              (raw (and (not (string-empty-p raw)) raw)))
    (json-parse-string raw :object-type 'alist :array-type 'list)))

(defun denote-sync-notion--apply-properties (id properties)
  "PATCH Notion page ID's PROPERTIES."
  (when properties
    (denote-sync-notion--run-json
     (list "api" (format "v1/pages/%s" id)
           "--data" (json-serialize
                     (list (cons 'properties (denote-sync-notion--resolve-property-sentinels properties))))
           "-X" "PATCH"))))

(defun denote-sync-notion--set-page-title (id properties title)
  "PATCH page ID's title property (found via PROPERTIES) to TITLE."
  (when-let* ((title (and title (not (string-empty-p title)) title))
              (key (car (seq-find (lambda (kv) (equal (map-elt (cdr kv) 'type) "title"))
                                  properties))))
    (denote-sync-notion--apply-properties
     id (list (cons key (list (cons 'title (denote-sync-notion--rich-text-value title))))))))

(defun denote-sync-notion--set-tags-from-properties (file properties)
  "Set FILE's notion_tags from Notion page PROPERTIES' Tags, if any."
  (when-let* ((tags-prop (map-elt properties 'Tags))
              (multi-select (map-elt tags-prop 'multi_select))
              (names (seq-map (lambda (tag) (map-elt tag 'name)) multi-select)))
    (denote-sync-frontmatter-set file "notion_tags" names)))

(defun denote-sync-notion--extract-page-id (id-or-url)
  "Return the 32-char hex page id embedded in ID-OR-URL."
  (let ((trimmed (replace-regexp-in-string "[?#].*\\'" "" id-or-url)))
    (if (string-match "\\([0-9a-fA-F]\\{32\\}\\)\\'"
                      (replace-regexp-in-string "-" "" trimmed))
        (match-string 1 (replace-regexp-in-string "-" "" trimmed))
      id-or-url)))

(defun denote-sync-notion--clean-imported-body (body)
  "Clean up BODY as returned by `ntn pages get' for storage in a denote note."
  (let (fences)
    (setq body (replace-regexp-in-string
                "```[^\n]*\n\\(?:.\\|\n\\)*?\n```"
                (lambda (match)
                  (let ((idx (length fences)))
                    (push match fences)
                    (format "\0%d\0" idx)))
                body))
    (setq fences (vconcat (nreverse fences)))
    (setq body (thread-last body
                             (replace-regexp-in-string "\n" "\n\n")
                             (replace-regexp-in-string "<br[ \t]*/?>" "\n")
                             (replace-regexp-in-string (regexp-quote "\\[") "[")
                             (replace-regexp-in-string (regexp-quote "\\]") "]")))
    (replace-regexp-in-string
     "\0\\([0-9]+\\)\0"
     (lambda (match)
       (string-match "\0\\([0-9]+\\)\0" match)
       (aref fences (string-to-number (match-string 1 match))))
     body)))

(defun denote-sync-notion--parent-arg (parent)
  "Format PARENT, a (TYPE . ID) cons, as an `ntn --parent' string."
  (unless (and (consp parent) (memq (car parent) '(page database data-source)))
    (user-error "Unknown Notion parent type: %S (want page, database, or data-source)" (car parent)))
  (format "%s:%s" (car parent) (cdr parent)))

(defun denote-sync-notion--parse-parent-arg (string)
  "Parse STRING back to a (TYPE . ID) cons."
  (when (and string (not (string-empty-p string)))
    (when-let* ((pos (string-search ":" string)))
      (cons (intern (substring string 0 pos)) (substring string (1+ pos))))))

(defun denote-sync-notion--registry-entry-for-parent (parent)
  "Return the `denote-sync-parent-registry' entry matching PARENT, or nil."
  (when parent
    (seq-find (lambda (entry)
                (cond
                 ((eq (cadr entry) 'notion)
                  (let ((p (if (consp (cdr (cdr entry)))
                               (car (cdr (cdr entry)))
                             (cdr (cdr entry)))))
                    (equal p parent)))
                 ((not (symbolp (cadr entry)))
                  (equal (cadr entry) parent))
                 (t nil)))
              denote-sync-parent-registry)))

(defun denote-sync-notion--registry-entry-properties (entry)
  "Extract properties alist from registry ENTRY."
  (when entry
    (if (eq (cadr entry) 'notion)
        (cdr (cdr (cdr entry)))
      (cdr (cdr entry)))))

(defun denote-sync-notion--link-url (id)
  "Return canonical Notion URL for ID."
  (format "https://www.notion.so/%s" (string-replace "-" "" id)))

(defun denote-sync-notion--resolve-data-source (id-or-url)
  "Resolve ID-OR-URL to a Notion data source, returning an (ID . NAME) cons."
  (let* ((id (denote-sync-notion--extract-page-id id-or-url))
         (result (denote-sync-notion--run-json (list "datasources" "resolve" id)))
         (data-sources (map-elt result 'data_sources)))
    (pcase (length data-sources)
      (0 (user-error "No data source found for %s" id-or-url))
      (1 (let ((data-source (car data-sources)))
           (cons (map-elt data-source 'id) (map-elt data-source 'name))))
      (_ (let* ((table (mapcar (lambda (data-source)
                                 (cons (map-elt data-source 'name) (map-elt data-source 'id)))
                               data-sources))
                (name (completing-read "Data source: " table nil t)))
           (cons (cdr (assoc name table)) name))))))

;;;###autoload
(defun denote-sync-notion-add-parent (url &optional name)
  "Resolve URL to a Notion data source and add it to `denote-sync-parent-registry'."
  (interactive "sNotion URL or id: ")
  (let* ((resolved (denote-sync-notion--resolve-data-source url))
         (id (car resolved))
         (name (or name
                   (and (called-interactively-p 'interactive)
                        (read-string "Registry name: " (cdr resolved)))
                   (cdr resolved)))
         (entry (list name 'notion (cons 'data-source id)))
         (form (format "(%S notion (data-source . %S))" name id)))
    (setq denote-sync-parent-registry
          (cons entry (seq-remove (lambda (e) (equal (car e) name))
                                  denote-sync-parent-registry)))
    (kill-new form)
    (message "Added %s -> notion data-source:%s to denote-sync-parent-registry (form copied to kill ring)" name id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend protocol implementation

(defun denote-sync-notion--create (file parent content)
  "Create a new Notion page for FILE under PARENT with CONTENT."
  (let* ((actual-parent (cond
                         ((and (consp parent) (consp (car parent))) (car parent))
                         ((and (consp parent) (memq (car parent) '(page database data-source))) parent)
                         ((stringp parent) (denote-sync-notion--parse-parent-arg parent))
                         (t (user-error "Invalid Notion parent: %S" parent))))
         (registry-entry (denote-sync-notion--registry-entry-for-parent actual-parent))
         (registry-props (denote-sync-notion--registry-entry-properties registry-entry))
         (parent-arg (denote-sync-notion--parent-arg actual-parent))
         (page (denote-sync-notion--run-json
                (list "pages" "create" "--parent" parent-arg "--content" content)))
         (id (map-elt page 'id))
         (url (map-elt page 'url))
         (created (map-elt page 'created_time))
         (edited (map-elt page 'last_edited_time)))
    (denote-sync-frontmatter-set file "notion_parent" parent-arg)
    (denote-sync-notion--set-tags-from-properties file (map-elt page 'properties))
    (denote-sync-notion--set-page-title id (map-elt page 'properties)
                                        (denote-sync--export-title file))
    (denote-sync-notion--apply-properties
     id
     (denote-sync-notion--merge-properties (denote-sync-notion--export-properties file)
                                           registry-props
                                           denote-sync-notion--default-export-properties))
    (list :id id :url url :created-time created :edited-time edited)))

(defun denote-sync-notion--update (file id content _force)
  "Update Notion page ID with CONTENT for FILE."
  (let* ((stored-parent (string-trim (or (denote-sync-frontmatter-get file "notion_parent") "") "\"" "\""))
         (registry-entry (denote-sync-notion--registry-entry-for-parent
                          (denote-sync-notion--parse-parent-arg stored-parent)))
         (default-properties (denote-sync-notion--registry-entry-properties registry-entry)))
    (denote-sync-notion--run-json (list "pages" "edit" id "--content" content))
    (let* ((page (map-elt (denote-sync-notion--run-json (list "pages" "get" id)) 'page))
           (edited (map-elt page 'last_edited_time))
           (url (map-elt page 'url)))
      (denote-sync-notion--set-page-title id (map-elt page 'properties)
                                          (denote-sync--export-title file))
      (denote-sync-notion--apply-properties
       id (denote-sync-notion--merge-properties (denote-sync-notion--export-properties file)
                                                default-properties))
      (list :id id :url url :edited-time edited))))

(defun denote-sync-notion--fetch-remote (id &optional fetch-body-p)
  "Fetch remote metadata and optional body for Notion page ID."
  (let* ((result (denote-sync-notion--run-json (list "pages" "get" id)))
         (page (map-elt result 'page))
         (edited (map-elt page 'last_edited_time))
         (body (when fetch-body-p
                 (denote-sync-notion--clean-imported-body
                  (map-elt (map-elt result 'markdown) 'markdown)))))
    (list :id id :edited-time edited :body body)))

(defun denote-sync-notion--async-fetch-remote (id callback &optional fetch-body-p)
  "Asynchronously fetch remote metadata and optional body for Notion page ID."
  (denote-sync-notion--run-json-async
   (list "pages" "get" id)
   (lambda (error result)
     (if error
         (funcall callback error nil)
       (let* ((page (map-elt result 'page))
              (edited (map-elt page 'last_edited_time))
              (body (when fetch-body-p
                      (denote-sync-notion--clean-imported-body
                       (map-elt (map-elt result 'markdown) 'markdown)))))
         (funcall callback nil (list :id id :edited-time edited :body body)))))))

(defun denote-sync-notion--async-push (file id content callback)
  "Asynchronously push CONTENT to Notion page ID for FILE."
  (denote-sync-notion--run-json-async
   (list "pages" "edit" id "--content" content)
   (lambda (error _result)
     (if error
         (funcall callback error nil)
       (denote-sync-notion--run-json-async
        (list "pages" "get" id)
        (lambda (error2 result2)
          (if error2
              (funcall callback error2 nil)
            (let* ((page (map-elt result2 'page))
                   (url (map-elt page 'url))
                   (edited (map-elt page 'last_edited_time))
                   (stored-parent (string-trim (or (denote-sync-frontmatter-get file "notion_parent") "") "\"" "\""))
                   (registry-entry (denote-sync-notion--registry-entry-for-parent
                                    (denote-sync-notion--parse-parent-arg stored-parent)))
                   (default-properties (denote-sync-notion--registry-entry-properties registry-entry)))
              (denote-sync-notion--set-page-title id (map-elt page 'properties)
                                                  (denote-sync--export-title file))
              (denote-sync-notion--apply-properties
               id (denote-sync-notion--merge-properties (denote-sync-notion--export-properties file)
                                                        default-properties))
              (funcall callback nil (list :id id :url url :edited-time edited))))))))))

(defun denote-sync-notion--apply-refresh (file id result)
  "Apply `pages get' RESULT to FILE for ID."
  (let* ((page (map-elt result 'page))
         (body (denote-sync-notion--clean-imported-body
                (map-elt (map-elt result 'markdown) 'markdown))))
    (denote-sync--write-body file body)
    (denote-sync-frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
    (denote-sync--record-synced-content file (alist-get 'notion denote-sync-backends) id body)
    (denote-sync-notion--set-tags-from-properties file (map-elt page 'properties))))

(defun denote-sync-notion--refresh-file (file id)
  "Synchronously refresh FILE from Notion page ID."
  (let ((result (denote-sync-notion--run-json (list "pages" "get" id))))
    (denote-sync-notion--apply-refresh file id result)))

(defun denote-sync-notion--async-refresh (file id callback)
  "Asynchronously refresh FILE from Notion page ID."
  (denote-sync-notion--run-json-async
   (list "pages" "get" id)
   (lambda (error result)
     (if error
         (funcall callback error)
       (denote-sync-notion--apply-refresh file id result)
       (funcall callback nil)))))

(defun denote-sync-notion--import-page (id)
  "Import a new Denote note from Notion page ID."
  (let* ((result (denote-sync-notion--run-json (list "pages" "get" id)))
         (page (map-elt result 'page))
         (body (denote-sync-notion--clean-imported-body
                (map-elt (map-elt result 'markdown) 'markdown)))
         (properties (map-elt page 'properties))
         (title (denote-sync-notion--rich-text-plain
                 (map-elt (map-elt properties 'Name) 'title)))
         (tags (seq-map (lambda (tag) (map-elt tag 'name))
                        (map-elt (map-elt properties 'Tags) 'multi_select)))
         (created (map-elt page 'created_time))
         (edited (map-elt page 'last_edited_time)))
    (denote (if (and title (not (string-empty-p title))) title "Untitled Notion import")
            (cons "notion" tags) 'markdown-yaml nil created)
    (let ((new-file (buffer-file-name)))
      (denote-sync--write-body new-file body)
      (denote-sync-frontmatter-set new-file "notion_id" id)
      (denote-sync-frontmatter-set new-file "notion_tags" tags)
      (denote-sync-frontmatter-set new-file "notion_created" created)
      (denote-sync-frontmatter-set new-file "notion_edited" edited)
      (denote-sync--record-synced-content new-file (alist-get 'notion denote-sync-backends) id body)
      (save-buffer)
      (message "Imported %s" (file-name-nondirectory new-file))
      new-file)))

(defun denote-sync-notion--read-parent ()
  "Interactively prompt for a Notion parent."
  (let* ((type (intern (completing-read "Notion parent type: " '("page" "database" "data-source") nil t)))
         (id (read-string (format "Notion %s ID or URL: " type))))
    (cons type (denote-sync-notion--extract-page-id id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend registration

(denote-sync-register-backend
 (denote-sync-backend-create
  :name 'notion
  :frontmatter-prefix "notion"
  :create-fn #'denote-sync-notion--create
  :update-fn #'denote-sync-notion--update
  :fetch-remote-fn #'denote-sync-notion--fetch-remote
  :link-url-fn #'denote-sync-notion--link-url
  :extract-id-fn #'denote-sync-notion--extract-page-id
  :async-fetch-remote-fn #'denote-sync-notion--async-fetch-remote
  :async-push-fn #'denote-sync-notion--async-push
  :async-refresh-fn #'denote-sync-notion--async-refresh
  :refresh-fn #'denote-sync-notion--refresh-file
  :import-fn #'denote-sync-notion--import-page
  :read-parent-fn #'denote-sync-notion--read-parent
  :format-parent-fn #'denote-sync-notion--parent-arg
  :parse-parent-fn #'denote-sync-notion--parse-parent-arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notion-specific helpers and test shims

(defconst denote-notion--dash-view-tracked-name "notion: tracked")
(defconst denote-notion--dash-view-conflicts-name "notion: conflicts")
(defconst denote-notion--dash-view-remote-updated-name "notion: remote-updated")
(defconst denote-notion--dash-conflict-grep-filter "^notion_conflict:[ \t]*t[ \t]*$")
(defconst denote-notion--dash-remote-dirty-grep-filter "^notion_remote_dirty:[ \t]*t[ \t]*$")
(defconst denote-notion--debug-buffer-name "*denote-sync-debug*")
(defvaralias 'denote-notion--auto-push-in-flight 'denote-sync--auto-push-in-flight)

(defun denote-notion--tracked-p (file)
  "Return non-nil if FILE is tracked by Notion."
  (denote-sync-tracked-p file (denote-sync-get-backend 'notion)))

(defun denote-notion--conflicted-p (file)
  "Return non-nil if FILE is conflicted in Notion."
  (denote-sync--conflicted-p file (denote-sync-get-backend 'notion)))

(defun denote-notion--remote-dirty-p (file)
  "Return non-nil if FILE is marked remote-dirty in Notion."
  (denote-sync--remote-dirty-p file (denote-sync-get-backend 'notion)))

(defun denote-notion--mark-remote-dirty (file dirty-p)
  "Set FILE's remote-dirty marker in Notion."
  (denote-sync--mark-remote-dirty file (denote-sync-get-backend 'notion) dirty-p))

(defun denote-notion--build-conflict-buffers (file id)
  "Return conflict buffers for FILE and ID in Notion."
  (denote-sync--build-conflict-buffers file (denote-sync-get-backend 'notion) id))

(defun denote-notion--finish-conflict-resolution (file merged-content)
  "Finish conflict resolution for FILE with MERGED-CONTENT in Notion."
  (denote-sync--finish-conflict-resolution file (denote-sync-get-backend 'notion) merged-content))

(defun denote-notion--auto-push-dependency (id target-file)
  "Auto-push dependency ID at TARGET-FILE in Notion."
  (denote-sync--auto-push-dependency id target-file (denote-sync-get-backend 'notion)))

(defun denote-notion--sync-state-async (file callback)
  "Check sync state asynchronously for FILE in Notion."
  (denote-sync--sync-state-async file (denote-sync-get-backend 'notion) callback))

(defun denote-notion--export-push-async (file callback)
  "Export push asynchronously for FILE in Notion."
  (let* ((backend (denote-sync-get-backend 'notion))
         (id (denote-sync--get-id file backend)))
    (pcase-let ((`(,content . ,_dangling) (denote-sync--export-body file backend)))
      (funcall (denote-sync-backend-async-push-fn backend)
               file id content
               (lambda (err res)
                 (unless err
                   (denote-sync--record-synced-content
                    file backend id content (plist-get res :edited-time)))
                 (funcall callback err res))))))

(defun denote-notion--refresh-remote-dirty-marker-async (file callback)
  "Refresh remote dirty marker asynchronously for FILE in Notion."
  (denote-sync--refresh-remote-dirty-marker-async file (denote-sync-get-backend 'notion) callback))

(defun denote-notion--sync-all-process-note (file counts done)
  "Process FILE for batch sync with COUNTS."
  (denote-sync--sync-all-process-note file counts done))

(provide 'denote-sync-notion)
;;; denote-sync-notion.el ends here
