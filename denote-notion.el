;;; denote-notion.el --- Two-way sync between Denote notes and Notion pages -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (denote "3.0.0") (annotated-completing-read "0.1.0") (ox-gfm "20231215.1901") (gen "0.1.0"))
;; Keywords: docs, notion, denote, tools
;; URL: https://github.com/tychoish/denote-notion

;;; Commentary:
;; Push a Denote note to Notion as a page, or pull a Notion page back into
;; a Denote note, on demand via the npx ntn CLI.  Both directions are
;; manual/interactive.
;;
;; Entry points:
;;   denote-notion-push — create or update the Notion page for the
;;                        current note
;;   denote-notion-pull — pull a Notion page: with an id/URL, create or
;;                        refresh; with none, dwim-refresh the current
;;                        buffer's tracked page.

;;; Code:

(require 'seq)
(require 'map)
(require 'json)
(require 'cl-lib)
(require 'generator)
(require 'denote)

(require 'annotated-completing-read)
(require 'gen)
(require 'ox-gfm)
(require 'ediff)

(declare-function org-export-to-buffer "ox")
(declare-function denote-dash--file-at-point "denote-dash")
(declare-function denote-dash-file-at-point "denote-dash") ;; future proof
(declare-function denote-sequence-hierarchy-find-file "denote-sequence")
(declare-function denote-dash-open-view "denote-dash")
(declare-function denote-dash-refresh "denote-dash")
(declare-function make-denote-dash-view "denote-dash")
(declare-function denote-dash-view-name "denote-dash")
(declare-function denote-dash--view-buffer-name "denote-dash")
(defvar denote-dash-saved-views)

(defun denote-notion--file-at-point ()
  "Return the denote file implied by the current point/buffer context, or nil.
Uses `denote-dash--file-at-point' when `denote-dash' is loaded, so a note
that's merely selected at point in a `denote-dash' or sequence-hierarchy
listing resolves correctly instead of requiring the file to be the
current buffer's own visited file.  Falls back to `buffer-file-name'."
  (cond
   ((and (fboundp 'denote-sequence-hierarchy-find-file)
	 (derived-mode-p 'denote-sequence-hierarchy-mode))
    (denote-sequence-hierarchy-find-file))
   ((fboundp 'denote-dash--file-at-point)
    (denote-dash--file-at-point))
   ((fboundp 'denote-dash-file-at-point)
    (denote-dash-file-at-point))
   (t (buffer-file-name))))

;;; Custom variables

(defgroup denote-notion nil
  "Two-way sync between Denote notes and Notion pages."
  :group 'denote)

(defcustom denote-notion-default-parent nil
  "Default Notion parent for a first-time export, or nil to always prompt.
When set, a cons of (TYPE . ID): TYPE is one of the symbols `page',
`database', or `data-source'; ID is that resource's id string.  Takes
priorityh over `denote-notion-parent-registry' — set this only for a
single fixed target used non-interactively (e.g. from a headless script);
leave nil to pick per-export via the registry or a raw prompt."
  :type '(choice (const :tag "Always prompt" nil)
                  (cons (choice (const page) (const database) (const data-source))
                        string))
  :group 'denote-notion)

(defcustom denote-notion-parent-registry nil
  "Alist of (NAME . (PARENT . PROPERTIES)) named Notion export targets.
NAME is a short string shown in the `denote-notion--read-parent' ACR
menu.  PARENT is a (TYPE . ID) cons — TYPE one of the symbols `page',
`database', or `data-source'; ID that resource's id string.  PROPERTIES
is an alist of default Notion property values applied to every export
under this target — same shape as `denote-notion-export-properties',
e.g. `((Timestamp (date (start . \"<today>\")))) — or nil for none.  Set
per-machine, e.g. in casap.el for a personal workspace."
  :type '(alist :key-type string
                 :value-type (cons (cons (choice (const page) (const database) (const data-source))
                                          string)
                                    (alist :key-type symbol :value-type sexp)))
  :group 'denote-notion)

(defcustom denote-notion-cache-directory
  (expand-file-name "denote-notion-cache/" user-emacs-directory)
  "Directory holding last-synced Notion page body snapshots, one file per id.
Deliberately outside `denote-directory' (or any of its subdirectories) --
Denote itself, and tooling like `denote-dash'/`denote-sequence', scan
every file under the denote directory as a candidate note; a cache
snapshot of a Notion page's last-synced content is not itself a note
and must never show up in a listing, tag search, or sequence hierarchy
alongside real notes.

This default is deliberately plain -- a single, portable location under
`user-emacs-directory', with no assumption about any particular
multi-instance or XDG state-path convention a given Emacs setup may
use.  A setup that needs this cache scoped per host/instance (e.g. via
`sprite-state-path') should `setq' this variable to that scoped path as
part of its own init, the same way it already does for
`savehist-file'/`url-configuration-directory'/etc., rather than this
package guessing at or depending on that convention itself."
  :type 'directory
  :group 'denote-notion)

(defcustom denote-notion-export-auto-push-linked-notes nil
  "When non-nil, auto-push untracked `denote:'-linked note before fallback.
A `denote:' link found while exporting a note's body (see
`denote-notion--rewrite-denote-links') that points to a real but
not-yet-Notion-tracked note is, when this is non-nil, pushed first (via
`denote-notion-push', recursively) so the link can still resolve to a
Notion URL; when nil (the default), such a link always falls back to
plain text instead, exactly as it did before this option existed."
  :type 'boolean
  :group 'denote-notion)

(defcustom denote-notion-batch-max-concurrent-processes 4
  "Max number of concurrent `ntn' subprocesses `denote-notion-sync-all' runs.
Each tracked note's sync costs at least one, and sometimes two, `ntn'
invocations (see `denote-notion--sync-all-process-note').  Bounds how
many notes are ever mid-sync at once: too low serializes the whole
batch; too high spikes CPU/network and Node startup overhead across
every tracked note at once.  The default, 4, keeps Emacs responsive
without saturating a single machine's connection."
  :type 'natnum
  :group 'denote-notion)

(defun denote-notion--parent-arg (parent)
  "Format PARENT, a (TYPE . ID) cons, as an `ntn --parent' string.
TYPE is one of the symbols `page', `database', or `data-source'."
  (unless (memq (car parent) '(page database data-source))
    (user-error "Unknown Notion parent type: %S (want page, database, or data-source)" (car parent)))
  (format "%s:%s" (car parent) (cdr parent)))

(defun denote-notion--registry-entry-for-parent (parent)
  "Return the `denote-notion-parent-registry' entry whose PARENT cons matches.
PARENT is a (TYPE . ID) cons.  Returns (NAME . (PARENT . PROPERTIES)), or nil."
  (cl-find-if (lambda (entry) (equal (car (cdr entry)) parent))
              denote-notion-parent-registry))

(defun denote-notion--acr-select-parent ()
  "Select a (TYPE . ID) parent from `denote-notion-parent-registry' via ACR."
  (let* ((table (mapcar (lambda (entry)
                           (cons (car entry) (format "%s:%s" (car (car (cdr entry))) (cdr (car (cdr entry))))))
                         denote-notion-parent-registry))
         (name (annotated-completing-read
                table :prompt "Notion export target: " :require-match t
                :category 'denote-notion-parent)))
    (car (cdr (assoc name denote-notion-parent-registry)))))

;;; ntn process wrapper

(defun denote-notion--run (args)
  "Run \"npx ntn\" with ARGS and return a (EXIT-CODE STDOUT STDERR) list.
STDOUT and STDERR are captured separately — `npx' echoes an \"npm notice
run ...\" progress line to stderr that embeds the full command it's
about to run, and npm's notice logger re-prefixes every embedded newline
with its own \"npm notice \" marker; since a note's `--content' body is
always multi-line, mixing that into STDOUT (as a single `call-process'
buffer destination would) corrupts the JSON `denote-notion--run-json'
parses from it."
  (let ((stderr-file (make-temp-file "denote-notion-ntn-stderr")))
    (unwind-protect
        (with-temp-buffer
          (let ((exit-code (apply #'call-process "npx" nil (list (current-buffer) stderr-file) nil "ntn" args)))
            (list exit-code (buffer-string)
                  (with-temp-buffer
                    (insert-file-contents stderr-file)
                    (buffer-string)))))
      (delete-file stderr-file))))

(defconst denote-notion--debug-buffer-name "*denote-notion-debug*"
  "Name of buffer holding raw `ntn' output for `denote-notion--run-json'.")

(defun denote-notion--debug-log (args stdout stderr)
  "Append the ntn invocation ARGS and its raw STDOUT/STDERR to the debug buffer.
Recorded for every call, not just failures, since a payload that parses
as JSON can still carry the wrong shape (see the map-elt call sites that
assume specific keys)."
  (with-current-buffer (get-buffer-create denote-notion--debug-buffer-name)
    (goto-char (point-max))
    (insert (format "\n--- ntn %s ---\n" (string-join args " ")))
    (insert "-- stdout --\n" stdout)
    (insert "-- stderr --\n" stderr)))

(defun denote-notion--report-dangling-links (dangling-links)
  "Report DANGLING-LINKS (see `denote-notion--rewrite-denote-links').
No-op if DANGLING-LINKS is nil.  Otherwise, messages a one-line count
and appends full detail to `denote-notion--debug-buffer-name'."
  (when dangling-links
    (message "Push: %d denote: link(s) did not resolve to a Notion page; see %s"
             (length dangling-links) denote-notion--debug-buffer-name)
    (with-current-buffer (get-buffer-create denote-notion--debug-buffer-name)
      (goto-char (point-max))
      (insert "\n--- dangling links ---\n")
      (dolist (entry dangling-links)
        (pcase-let ((`(,desc ,id ,reason ,source-file) entry))
          (insert (format "  %S (%s) — %s, from %s\n" desc id reason source-file)))))))

(defun denote-notion--run-json (args)
  "Run \"npx ntn\" with ARGS and return the parsed JSON result.
Appends \"--json\" unless ARGS invokes the `api' subcommand — `ntn api'
always emits JSON and has no `--json' flag at all (unlike `pages
get'/`create'/`edit'), so appending it there is an unrecognized
argument, not a no-op: `ntn api's variadic trailing [INPUT]... catch-all
means a bare trailing \"--json\" is rejected outright rather than
silently absorbed. Returns nil if the process exits non-zero. Signals
a `user-error' with the CLI's own output when the exit code is
non-zero."
  (let* ((args (if (equal (car args) "api") args (append args '("--json"))))
         (result (denote-notion--run args))
         (exit-code (nth 0 result))
         (stdout (nth 1 result))
         (stderr (nth 2 result)))
    (denote-notion--debug-log args stdout stderr)
    (unless (zerop exit-code)
      (user-error "ntn %s failed: %s" (string-join args " ")
                  (if (string-empty-p stderr) stdout stderr)))
    (json-parse-string stdout :object-type 'alist :array-type 'list)))

(defun denote-notion--run-async (args callback)
  "Async sibling of `denote-notion--run'; run \"npx ntn\" ARGS via `make-process'.
Keeps the same stdout/stderr separation `denote-notion--run' relies on
\(see that function's docstring\), but via `make-process''s `:stderr'
keyword -- given a buffer rather than nil, it spins up a second,
associated pipe process so stderr never lands in the stdout buffer --
since `make-process' has no tempfile destination option the way
`call-process' does.

CALLBACK is invoked, once the process exits, as
\(CALLBACK EXIT-CODE STDOUT STDERR\), the same three values
`denote-notion--run' returns as a list."
  (let* ((stdout-buffer (generate-new-buffer " *denote-notion-ntn-stdout*"))
         (stderr-buffer (generate-new-buffer " *denote-notion-ntn-stderr*")))
    (make-process
     :name "denote-notion-ntn"
     :buffer stdout-buffer
     :stderr stderr-buffer
     :command (append (list "npx" "ntn") args)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let ((exit-code (process-exit-status proc))
               (stdout (with-current-buffer stdout-buffer (buffer-string)))
               (stderr (with-current-buffer stderr-buffer (buffer-string))))
           (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
           (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))
           (funcall callback exit-code stdout stderr)))))))

(defun denote-notion--run-json-async (args callback)
  "Async sibling of `denote-notion--run-json'; parse \"npx ntn\" ARGS's JSON.
Unlike `denote-notion--run-json', never signals a `user-error' on a
non-zero exit -- a process sentinel has no synchronous caller to
propagate a signal to; Emacs would just log it via its own \"error in
process sentinel\" handler and move on.  Instead CALLBACK is invoked as
\(CALLBACK ERROR RESULT\): ERROR is nil and RESULT holds the parsed JSON
alist on success; on a non-zero exit, ERROR is the same failure message
`denote-notion--run-json' would have raised, and RESULT is nil."
  (let ((args (if (equal (car args) "api") args (append args '("--json")))))
    (denote-notion--run-async
     args
     (lambda (exit-code stdout stderr)
       (denote-notion--debug-log args stdout stderr)
       (if (zerop exit-code)
           (funcall callback nil (json-parse-string stdout :object-type 'alist :array-type 'list))
         (funcall callback
                  (format "ntn %s failed: %s" (string-join args " ")
                          (if (string-empty-p stderr) stdout stderr))
                  nil))))))

;;; Front matter get/set

(defun denote-notion--frontmatter-line-regexp (key)
  "Return a regexp matching a front-matter KEY:VALUE line."
  (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote key)))

(defun denote-notion--frontmatter-get (file key)
  "Return the string value of front-matter KEY in FILE, or nil if absent."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward (denote-notion--frontmatter-line-regexp key) nil t)
      (string-trim (match-string 1)))))

(defun denote-notion--frontmatter-format-value (value)
  "Format VALUE the way the existing Notion-tracked notes do.
A list of strings becomes a JSON-array-like [\"a\", \"b\"]; anything else
is written as a double-quoted string."
  (if (listp value)
      (concat "[" (string-join (seq-map (lambda (v) (format "%S" v)) value) ", ") "]")
    (format "%S" value)))

(defun denote-notion--frontmatter-set (file key value)
  "Set front-matter KEY to VALUE in FILE, replacing or appending the line.
VALUE is formatted with `denote-notion--frontmatter-format-value'.  FILE
must already be visited or is visited (and saved) as part of this call."
  (let ((line (format "%-15s %s" (concat key ":") (denote-notion--frontmatter-format-value value))))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (if (re-search-forward (denote-notion--frontmatter-line-regexp key) nil t)
          (replace-match line)
        (goto-char (point-min))
        (forward-line 1)
        (insert line "\n"))
      (save-buffer))))

(defun denote-notion--tracked-p (file)
  "Return non-nil if FILE has a non-empty notion_id front-matter value."
  (let ((id (denote-notion--frontmatter-get file "notion_id")))
    (and id (not (string-empty-p id)) (not (string= id "\"\"")))))

(defun denote-notion--conflicted-p (file)
  "Return non-nil if FILE is marked `notion_conflict'.
See `denote-notion--export-update', which sets this flag.  Unlike
`denote-notion--tracked-p', the truthy value is the bare symbol `t'
written literally, not a quoted string -- so a present-but-cleared
value (the empty string left by
`denote-notion--finish-conflict-resolution') reads as unset, not just
\"absent\"."
  (let ((value (denote-notion--frontmatter-get file "notion_conflict")))
    (and value (string= value "t"))))

;;; Body extraction and conversion

(defun denote-notion--body-without-front-matter (file)
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

(defun denote-notion--org-to-markdown (org-body)
  "Convert ORG-BODY (a string of Org markup) to Markdown via `ox-gfm'.
`ox-gfm' (rather than plain `ox-md') is used so that Org tables export
as GFM pipe tables and src blocks export as fenced \"```\" blocks --
`ox-md' has no Markdown table transcoder at all and always falls back
to raw HTML tables, and renders src blocks as 4-space-indented blocks
instead of fenced ones, neither of which round-trips cleanly through
Notion.
Shadows `denote-link-ol-export' for the dynamic extent of the export
call only, so an Org-source `denote:' link exports to the same
intermediate Markdown shape a Markdown source has natively --
`[desc](denote:ID)' -- instead of Org's built-in behavior of baking in
the target's absolute local filesystem path.  `cl-letf' is used rather
than `org-link-set-parameters' since it restores the function cell
automatically, even on a non-local exit, without mutating the global
link-type registry.  The export's `:with-toc' option is disabled so a
spurious \"Table of Contents\" heading is never injected into the
exported body."
  (with-temp-buffer
    (insert org-body)
    (org-mode)
    (let ((md-buffer
           (cl-letf (((symbol-function 'denote-link-ol-export)
                      (lambda (link description _format)
                        (pcase-let ((`(,_path ,query ,_search)
                                     (denote-link--ol-resolve-link-to-target link :full-data)))
                          (format "[%s](denote:%s)" description query)))))
             (org-export-to-buffer 'gfm (generate-new-buffer-name "*denote-notion-md*")
                                    nil nil nil nil '(:with-toc nil)))))
      (unwind-protect
          (with-current-buffer md-buffer
            (string-trim (buffer-string)))
        (kill-buffer md-buffer)))))

(defun denote-notion--export-body (file)
  "Return a cons (CONTENT . DANGLING-LINKS) for FILE's exportable body.
The body is converted to Markdown first if FILE is an Org note, then
run through `denote-notion--rewrite-denote-links' to resolve any
`denote:' links to Notion page URLs."
  (let* ((body (denote-notion--body-without-front-matter file))
         (converted (if (eq (denote-filetype-heuristics file) 'org)
                        (denote-notion--org-to-markdown body)
                      body)))
    (denote-notion--rewrite-denote-links converted file)))

(defvar denote-notion--auto-push-in-flight nil
  "Hash table of Denote identifiers mid-push in the current call chain, or nil.
Bound (to a fresh `:test \\='equal' hash table, unless already bound) by
`denote-notion-push', and consulted by `denote-notion--auto-push-dependency'
to detect and break a `denote:' link cycle when
`denote-notion-export-auto-push-linked-notes' is non-nil -- without this,
note A linking to note B linking back to note A would recurse forever.")

(defun denote-notion--format-notion-link (desc target)
  "Return a Markdown link with DESC as its text, to TARGET's Notion page.
TARGET must already be Notion-tracked (see `denote-notion--tracked-p') --
its `notion_id' front-matter value is read and any hyphens stripped to
build the `https://www.notion.so/...' URL."
  (let* ((notion-id (string-trim (denote-notion--frontmatter-get target "notion_id") "\"" "\""))
         (clean-id (string-replace "-" "" notion-id)))
    (format "[%s](https://www.notion.so/%s)" desc clean-id)))

(defun denote-notion--auto-push-dependency (id target-file)
  "Push TARGET-FILE (Denote identifier ID) if untracked, breaking link cycles.
Returns non-nil once TARGET-FILE is Notion-tracked -- either it already
was, or this call just pushed it via `denote-notion-push' (recursively).
Returns nil instead of recursing when ID is already recorded in
`denote-notion--auto-push-in-flight', i.e. TARGET-FILE's own push is
already in progress somewhere higher up the current call chain (a link
cycle) -- the caller falls back to plain text with reason
`cycle-detected' in that case.  Any failure inside the recursive
`denote-notion-push' call (an `ntn' error, an unanswered parent prompt)
propagates normally and aborts the whole outer push, exactly as it would
for a top-level push -- a dependency push failing is treated as the
whole export failing."
  (cond
   ((denote-notion--tracked-p target-file) t)
   ((and denote-notion--auto-push-in-flight
         (gethash id denote-notion--auto-push-in-flight))
    nil)
   (t
    (when denote-notion--auto-push-in-flight
      (puthash id t denote-notion--auto-push-in-flight))
    (denote-notion-push target-file)
    t)))

(defun denote-notion--rewrite-denote-links (body source-file)
  "Rewrite `denote:' Markdown links in BODY to Notion page URLs.
Scans BODY for every match of `denote-md-link-in-context-regexp'
\(the Markdown form `[desc](denote:ID)', shared with Org sources once
`denote-notion--org-to-markdown' has run -- see the Org-shadow task).
For each match:
- If `(denote-get-path-by-id id)' returns a tracked file (see
  `denote-notion--tracked-p'), rewrite the match to
  `[desc](https://www.notion.so/ID-WITHOUT-DASHES)', where ID-WITHOUT-DASHES
  is the file's `notion_id' front-matter value with any hyphens stripped.
- If the file exists but is untracked, and
  `denote-notion-export-auto-push-linked-notes' is non-nil, the target is
  pushed first (see `denote-notion--auto-push-dependency'); on success the
  link is rewritten the same as an already-tracked target.  If that push
  would recurse into a link cycle instead, or if the option is nil, the
  match falls back to plain text as below, with reason `cycle-detected'
  in the cycle case.
- Otherwise (no file for the id, file exists but untracked with the option
  off, or a cycle was detected), rewrite the match to just DESC (plain
  text, link syntax stripped), and push `(desc id reason source-file)'
  onto an accumulator, REASON one of the symbols `not-yet-pushed' (file
  exists, untracked), `missing-file' (no file for that id), or
  `cycle-detected' (auto-push declined to recurse into a link cycle).
Returns a cons `(REWRITTEN-BODY . DANGLING-LINKS)', DANGLING-LINKS a list
in the order encountered."
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
                        (or (denote-notion--tracked-p target)
                            (and denote-notion-export-auto-push-linked-notes
                                 (denote-notion--auto-push-dependency id target))))
                   (denote-notion--format-notion-link desc target))
                  (t
                   (push (list desc id
                               (cond
                                ((not target) 'missing-file)
                                (denote-notion-export-auto-push-linked-notes 'cycle-detected)
                                (t 'not-yet-pushed))
                               source-file)
                         dangling)
                   desc)))))
           body)))
    (cons rewritten (nreverse dangling))))

;;; Change detection: content cache and sync-state classification

(defun denote-notion--cache-file-for (notion-id)
  "Return the absolute path of NOTION-ID's cache file.
Creates `denote-notion-cache-directory' on demand if missing, so the
first write (or even a probing read) never has to worry about an
absent parent directory.  The file is named after NOTION-ID verbatim,
with no extension -- a Notion page id is already a bare hex/dash
string safe to use directly as a filename, and only
`denote-notion--cache-read'/`denote-notion--cache-write' ever open it."
  (unless (file-directory-p denote-notion-cache-directory)
    (make-directory denote-notion-cache-directory t))
  (expand-file-name notion-id denote-notion-cache-directory))

(defun denote-notion--cache-read (notion-id)
  "Return NOTION-ID's cached last-synced Markdown body, or nil if none yet.
A missing cache file is not an error -- callers (e.g.
`denote-notion--build-conflict-buffers') need \"no ancestor exists yet\"
distinguished from \"an ancestor exists but is empty\"; signaling here
would make that distinction impossible to draw at the call site."
  (let ((cache-file (denote-notion--cache-file-for notion-id)))
    (when (file-exists-p cache-file)
      (with-temp-buffer
        (insert-file-contents cache-file)
        (buffer-string)))))

(defun denote-notion--cache-write (notion-id content)
  "Write CONTENT as NOTION-ID's cached last-synced Markdown body.
Unconditionally overwrites any cache file already on disk for NOTION-ID
-- the cache only ever needs to remember the single most recent
exchange with Notion, not a history of every past one."
  (with-temp-file (denote-notion--cache-file-for notion-id)
    (insert content)))

(defun denote-notion--content-hash (content)
  "Return a content hash of CONTENT, for sync-state change detection.
Thin wrapper around `secure-hash' (SHA-1), so call sites read as \"the
content hash\" and the algorithm is swappable in this one place."
  (secure-hash 'sha1 content))

(defun denote-notion--classify-sync-state (local-changed-p remote-changed-p)
  "Pure classification rule shared by `denote-notion--sync-state' and its
async sibling.  Given LOCAL-CHANGED-P and REMOTE-CHANGED-P, each already
computed by the caller, returns the corresponding
`unchanged'/`local-only'/`remote-only'/`both-changed' symbol -- factored
out so the two callers never state these four rules twice."
  (cond
   ((and local-changed-p remote-changed-p) 'both-changed)
   (local-changed-p 'local-only)
   (remote-changed-p 'remote-only)
   (t 'unchanged)))

(defun denote-notion--sync-state (file)
  "Classify FILE's sync state relative to its already-tracked Notion page.
FILE must be Notion-tracked (see `denote-notion--tracked-p'); signals a
`user-error' otherwise -- this is a programming-error guard, not a
user-facing check, since every caller is expected to have already
filtered to tracked files before reaching here.

Returns one of the symbols:
- `unchanged' -- neither side has moved since the last recorded sync.
- `local-only' -- FILE's own exportable body has changed (its content
  hash no longer matches the stored `notion_sync_hash'), but the
  remote page has not.
- `remote-only' -- the remote page's `last_edited_time' has advanced
  past the stored `notion_edited', but FILE's local body is unchanged.
- `both-changed' -- both sides have moved -- the conflict a push or
  pull must not silently resolve one way or the other.

Local change is detected by content hash, not by a file modification
timestamp -- FILE's own Denote front matter (tags, signature, any other
metadata) changes far more often than its actual exportable body does,
so an mtime check would false-positive on nearly every edit.  A missing
or empty stored `notion_sync_hash' (a note that predates this field, or
one whose last sync somehow never recorded it) is treated as
local-changed -- the conservative default for \"nothing has been
recorded as synced yet\".

Remote change is detected from a single `pages get' call's
`last_edited_time' alone, compared against the stored `notion_edited'
the same way `denote-notion--export-update' always has (`string>' on
the raw ISO-8601 timestamps means newer, since they sort lexically).
This function makes exactly one remote request in total, so
`denote-notion-sync-all' can call it once per tracked note without
multiplying network calls across a whole directory.  The remote page's
Markdown body is never fetched or hashed: an advanced timestamp is
simply treated as remote-changed, without distinguishing a genuine
content edit from a cosmetic property-only one -- that distinction
would require a second fetch this function is designed to avoid."
  (unless (denote-notion--tracked-p file)
    (user-error "File is not Notion-tracked: %s" file))
  (let* ((id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\""))
         (stored-hash (string-trim (or (denote-notion--frontmatter-get file "notion_sync_hash") "") "\"" "\""))
         (stored-edited (string-trim (or (denote-notion--frontmatter-get file "notion_edited") "") "\"" "\""))
         (local-hash (denote-notion--content-hash (car (denote-notion--export-body file))))
         (local-changed-p (or (string-empty-p stored-hash) (not (equal local-hash stored-hash))))
         (remote (map-elt (denote-notion--run-json (list "pages" "get" id)) 'page))
         (remote-edited (map-elt remote 'last_edited_time))
         (remote-changed-p (and remote-edited stored-edited
                                 (not (string-empty-p stored-edited))
                                 (string> remote-edited stored-edited))))
    (denote-notion--classify-sync-state local-changed-p remote-changed-p)))

(defun denote-notion--sync-state-async (file callback)
  "Async sibling of `denote-notion--sync-state'; see it for the full rules.
Only the one remote `pages get' call is async here -- FILE's local
content hash is a fast, purely local computation and still runs
synchronously up front.  Needed so `denote-notion-sync-all''s
concurrent pool can classify every tracked note without blocking on
each one's remote call in turn, which would reintroduce the serial-
blocking problem batch sync exists to avoid.

CALLBACK is invoked as \(CALLBACK ERROR STATE\): ERROR non-nil (and STATE
nil\) on an `ntn' failure, mirroring `denote-notion--run-json-async';
otherwise ERROR is nil and STATE is the classification symbol."
  (unless (denote-notion--tracked-p file)
    (user-error "File is not Notion-tracked: %s" file))
  (let* ((id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\""))
         (stored-hash (string-trim (or (denote-notion--frontmatter-get file "notion_sync_hash") "") "\"" "\""))
         (stored-edited (string-trim (or (denote-notion--frontmatter-get file "notion_edited") "") "\"" "\""))
         (local-hash (denote-notion--content-hash (car (denote-notion--export-body file))))
         (local-changed-p (or (string-empty-p stored-hash) (not (equal local-hash stored-hash)))))
    (denote-notion--run-json-async
     (list "pages" "get" id)
     (lambda (error result)
       (if error
           (funcall callback error nil)
         (let* ((remote (map-elt result 'page))
                (remote-edited (map-elt remote 'last_edited_time))
                (remote-changed-p (and remote-edited stored-edited
                                        (not (string-empty-p stored-edited))
                                        (string> remote-edited stored-edited))))
           (funcall callback nil (denote-notion--classify-sync-state local-changed-p remote-changed-p))))))))

(defun denote-notion--record-synced-content (file notion-id content)
  "Record CONTENT as the body last exchanged with FILE's Notion page NOTION-ID.
Writes FILE's `notion_sync_hash' front-matter field (see
`denote-notion--content-hash') and CONTENT itself to NOTION-ID's
on-disk cache (see `denote-notion--cache-write'), together, so a later
`denote-notion--sync-state' call can tell FILE's body hasn't moved
since this exchange, and `denote-notion-resolve-conflict''s three-way
merge has an ancestor to diff against.  Kept as one function, called
identically from `denote-notion--export-create',
`denote-notion--export-update', and `denote-notion--import-refresh-file',
so the two writes can't drift apart across call sites."
  (denote-notion--frontmatter-set file "notion_sync_hash" (denote-notion--content-hash content))
  (denote-notion--cache-write notion-id content))

;;; Export

(defun denote-notion--read-parent ()
  "Prompt for a Notion parent, honoring `denote-notion-default-parent'.
Returns a (TYPE . ID) cons.  Priority: `denote-notion-default-parent',
then an ACR pick from `denote-notion-parent-registry' if non-empty,
then a raw type+id prompt."
  (or denote-notion-default-parent
      (if denote-notion-parent-registry
          (denote-notion--acr-select-parent)
        (let ((type (intern (completing-read "Notion parent type: "
                                              '("page" "database" "data-source") nil t)))
              (id (read-string "Notion parent id: ")))
          (cons type id)))))

(defun denote-notion--export-title (file)
  "Return FILE's Denote title, or nil if it has none."
  (let ((title (denote-retrieve-front-matter-title-value
                file (denote-filetype-heuristics file))))
    (and title (not (string-empty-p title)) title)))

(defun denote-notion--rich-text-value (text)
  "Return a Notion rich_text property value array for the plain string TEXT."
  (vector (list (cons 'text (list (cons 'content text))))))

(defun denote-notion--set-page-title (id properties title)
  "PATCH page ID's title property (found via PROPERTIES) to TITLE.
Every Notion page has exactly one property of type `title'; its name
varies by schema (\"Name\" is common; a bare page parent's is literally
\"title\"), so the key is discovered from PROPERTIES rather than assumed.
Does nothing if TITLE is nil or PROPERTIES has no `title'-typed entry."
  (when-let* ((title (and title (not (string-empty-p title)) title))
              (key (car (seq-find (lambda (kv) (equal (map-elt (cdr kv) 'type) "title"))
                                   properties))))
    (denote-notion--apply-properties
     id (list (cons key (list (cons 'title (denote-notion--rich-text-value title))))))))

(defun denote-notion--set-tags-from-properties (file properties)
  "Set FILE's notion_tags from Notion page PROPERTIES' Tags, if any."
  (when-let* ((tags-prop (map-elt properties 'Tags))
              (multi-select (map-elt tags-prop 'multi_select))
              (names (seq-map (lambda (tag) (map-elt tag 'name)) multi-select)))
    (denote-notion--frontmatter-set file "notion_tags" names)))

;;; Custom Notion property values

(defconst denote-notion--property-sentinels
  `(("<today>" . ,(lambda () (format-time-string "%Y-%m-%d"))))
  "Alist of (SENTINEL . NILADIC-FN) recognized in Notion property values.
Any leaf string exactly matching SENTINEL — in a note's `notion_properties'
front-matter value, or in a `denote-notion-parent-registry' entry's default
PROPERTIES — is replaced by calling NILADIC-FN.")

(defun denote-notion--merge-properties (&rest alists)
  "Merge ALISTS of Notion property values; earlier alists' keys win."
  (let (result seen)
    (dolist (alist alists)
      (dolist (kv alist)
        (unless (member (car kv) seen)
          (push (car kv) seen)
          (push kv result))))
    (nreverse result)))

(defun denote-notion--resolve-property-sentinels (value)
  "Recursively replace sentinel strings in VALUE.
See `denote-notion--property-sentinels'."
  (cond
   ((stringp value)
    (if-let* ((fn (cdr (assoc value denote-notion--property-sentinels))))
        (funcall fn)
      value))
   ((and (consp value) (consp (car value)))
    (mapcar (lambda (kv) (cons (car kv) (denote-notion--resolve-property-sentinels (cdr kv)))) value))
   ((listp value)
    (mapcar #'denote-notion--resolve-property-sentinels value))
   (t value)))

(defconst denote-notion--default-export-properties
  '((Timestamp (date (start . "<today>"))))
  "Built-in default Notion property values applied to every newly created
page, before any `denote-notion-parent-registry' entry's or file's own
override — see `denote-notion--merge-properties' precedence in
`denote-notion--export-create'.  Several Casap Notion data sources expect
a `Timestamp' date property to be populated on creation; this ensures
that happens even when no registry entry or per-file `notion_properties'
has been configured to do it explicitly.  Sentinels (e.g. \"<today>\")
are resolved the same as any other property value — see
`denote-notion--apply-properties'.")

(defun denote-notion--export-properties (file)
  "Return FILE's `notion_properties' front-matter value, parsed but unresolved.
The value is a raw JSON object mapping Notion property names to Notion API
property-value objects, e.g. {\"Timestamp\": {\"date\": {\"start\": \"<today>\"}}}.
Sentinels (see `denote-notion--property-sentinels') are left unresolved
here — `denote-notion--apply-properties' resolves them once, uniformly,
regardless of which layer (file, registry entry, or built-in default)
contributed the value.  Returns nil if FILE has no `notion_properties'
line."
  (when-let* ((raw (denote-notion--frontmatter-get file "notion_properties"))
              (raw (and (not (string-empty-p raw)) raw)))
    (json-parse-string raw :object-type 'alist :array-type 'list)))

(defun denote-notion--apply-properties (id properties)
  "PATCH Notion page ID's PROPERTIES (an alist ready for JSON serialization).
Sentinels in PROPERTIES (see `denote-notion--property-sentinels') are
resolved here, just before sending — the single point every caller's
merged properties pass through, whether they came from a note's own
`notion_properties', a `denote-notion-parent-registry' entry's default,
or `denote-notion--default-export-properties'.  Resolving earlier, per
layer, would miss whichever layers didn't happen to call it."
  (when properties
    (denote-notion--run-json
     (list "api" (format "v1/pages/%s" id)
           "--data" (json-serialize
                     (list (cons 'properties (denote-notion--resolve-property-sentinels properties))))
           "-X" "PATCH"))))

(defun denote-notion--parse-parent-arg (string)
  "Parse STRING (as formatted by `denote-notion--parent-arg') back to a cons.
Returns (TYPE . ID) with TYPE interned as a symbol, or nil if STRING is
nil/empty."
  (when (and string (not (string-empty-p string)))
    (when-let* ((pos (string-search ":" string)))
      (cons (intern (substring string 0 pos)) (substring string (1+ pos))))))

(defun denote-notion--export-create (file parent)
  "Create a new Notion page for FILE under PARENT; write back tracking fields.
PARENT is a (TYPE . ID) cons; see `denote-notion-default-parent'.  The
`type:id' form of PARENT itself (not any `denote-notion-parent-registry'
entry's name) is recorded as `notion_parent', so a later
`denote-notion--export-update' resolves default properties by looking the
same locator back up in the registry (see
`denote-notion--registry-entry-for-parent') — storing the registry name
instead would bitrot the moment that name is renamed or removed from
config, since the note itself has no other record of which parent it
was actually created under.  If PARENT matches a registry entry, that
entry's default properties are merged under FILE's own
`notion_properties', which in turn take precedence over
`denote-notion--default-export-properties' (e.g. `Timestamp') — so the
built-in default always populates a value on creation unless something
more specific already provides one.

`ntn pages create --json' returns the created page object directly at
its top level (unlike `ntn pages get --json', which wraps it under a
`page' key alongside the converted markdown) — RESULT below is used as
the page object as-is.

Returns a cons (URL . DANGLING-LINKS); see `denote-notion--export-body'."
  (let ((registry-entry (denote-notion--registry-entry-for-parent parent)))
    (pcase-let ((`(,content . ,dangling) (denote-notion--export-body file)))
      (let ((page (denote-notion--run-json
                   (list "pages" "create" "--parent" (denote-notion--parent-arg parent)
                         "--content" content))))
        (denote-notion--frontmatter-set file "notion_id" (map-elt page 'id))
        (denote-notion--frontmatter-set file "notion_created" (map-elt page 'created_time))
        (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
        (denote-notion--frontmatter-set file "notion_parent" (denote-notion--parent-arg parent))
        (denote-notion--record-synced-content file (map-elt page 'id) content)
        (denote-notion--set-tags-from-properties file (map-elt page 'properties))
        (denote-notion--set-page-title (map-elt page 'id) (map-elt page 'properties)
                                        (denote-notion--export-title file))
        (denote-notion--apply-properties
         (map-elt page 'id)
         (denote-notion--merge-properties (denote-notion--export-properties file)
                                           (cdr (cdr registry-entry))
                                           denote-notion--default-export-properties))
        (cons (map-elt page 'url) dangling)))))

(defun denote-notion--export-apply-pushed-page (file id content dangling page)
  "Apply post-edit bookkeeping to FILE/ID once CONTENT has been pushed to PAGE.
PAGE is the full page object re-fetched via `pages get' after a `pages
edit' call -- `ntn pages edit --json' itself returns only a minimal
confirmation object, with neither `url' nor `properties', so both
`denote-notion--export-update''s FORCE path and its async sibling
`denote-notion--export-push-async' re-fetch PAGE before calling this.
Records FILE's `notion_edited' and synced-content cache entry (see
`denote-notion--record-synced-content'), sets the discovered title
property, and PATCHes any configured Notion properties, shared so
neither caller duplicates those four steps -- mirroring how
`denote-notion--import-apply-refresh' factors the equivalent steps on
the import side.
Returns a cons (URL . DANGLING-LINKS); DANGLING-LINKS is passed through
unchanged from CONTENT's own `denote-notion--export-body' call."
  (let* ((stored-parent (string-trim (or (denote-notion--frontmatter-get file "notion_parent") "") "\"" "\""))
         (registry-entry (denote-notion--registry-entry-for-parent
                           (denote-notion--parse-parent-arg stored-parent)))
         (default-properties (cdr (cdr registry-entry))))
    (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
    (denote-notion--record-synced-content file id content)
    (denote-notion--set-page-title id (map-elt page 'properties) (denote-notion--export-title file))
    (denote-notion--apply-properties
     id (denote-notion--merge-properties (denote-notion--export-properties file) default-properties))
    (cons (map-elt page 'url) dangling)))

(defun denote-notion--export-update (file force)
  "Update the Notion page already tracked by FILE, or signal a conflict.
With FORCE non-nil, overwrite the remote page unconditionally, skipping
`denote-notion--sync-state' (and its remote fetch) entirely.

Without FORCE, `denote-notion--sync-state' classifies FILE first:
- `unchanged' -- the network edit is skipped outright (no `ntn pages
  edit' call at all), and a message reports the file is already in
  sync; the previously-fetched URL (reconstructed from NOTION_ID,
  not re-fetched) is returned as if a push had happened, so callers
  like `denote-notion-push' can report it uniformly either way.
- `both-changed' -- rather than erroring, FILE's `notion_conflict'
  front-matter flag is set to `t' (see `denote-notion--conflicted-p')
  and the network edit is skipped outright, exactly like `unchanged'
  above -- the caller is not blocked, just told (via the returned URL
  and a message) that nothing was pushed and that
  `denote-notion-resolve-conflict' is how to reconcile the two sides
  by hand.
- `local-only' or `remote-only' -- the update proceeds normally; a
  `remote-only' push is not protected from clobbering content that
  changed on the Notion side alone.

Unlike `ntn pages create --json' (a flat page object with `url',
`properties', `last_edited_time', etc.), `ntn pages edit --json' returns
only a minimal confirmation object — id/markdown/object/request_id/
truncated/unknown_block_ids, no `url' or `properties' — so the FORCE
branch re-fetches the full page object via `pages get' and hands it to
`denote-notion--export-apply-pushed-page' rather than reading it from
the edit response.

Returns a cons (URL . DANGLING-LINKS); see `denote-notion--export-body'."
  (let* ((id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\""))
         (state (unless force (denote-notion--sync-state file))))
    (when (eq state 'both-changed)
      (denote-notion--frontmatter-set file "notion_conflict" t)
      (message
       "Notion page %s changed since the last sync (remote and local both changed); marked notion_conflict -- run `denote-notion-resolve-conflict' to reconcile"
       id))
    (if (memq state '(unchanged both-changed))
        (progn
          (unless (eq state 'both-changed)
            (message "Notion page %s already in sync; nothing to push" id))
          (cons (format "https://www.notion.so/%s" (string-replace "-" "" id)) nil))
      (pcase-let ((`(,content . ,dangling) (denote-notion--export-body file)))
        (denote-notion--run-json (list "pages" "edit" id "--content" content))
        (let ((page (map-elt (denote-notion--run-json (list "pages" "get" id)) 'page)))
          (denote-notion--export-apply-pushed-page file id content dangling page))))))

(defun denote-notion--export-push-async (file callback)
  "Async, state-already-known push of FILE's content to its tracked page.
The async counterpart to `denote-notion--export-update' called with
FORCE non-nil -- it never computes or re-checks sync-state itself.
Built for `denote-notion-sync-all', which has already classified FILE
as `local-only' via one async `pages get' call
\(`denote-notion--sync-state-async'\); re-deriving sync-state here too
would mean a second, redundant remote fetch per `local-only' note in a
batch run.

Mirrors `denote-notion--export-update''s FORCE branch: edit the page's
content, re-fetch the page (`ntn pages edit --json' returns neither
`url' nor `properties'), then apply it via the same
`denote-notion--export-apply-pushed-page' helper, including its final
properties PATCH -- left synchronous even here since it is a small,
independent call, not the per-note round-trip this function exists to
make concurrent.

CALLBACK is invoked as \(CALLBACK ERROR URL-AND-DANGLING\), mirroring
`denote-notion--run-json-async''s shape; URL-AND-DANGLING is the same
\(URL . DANGLING-LINKS\) cons `denote-notion--export-update' returns, nil
on error."
  (let ((id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\"")))
    (pcase-let ((`(,content . ,dangling) (denote-notion--export-body file)))
      (denote-notion--run-json-async
       (list "pages" "edit" id "--content" content)
       (lambda (error _result)
         (if error
             (funcall callback error nil)
           (denote-notion--run-json-async
            (list "pages" "get" id)
            (lambda (error2 result2)
              (if error2
                  (funcall callback error2 nil)
                (funcall callback nil
                         (denote-notion--export-apply-pushed-page
                          file id content dangling (map-elt result2 'page))))))))))))

;;;###autoload
(defun denote-notion-push (&optional file parent force)
  "Push FILE (default the current buffer's file) to Notion as a page.
If FILE is not yet Notion-tracked, PARENT — a (TYPE . ID) cons, where TYPE
is one of the symbols `page', `database', or `data-source' — is required:
prompted interactively unless `denote-notion-default-parent' is set — and
a new page is created.  If FILE is already tracked, its Notion page is
updated, unless the remote page has changed since the last sync, in which
case a conflict is signaled; pass FORCE (or the prefix argument,
interactively) to overwrite anyway.  Also reports (see
`denote-notion--report-dangling-links') any `denote:' links in FILE
that failed to resolve to a Notion page."
  (interactive (list nil nil current-prefix-arg))
  (let ((denote-notion--auto-push-in-flight
         (or denote-notion--auto-push-in-flight (make-hash-table :test 'equal))))
    (let ((file (or file (denote-notion--file-at-point) (user-error "No file to export"))))
      (puthash (denote-retrieve-filename-identifier file) t denote-notion--auto-push-in-flight)
      (pcase-let ((`(,url . ,dangling)
                   (if (denote-notion--tracked-p file)
                       (denote-notion--export-update file force)
                     (denote-notion--export-create file (or parent (denote-notion--read-parent))))))
        (message "Exported to %s" url)
        (denote-notion--report-dangling-links dangling)))))

;;; Conflict resolution

(defun denote-notion--finish-conflict-resolution (file merged-content)
  "Write MERGED-CONTENT back into FILE, clear its conflict flag, and force-push.
Called with the finished merge buffer's text once a human has resolved
a `notion_conflict' note's merge -- see `denote-notion-resolve-conflict'.

Writes MERGED-CONTENT as FILE's body via `denote-notion--import-write-body',
then clears `notion_conflict' to the empty string rather than removing
the front-matter line -- `denote-notion--frontmatter-set' has no
\"remove a key\" mode, and nil would format as the literal, meaningless
text \"nil\" (`denote-notion--frontmatter-format-value' applies `%S' to
any non-list value); an empty string formats as the unambiguous `\"\"',
the same way a cleared `notion_id' reads as untracked.

Finally pushes FILE to Notion with FORCE unconditionally true: a plain
push would recompute `denote-notion--sync-state' and, since the remote
page's `last_edited_time' has not moved since the conflict was first
detected, would likely re-classify it as `both-changed' all over
again -- the user has just reconciled both sides by hand, so forcing is
correct here, not a shortcut around the conflict check."
  (denote-notion--import-write-body file merged-content)
  (denote-notion--frontmatter-set file "notion_conflict" "")
  (denote-notion-push file nil t))

(defun denote-notion--build-conflict-buffers (file notion-id)
  "Return a plist of buffers for FILE's three-way (or two-way) conflict merge.
Keys `:local', `:remote', `:ancestor' hold freshly created buffers
populated with, respectively: FILE's current exportable body (see
`denote-notion--export-body'), NOTION-ID's current remote Markdown body
\(fetched fresh via `ntn pages get', the same nested `markdown' shape
`denote-notion--import-refresh-file' already unwraps\), and NOTION-ID's
cached last-synced body (see `denote-notion--cache-read') -- `:ancestor'
is nil, not a buffer, when no cache entry exists yet (a note tracked
before this cache existed), so the caller can tell \"use the three-way
merge\" apart from \"fall back to a two-way one\"."
  (let* ((local-content (car (denote-notion--export-body file)))
         (remote-result (denote-notion--run-json (list "pages" "get" notion-id)))
         (remote-content (denote-notion--clean-imported-body
                           (map-elt (map-elt remote-result 'markdown) 'markdown)))
         (ancestor-content (denote-notion--cache-read notion-id))
         (local-buffer (generate-new-buffer (format "*denote-notion-conflict-local-%s*" notion-id)))
         (remote-buffer (generate-new-buffer (format "*denote-notion-conflict-remote-%s*" notion-id)))
         (ancestor-buffer (and ancestor-content
                                (generate-new-buffer
                                 (format "*denote-notion-conflict-ancestor-%s*" notion-id)))))
    (with-current-buffer local-buffer (insert local-content))
    (with-current-buffer remote-buffer (insert remote-content))
    (when ancestor-buffer
      (with-current-buffer ancestor-buffer (insert ancestor-content)))
    (list :local local-buffer :remote remote-buffer :ancestor ancestor-buffer)))

;;;###autoload
(defun denote-notion-resolve-conflict (&optional file)
  "Interactively resolve FILE's `notion_conflict' via an ediff merge session.
FILE defaults to `denote-notion--file-at-point', erroring (matching
`denote-notion-push''s own style) when there is none.  FILE must already
be Notion-tracked and marked conflicted (see `denote-notion--tracked-p'
and `denote-notion--conflicted-p'); either condition missing is a
`user-error', not silently a no-op.

Builds three in-memory buffers (see
`denote-notion--build-conflict-buffers'): FILE's own current exportable
body, NOTION-ID's current remote body (fetched fresh, not from the
sync-state check that flagged the conflict), and the cached last-synced
ancestor body, if one exists.  When an ancestor exists, starts
`ediff-merge-buffers-with-ancestor' -- a genuine three-way merge that
auto-resolves every region only one side touched, dropping the user into
an interactive session only for the regions both sides touched
differently.  When no ancestor exists (a note tracked before the sync
cache existed, so no prior snapshot was recorded), falls back to a
plain two-way `ediff-buffers' comparing local against remote directly.

The merge's finish action is wired via a buffer-local addition to
`ediff-quit-hook' inside the ediff control buffer, added immediately
after the session starts -- `ediff-quit-hook' is otherwise a single
global hook list shared by every ediff session in the Emacs instance,
so a plain `add-hook' without the LOCAL argument would also fire (and
persist) across any other, unrelated ediff session.  The hook function
kills the three scratch buffers itself before ediff discards its own
control buffer, so nothing leaks past this one merge."
  (interactive)
  (let* ((file (or file (denote-notion--file-at-point) (user-error "No file to resolve")))
         (_ (unless (denote-notion--tracked-p file)
              (user-error "File is not Notion-tracked: %s" file)))
         (_ (unless (denote-notion--conflicted-p file)
              (user-error "File has no notion_conflict to resolve: %s" file)))
         (notion-id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\""))
         (buffers (denote-notion--build-conflict-buffers file notion-id))
         (local-buffer (plist-get buffers :local))
         (remote-buffer (plist-get buffers :remote))
         (ancestor-buffer (plist-get buffers :ancestor))
         (cleanup
          (lambda ()
            (dolist (buf (list local-buffer remote-buffer ancestor-buffer))
              (when (buffer-live-p buf) (kill-buffer buf)))))
         (finish
          (lambda ()
            ;; A three-way session (ancestor present) produces a dedicated
            ;; merge buffer at `ediff-buffer-C'.  The two-way fallback
            ;; (`ediff-buffers', no ancestor) has no such buffer -- there,
            ;; LOCAL-BUFFER is the one the user reconciles directly, via
            ;; ediff's ordinary copy-hunk-A-to-B/B-to-A actions, so its
            ;; final text is what gets written back.
            (let ((merged (with-current-buffer (if ancestor-buffer ediff-buffer-C local-buffer)
                            (buffer-string))))
              (denote-notion--finish-conflict-resolution file merged))
            (funcall cleanup))))
    (if ancestor-buffer
        (ediff-merge-buffers-with-ancestor
         local-buffer remote-buffer ancestor-buffer
         (list (lambda () (add-hook 'ediff-quit-hook finish nil t))))
      (ediff-buffers
       local-buffer remote-buffer
       (list (lambda () (add-hook 'ediff-quit-hook finish nil t)))))))

;;; Registry maintenance

(defun denote-notion--resolve-data-source (id-or-url)
  "Resolve ID-OR-URL to a Notion data source, returning an (ID . NAME) cons.
ID-OR-URL is a database id, a data source id, or a Notion URL for either
-- see `denote-notion--extract-page-id' for the accepted forms.  Runs
\"ntn datasources resolve\" on the extracted id: a bare data source id
resolves to itself, and a database id resolves to the data source
`denote-notion-parent-registry' actually needs to work with `ntn
pages create --parent'.  Pasting a full-page database's own URL and
registering its id directly, without this resolution step, is exactly
the mistake this function exists to catch early: that id looks like a
valid parent locator, but the Notion API only rejects it as
`object_not_found' much later, at export time, not when the registry
entry was written.  When the database holds more than one data source,
prompts via `completing-read' to pick one by name."
  (let* ((id (denote-notion--extract-page-id id-or-url))
         (result (denote-notion--run-json (list "datasources" "resolve" id)))
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
(defun denote-notion-add-parent (url &optional name)
  "Resolve URL to a Notion data source and add it to
`denote-notion-parent-registry'.
URL is a Notion database or data source URL, or a bare id -- see
`denote-notion--resolve-data-source', which does the actual resolution.
NAME is the entry's display name in the `denote-notion--acr-select-parent'
menu; when omitted it defaults to, and is interactively prompted for
with a default of, the data source's own Notion name.

Adds `(NAME . ((data-source . ID)))' to the front of the in-memory
`denote-notion-parent-registry', replacing any existing entry already
registered under NAME, and also copies that same form to the kill ring:
the registry itself is ordinarily populated by a `setq' in an init file
that this command neither reads nor writes, so the kill ring is what
carries the entry to wherever that `setq' lives for it to survive a
restart."
  (interactive "sNotion URL or id: ")
  (let* ((resolved (denote-notion--resolve-data-source url))
         (id (car resolved))
         (name (or name
                   (and (called-interactively-p 'interactive)
                        (read-string "Registry name: " (cdr resolved)))
                   (cdr resolved)))
         (entry (cons name (list (cons 'data-source id))))
         (form (format "(%S\n . ((data-source . %S)))" name id)))
    (setq denote-notion-parent-registry
          (cons entry (assoc-delete-all name (copy-sequence denote-notion-parent-registry))))
    (kill-new form)
    (message "Added %s -> data-source:%s to denote-notion-parent-registry (form copied to kill ring)" name id)))

;;; Import

(defun denote-notion--extract-page-id (id-or-url)
  "Return the 32-char hex page id embedded in ID-OR-URL.
Any query string or fragment is stripped first, e.g. the trailing
\"?v=<view-id>\" on a database URL like
\"https://app.notion.com/p/<id>?v=<view-id>\" -- otherwise the view id's
own 32 hex characters, not the database id's, would be the last thing
in the string and would be extracted instead."
  (let ((trimmed (replace-regexp-in-string "[?#].*\\'" "" id-or-url)))
    (if (string-match "\\([0-9a-fA-F]\\{32\\}\\)\\'"
                      (replace-regexp-in-string "-" "" trimmed))
        (match-string 1 (replace-regexp-in-string "-" "" trimmed))
      id-or-url)))

(defun denote-notion--clean-imported-body (body)
  "Clean up BODY as returned by `ntn pages get' for storage in a denote note.
Every separate Notion block (a paragraph, a heading, a list item, ...) is
joined to the next by a single newline in `ntn's Markdown, with no blank
line between them; a single newline is not a paragraph break in Markdown,
so without widening it every block runs into the next as one paragraph.
Fenced \"```\" code blocks are pulled out and swapped back in verbatim
around that widening, since their internal single newlines are
meaningful code line breaks rather than block joins -- widening them
too would blank-line-separate every line of code.  Each remaining
single newline is widened to a blank line, then a literal \"<br>\" tag
— Notion's *soft* line break within one block — is turned into a
single newline, so it does not also become a paragraph break.  Any
literal square bracket in prose is also backslash-escaped (\\[, \\])
per CommonMark convention, to stop it from being misread as link syntax
by a Markdown parser; a denote note is not read through one, so the
escaping only pollutes prose that never had it in Notion's own editor."
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

(defun denote-notion--rich-text-plain (rich-text-array)
  "Return the concatenated `plain_text' of RICH-TEXT-ARRAY.
RICH-TEXT-ARRAY is a Notion API rich_text array (as found in a `title' or
`rich_text' property value) -- a list of alists each carrying their own
`plain_text', not a plain string on its own."
  (mapconcat (lambda (segment) (or (map-elt segment 'plain_text) ""))
             rich-text-array ""))

(defun denote-notion--import-write-body (file body)
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

(defun denote-notion--import-apply-refresh (file page-id result)
  "Apply a `pages get' RESULT to FILE, as a refresh of its tracked PAGE-ID.
RESULT is the same shape `denote-notion--run-json' and
`denote-notion--run-json-async' return for `pages get' -- `ntn pages get
--json' nests the markdown text
under its own `markdown' object: {\"markdown\": {\"markdown\": \"...\"},
\"page\": {...}}.  Writes FILE's body, refreshes its `notion_edited' and
synced-content cache, and syncs `notion_tags' from the page's current
Tags property.  denote's own title/keywords are left alone, matching how
export treats them as user-owned once set.

Factored out of `denote-notion--import-refresh-file' so both it and its
async sibling `denote-notion--import-refresh-file-async' (used by the
batch-sync `remote-only' path) apply an already-fetched RESULT the same
way, rather than duplicating these four steps between a sync and an
async caller."
  (let* ((page (map-elt result 'page))
         (body (denote-notion--clean-imported-body
                (map-elt (map-elt result 'markdown) 'markdown))))
    (denote-notion--import-write-body file body)
    (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
    (denote-notion--record-synced-content file page-id body)
    (denote-notion--set-tags-from-properties file (map-elt page 'properties))))

(defun denote-notion--import-refresh-file (file page-id)
  "Pull PAGE-ID's current content and tracking fields into FILE.
See `denote-notion--import-apply-refresh' for what is actually applied."
  (denote-notion--import-apply-refresh
   file page-id (denote-notion--run-json (list "pages" "get" page-id))))

(defun denote-notion--import-refresh-file-async (file page-id callback)
  "Async sibling of `denote-notion--import-refresh-file'.
Built for `denote-notion-sync-all''s `remote-only' action, which has
already classified FILE via `denote-notion--sync-state-async' before
deciding to pull -- this still makes its own `pages get' call rather
than reusing that classification call's result, since
`denote-notion--sync-state' (and its async sibling) deliberately never
fetches the remote body at all, only `last_edited_time' -- see
`denote-notion--sync-state''s docstring.  CALLBACK is invoked as
\(CALLBACK ERROR\): ERROR non-nil on an `ntn' failure
\(see `denote-notion--run-json-async'\), nil on success."
  (denote-notion--run-json-async
   (list "pages" "get" page-id)
   (lambda (error result)
     (if error
         (funcall callback error)
       (denote-notion--import-apply-refresh file page-id result)
       (funcall callback nil)))))

(defun denote-notion--find-tracked-file (id)
  "Return the denote file already tracking Notion page ID, or nil.
Searches every file in `denote-directory-files', not just the current
buffer or an explicit TARGET-FILE -- otherwise importing a page id
already tracked by some other, not-currently-open note creates a
duplicate rather than refreshing the note that already exists for it."
  (seq-find (lambda (f)
              (equal (string-trim (or (denote-notion--frontmatter-get f "notion_id") "") "\"" "\"")
                     id))
            (denote-directory-files)))

;;;###autoload
(defun denote-notion-pull (&optional page-id target-file)
  "Pull a Notion page into a denote note, creating or refreshing as needed.

With PAGE-ID (an id, or a Notion URL): pull that specific page.  If
TARGET-FILE, the current buffer, or any other denote note already tracks
that page's id (see `denote-notion--find-tracked-file'), replace that
file's body and refresh its tracking fields in place.  Otherwise create a
new tracked denote note from the page's properties and body — so
re-running an import on a page you already have never creates a
duplicate note, even from a buffer other than the one already tracking it.

Without PAGE-ID: refresh TARGET-FILE (or the current buffer) using its
own already-tracked `notion_id', so \"pull this specific page\" and
\"re-pull whatever I'm already looking at\" are the same command.
Errors if there's no tracked file to fall back on.

Interactively, PAGE-ID is only prompted for when the current buffer isn't
already a tracked Notion note — otherwise this dwim-refreshes the current
buffer directly, with no prompt at all."
  (interactive
   (list (let ((file (denote-notion--file-at-point)))
           (unless (and file (denote-notion--tracked-p file))
             (read-string "Notion page id or URL: ")))))
  (let ((file (or target-file (denote-notion--file-at-point))))
    (if (not page-id)
        (progn
          (unless (and file (denote-notion--tracked-p file))
            (user-error "No file to refresh: not Notion-tracked, and no page id given"))
          (let ((id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\"")))
            (denote-notion--import-refresh-file file id)
            (message "Refreshed %s from Notion" (file-name-nondirectory file))))
      (let* ((id (denote-notion--extract-page-id page-id))
             (target (or target-file
                         (when (and file
                                    (equal (string-trim (or (denote-notion--frontmatter-get file "notion_id") "") "\"" "\"")
                                           id))
                           file)
                         (denote-notion--find-tracked-file id))))
        (if target
            (progn
              (denote-notion--import-refresh-file target id)
              (message "Refreshed %s from Notion" (file-name-nondirectory target)))
          (let* ((result (denote-notion--run-json (list "pages" "get" id)))
                 (page (map-elt result 'page))
                 (body (denote-notion--clean-imported-body
                        (map-elt (map-elt result 'markdown) 'markdown)))
                 (properties (map-elt page 'properties))
                 ;; `Name' (a `title' property) is a Notion rich_text array,
                 ;; not a plain string -- see `denote-notion--rich-text-plain'.
                 (title (denote-notion--rich-text-plain
                         (map-elt (map-elt properties 'Name) 'title)))
                 (tags (seq-map (lambda (tag) (map-elt tag 'name))
                                (map-elt (map-elt properties 'Tags) 'multi_select)))
                 ;; Backdate the new note's identifier/date to when the
                 ;; Notion page was actually created, rather than "now" --
                 ;; otherwise an import loses the page's real authorship date.
                 (created (map-elt page 'created_time)))
            ;; `markdown-yaml' matches the front matter shape already used
            ;; by every note this tool tracks.
            (denote (if (and title (not (string-empty-p title))) title "Untitled Notion import")
                    (cons "notion" tags) 'markdown-yaml nil created)
            (let ((new-file (buffer-file-name)))
              (goto-char (point-max))
              (insert "\n" body)
              (denote-notion--frontmatter-set new-file "notion_id" id)
              (denote-notion--frontmatter-set new-file "notion_tags" tags)
              (denote-notion--frontmatter-set new-file "notion_created" (map-elt page 'created_time))
              (denote-notion--frontmatter-set new-file "notion_edited" (map-elt page 'last_edited_time))
              (save-buffer)
              (message "Imported %s" (file-name-nondirectory new-file)))))))))

;;; Denote-dash view helpers

(defconst denote-notion--dash-view-tracked-name "notion: tracked"
  "Name of the saved `denote-dash-view' registered by
`denote-notion-dash-view-tracked'.")

(defconst denote-notion--dash-view-conflicts-name "notion: conflicts"
  "Name of the saved `denote-dash-view' registered by
`denote-notion-dash-view-conflicts'.")

(defconst denote-notion--dash-view-remote-updated-name "notion: remote-updated"
  "Name of the saved `denote-dash-view' registered by
`denote-notion-dash-view-remote-updated'.")

(defun denote-notion--frontmatter-nonempty-value-regexp (key)
  "Return a regexp matching a front-matter KEY line with a non-empty value.
Mirrors `denote-notion--frontmatter-line-regexp''s KEY:VALUE line shape,
but further anchored to require at least one non-quote character right
after the value's opening quote -- every value this tool itself writes is
quoted via `denote-notion--frontmatter-format-value', so an empty string
always reads back as the literal two-character \"\\\"\\\"\", with a closing
quote immediately following the opening one and nothing in between; this
regexp excludes exactly that case, the same \"non-empty\" test
`denote-notion--tracked-p' already applies via `string-empty-p' and a
literal `\\\"\\\"' comparison, rewritten here as a content regexp because a
`denote-dash' grep filter is exactly that -- a plain regexp matched
against a whole file's content via `re-search-forward', not a predicate
function call per file (see `denote-dash--file-grep-matches-p')."
  (format "^%s:[ \t]*\"[^\"]" (regexp-quote key)))

(defconst denote-notion--dash-conflict-grep-filter
  "^notion_conflict:[ \t]*t[ \t]*$"
  "Grep filter matching exactly what `denote-notion--conflicted-p' checks:
`notion_conflict' set to the bare, unquoted symbol `t' (formatted as the
literal text \"t\" by `denote-notion--frontmatter-format-value', never a
quoted string) -- not merely present, and not the empty string
`denote-notion--finish-conflict-resolution' clears it to once resolved.
Kept as a single literal constant, rather than built from
`denote-notion--frontmatter-nonempty-value-regexp' (which assumes a
quoted value), since this field's truthy value is unquoted text, a
different shape from every other tracking field this file writes.")

(defun denote-notion--dash-register-view (name grep-filter)
  "Register (or re-register, overwriting) a NAME'd `denote-dash-view'.
Built with GREP-FILTER as its only populated slot -- every other
`denote-dash-view' field (narrowed sequences, active directory, sort,
visible columns, ...) is left nil, meaning \"show everything, unfiltered
and unsorted, except for GREP-FILTER\" -- the same minimal shape all
three `denote-notion-dash-view-*' commands share.  Pushed onto
`denote-dash-saved-views', replacing any existing entry already
registered under NAME, mirroring `denote-dash-bookmark-save''s own
replace-by-name convention -- this is exactly what that command does
interactively, just built programmatically here instead, with no live
`denote-dash' buffer required to do it."
  (require 'denote-dash)
  (setq denote-dash-saved-views
        (cons (make-denote-dash-view :name name :grep-filter grep-filter)
              (seq-remove (lambda (v) (equal (denote-dash-view-name v) name))
                          denote-dash-saved-views))))

;;;###autoload
(defun denote-notion-dash-view-tracked ()
  "Open a `denote-dash' view of every Notion-tracked note.
Registers (or re-registers, overwriting) a `denote-dash-view' named
`denote-notion--dash-view-tracked-name' whose `grep-filter' matches any
file with a non-empty `notion_id' front-matter line -- the same
\"tracked\" test `denote-notion--tracked-p' applies, rewritten as a
content regexp (see `denote-notion--frontmatter-nonempty-value-regexp')
since that is all a `denote-dash' grep filter is.  Then opens the view
via `denote-dash-open-view'."
  (interactive)
  (denote-notion--dash-register-view
   denote-notion--dash-view-tracked-name
   (denote-notion--frontmatter-nonempty-value-regexp "notion_id"))
  (denote-dash-open-view denote-notion--dash-view-tracked-name))

;;;###autoload
(defun denote-notion-dash-view-conflicts ()
  "Open a `denote-dash' view of every note marked `notion_conflict'.
Registers (or re-registers, overwriting) a `denote-dash-view' named
`denote-notion--dash-view-conflicts-name' whose `grep-filter'
(`denote-notion--dash-conflict-grep-filter') matches exactly what
`denote-notion--conflicted-p' itself checks, so the view and the
predicate always agree on what \"conflicted\" means.  Then opens the
view via `denote-dash-open-view'.  Conflict resolution itself is
unrelated and unchanged -- see `denote-notion-resolve-conflict'; this
command only surfaces which notes need it."
  (interactive)
  (denote-notion--dash-register-view
   denote-notion--dash-view-conflicts-name
   denote-notion--dash-conflict-grep-filter)
  (denote-dash-open-view denote-notion--dash-view-conflicts-name))

;;; Remote-updated view: backgrounded per-note remote timestamp check

(defconst denote-notion--dash-remote-dirty-grep-filter
  "^notion_remote_dirty:[ \t]*t[ \t]*$"
  "Grep filter matching a file whose `notion_remote_dirty' marker is set.
Same unquoted-`t' shape as `denote-notion--dash-conflict-grep-filter' --
see `denote-notion--remote-dirty-p', `denote-notion--mark-remote-dirty'.")

(defun denote-notion--remote-dirty-p (file)
  "Return non-nil if FILE is marked `notion_remote_dirty'.
Mirrors `denote-notion--conflicted-p''s shape exactly (see it for the
bare-`t'-vs-empty-string convention).  `notion_remote_dirty' is a
dedicated, purely local bookkeeping field (see
`denote-notion--mark-remote-dirty'): never read by
`denote-notion--sync-state', `denote-notion-push', or
`denote-notion-pull', and never sent to Notion.  It exists solely so
`denote-notion-dash-view-remote-updated' can stay a plain grep-filter
view, structurally identical to the other two
`denote-notion-dash-view-*' commands."
  (let ((value (denote-notion--frontmatter-get file "notion_remote_dirty")))
    (and value (string= value "t"))))

(defun denote-notion--mark-remote-dirty (file dirty)
  "Set FILE's `notion_remote_dirty' marker to t if DIRTY, else clear it.
Clearing writes the empty string, not a removed line -- see
`denote-notion--finish-conflict-resolution''s identical convention for
`notion_conflict', which this field's lifecycle otherwise mirrors."
  (denote-notion--frontmatter-set file "notion_remote_dirty" (if dirty t "")))

(defun denote-notion--remote-timestamp-stale-p (stored-edited remote-edited)
  "Return non-nil if REMOTE-EDITED has advanced past STORED-EDITED.
Pure decision logic, factored out of `denote-notion--sync-state''s own
inline `remote-changed-p' computation so it can be unit-tested directly,
without a live network fetch: both arguments are raw ISO-8601 timestamp
strings, compared with `string>' because they sort lexically (exactly as
`denote-notion--sync-state' and `denote-notion--export-update' already
rely on).  A nil or empty STORED-EDITED (nothing recorded as synced yet)
or a nil REMOTE-EDITED (fetch failed to report one) is never considered
stale -- the conservative \"nothing has moved\" default, matching
`denote-notion--sync-state''s own guard."
  (and remote-edited stored-edited
       (not (string-empty-p stored-edited))
       (string> remote-edited stored-edited)))

(defun denote-notion--refresh-remote-dirty-marker-async (file on-done)
  "Asynchronously refresh FILE's `notion_remote_dirty' marker; then call ON-DONE.
Fetches FILE's Notion page once via `denote-notion--run-json-async'
\(\"pages get\"), compares the returned `last_edited_time' against FILE's
own stored `notion_edited' (see `denote-notion--remote-timestamp-stale-p'),
and writes the result via `denote-notion--mark-remote-dirty'.  ON-DONE is
called with no arguments once the marker has been updated, or immediately
if FILE has no readable `notion_id' or the fetch failed -- regardless of
outcome, so a caller refreshing a whole directory of tracked notes can
count completions uniformly without special-casing failures."
  (let* ((id (ignore-errors
               (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\"")))
         (stored-edited (string-trim (or (denote-notion--frontmatter-get file "notion_edited") "") "\"" "\"")))
    (if (not (and id (not (string-empty-p id))))
        (funcall on-done)
      (denote-notion--run-json-async
       (list "pages" "get" id)
       (lambda (error result)
         (unwind-protect
             (unless error
               (let ((remote-edited (map-elt (map-elt result 'page) 'last_edited_time)))
                 (denote-notion--mark-remote-dirty
                  file (denote-notion--remote-timestamp-stale-p stored-edited remote-edited))))
           (funcall on-done)))))))

;;;###autoload
(defun denote-notion-dash-view-remote-updated ()
  "Open a backgrounded `denote-dash' view of notes updated on Notion.
Unlike `denote-notion-dash-view-tracked'/`-conflicts', \"updated
remotely\" cannot be decided from a note's own file content alone -- it
requires comparing each tracked note's live Notion `last_edited_time'
against its own stored `notion_edited', one network fetch per tracked
note.  That fetch is never allowed to block opening the view:

1. The view is registered and opened immediately, via the same
   `denote-notion--dash-register-view' / `denote-dash-open-view' pair
   the other two `denote-notion-dash-view-*' commands use, with whatever
   `notion_remote_dirty' marker state (see
   `denote-notion--remote-dirty-p') is already on disk from some earlier
   refresh.  Stale is fine, and expected -- that is the whole point of
   \"instant open\".
2. A fresh per-note async fetch
   (`denote-notion--refresh-remote-dirty-marker-async') is then kicked
   off for every currently Notion-tracked file, each one
   updating that note's own marker field in place as its fetch
   completes, then refreshing the view's buffer (looked up by name,
   guarded with `buffer-live-p' since the user may have killed it before
   a given fetch finishes) via `denote-dash-refresh', so entries update
   in the open buffer incrementally rather than all at once.

`notion_remote_dirty' is a dedicated, purely local bookkeeping field
introduced for this view alone -- chosen over an in-memory note-id ->
bool table so this view stays structurally identical to its two
siblings (register a `denote-dash-view' with a `grep-filter', open it).
It also persists across sessions the same way `denote-dash-saved-views'
does, so a later re-open still reflects the last background refresh's
results even before a fresh one runs again."
  (interactive)
  (denote-notion--dash-register-view
   denote-notion--dash-view-remote-updated-name
   denote-notion--dash-remote-dirty-grep-filter)
  (denote-dash-open-view denote-notion--dash-view-remote-updated-name)
  (let* ((buffer-name (denote-dash--view-buffer-name denote-notion--dash-view-remote-updated-name))
         (files (seq-filter #'denote-notion--tracked-p (denote-directory-files))))
    (dolist (file files)
      (denote-notion--refresh-remote-dirty-marker-async
       file
       (lambda ()
         (when-let* ((buf (get-buffer buffer-name)))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (denote-dash-refresh)))))))))

;;; Batch sync: bounded async process-pool over every tracked note

(defun denote-notion--batch-run (generator max-concurrent process-fn on-complete)
  "Drive PROCESS-FN over GENERATOR with at most MAX-CONCURRENT items in flight.
GENERATOR is a `gen' struct (see `gen-wrap').  Items are pulled one at a
time via the public, built-in `generator.el' primitive `iter-next' on
GENERATOR's own raw iterator (`gen-iter'), wrapped in a `condition-case'
catching `iter-end-of-sequence' -- gen.el's own `gen--next'/`gen--peek'
are private implementation details for its `seq.el' methods, not a
public pull-one-at-a-time API, so this calls `iter-next' directly
instead.  Pulling lazily, one item per freed slot, rather than eagerly
listing and classifying every tracked note up front, is the whole point
of building this over a generator -- see `denote-notion-sync-all'.

PROCESS-FN is called as \(PROCESS-FN ITEM DONE-FN\) for each item pulled;
it must arrange for DONE-FN to be called -- synchronously or
asynchronously, it makes no difference here -- exactly once when ITEM's
work has finished, regardless of success or failure.  A slot freed by one
DONE-FN call is immediately refilled from GENERATOR if another item
remains, so at most MAX-CONCURRENT items are ever mid-flight at once.

ON-COMPLETE is called with no arguments exactly once, after GENERATOR is
exhausted and every dispatched item's DONE-FN has fired -- including the
degenerate case of an already-empty GENERATOR, where it fires immediately
with nothing ever dispatched."
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

(defun denote-notion--sync-all-process-note (file counts done)
  "Classify and sync FILE for `denote-notion-sync-all', recording its outcome.
COUNTS is an alist of (SYMBOL . COUNT) with keys `unchanged', `pushed',
`pulled', `conflicted', `errored' -- exactly `denote-notion-sync-all''s
five summary buckets; this increments the appropriate cell by `cl-incf'
before calling DONE (with no arguments), so the caller's summary reflects
every note's outcome regardless of which branch below handled it.

Classification runs through `denote-notion--sync-state-async' (one
async `pages get' call), not the blocking `denote-notion--sync-state' --
see that function's docstring for why.  Each of the four
classifications dispatches the corresponding action documented on
`denote-notion-sync-all', at most one more `ntn' call each:
- `unchanged' -- nothing further; recorded as `unchanged'.
- `local-only' -- pushed via `denote-notion--export-push-async' (not a
  second, redundant `denote-notion--sync-state' fetch); recorded as
  `pushed', or `errored' on an `ntn' failure.
- `remote-only' -- pulled via `denote-notion--import-refresh-file-async';
  recorded as `pulled', or `errored' on an `ntn' failure.
- `both-changed' -- FILE's `notion_conflict' flag is set directly (the
  same field `denote-notion--export-update' sets for this case), with no
  further network call and no `user-error' -- there is no minibuffer
  prompt watching a batch run for this to interrupt; recorded as
  `conflicted'.

Any synchronous error raised before an async callback is ever reached
(a malformed front-matter value, FILE mysteriously becoming untracked
mid-batch) is also caught and recorded as `errored', exactly like an
`ntn' failure -- one note's problem, of any kind, must never stop the
rest of the batch or leave DONE uncalled."
  (cl-flet ((record (key)
              (cl-incf (cdr (assq key counts)))
              (funcall done)))
    (condition-case nil
        (denote-notion--sync-state-async
         file
         (lambda (error state)
           (cond
            (error (record 'errored))
            (t
             (pcase state
               ('unchanged (record 'unchanged))
               ('local-only
                (condition-case nil
                    (denote-notion--export-push-async
                     file (lambda (error2 _result) (record (if error2 'errored 'pushed))))
                  (error (record 'errored))))
               ('remote-only
                (let ((id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\"")))
                  (condition-case nil
                      (denote-notion--import-refresh-file-async
                       file id (lambda (error3) (record (if error3 'errored 'pulled))))
                    (error (record 'errored)))))
               ('both-changed
                (denote-notion--frontmatter-set file "notion_conflict" t)
                (record 'conflicted)))))))
      (error (record 'errored)))))

;;;###autoload
(defun denote-notion-sync-all ()
  "Batch-sync every Notion-tracked note via a bounded async process pool.

Builds a lazy generator (see `gen-wrap') over every file under
`denote-directory-files' for which `denote-notion--tracked-p' is
non-nil, wrapping a plain `iter-make' that filters and yields one file at
a time -- never materializing or classifying the full list of tracked
notes up front, so e.g. note 400's `pages get' call is never made at all
if note 50's turns out to already need the user's attention and the rest
of the run is otherwise cut short.  `denote-notion--batch-run' then drives
that generator, keeping at most `denote-notion-batch-max-concurrent-processes'
notes' worth of `ntn' subprocesses in flight at once (see
`denote-notion--sync-all-process-note' for what each note's sync
actually does), rather than either blocking Emacs for the whole batch
serially (the problem this command exists to solve) or starting every
tracked note's subprocess at once regardless of count.

Reports a one-line `message' summary of counts across five buckets:
unchanged, pushed, pulled, conflicted, errored.  A plain `message' is
sufficient here, rather than a dedicated results buffer: nothing needs
random access to individual outcomes, only the aggregate counts, and
`denote-notion--debug-buffer-name' already accumulates every `ntn'
call's raw output (including failures) for anyone who needs to look
closer at exactly which note did what.

One note's failure -- a non-zero `ntn' exit, an unexpected error --never
halts the batch; it is simply counted as `errored' and the run continues
with every other note, unlike a single interactive
`denote-notion-push''s own failure, which signals and stops that one
call outright -- there is no minibuffer prompt watching a batch run for
a signal to usefully interrupt."
  (interactive)
  (let* ((generator (gen-wrap
                     (iter-make
                      (dolist (f (seq-filter #'denote-notion--tracked-p (denote-directory-files)))
                        (iter-yield f)))))
         (counts (list (cons 'unchanged 0) (cons 'pushed 0) (cons 'pulled 0)
                       (cons 'conflicted 0) (cons 'errored 0))))
    (denote-notion--batch-run
     generator
     denote-notion-batch-max-concurrent-processes
     (lambda (file done) (denote-notion--sync-all-process-note file counts done))
     (lambda ()
       (message "denote-notion-sync-all: %d unchanged, %d pushed, %d pulled, %d conflicted, %d errored"
                (cdr (assq 'unchanged counts)) (cdr (assq 'pushed counts))
                (cdr (assq 'pulled counts)) (cdr (assq 'conflicted counts))
                (cdr (assq 'errored counts)))))))

(provide 'denote-notion)
;;; denote-notion.el ends here
