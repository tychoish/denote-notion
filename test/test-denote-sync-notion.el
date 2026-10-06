;;; test-denote-sync-notion.el --- ERT tests for denote-sync Notion backend -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Tests for the pure/stubbable parts of denote-notion.el: front-matter
;; get/set, tracked-p, body extraction, and the ntn process wrapper (with
;; `call-process' stubbed out — no network).

;;; Code:

(require 'ert)
(require 'denote-sync)
(require 'denote-sync-notion)

;; `denote-dash' is a soft dependency (see `denote-sync--dash-register-view'
;; and friends): not declared in this package's own `Package-Requires', so it
;; is not guaranteed to already be on `load-path' the way `denote'/`cl-lib'
;; are.  Add the sibling checkout's directory the same way this package's own
;; CI/dev environment would, so these tests can `require' it the same way
;; the main file's dash-view commands do.
(let ((sibling (expand-file-name "../../denote-dash" (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-directory-p sibling)
    (add-to-list 'load-path sibling)))
(require 'denote-dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixtures

(defconst test-denote-notion--md-fixture
  "---
title:      \"Example Note\"
date:       2026-01-05T17:52:00-05:00
tags:       [\"casap\", \"notion\"]
identifier: \"20260105T175200\"
signature:  \"2a8a\"
notion_tags:    [\"engineering\", \"observation\"]
notion_id:      \"2f094bf7-31a4-8081-8280-f0a225af4db2\"
notion_created: \"2026-01-22T19:54:00.000Z\"
notion_edited:  \"2026-02-23T18:18:00.000Z\"
source_url:     \"https://app.notion.com/p/example\"
---

## Body

Body content here.
")

(defconst test-denote-notion--untracked-fixture
  "---
title:      \"Untracked Note\"
date:       2026-01-05T17:52:00-05:00
tags:       [\"casap\"]
identifier: \"20260105T175201\"
signature:  \"2a8b\"
---

## Body

Untracked body.
")

(defconst test-denote-notion--untracked-with-properties-fixture
  "---
title:      \"Untracked Note\"
date:       2026-01-05T17:52:00-05:00
tags:       [\"casap\"]
identifier: \"20260105T175201\"
signature:  \"2a8b\"
notion_properties: {\"Timestamp\": {\"date\": {\"start\": \"2020-01-01\"}}}
---

## Body

Untracked body.
")

(defmacro test-denote-notion--with-fixture (content &rest body)
  "Write CONTENT to a temp file, bind it as `file', and run BODY.
Also rebinds `denote-sync-cache-directory' to a fresh temp directory
for the duration of BODY, removed afterward -- any code path reached
from BODY that writes a sync cache snapshot (`denote-notion--export-create',
`denote-notion--export-update', `denote-sync-notion--refresh-file', ...)
must never touch the real, user-configured cache directory just because
a test happened to exercise it without its own explicit binding."
  (declare (indent 1))
  `(let ((file (make-temp-file "denote-notion-test" nil ".md"))
         (denote-sync-cache-directory (make-temp-file "denote-notion-cache-test-" t)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           ,@body)
       (delete-file file)
       (delete-directory denote-sync-cache-directory t)
       (when-let* ((buf (get-file-buffer file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf)))))

(defmacro test-denote-notion--with-temp-denote-dir (dir-var &rest body)
  "Bind DIR-VAR to a fresh temp directory, run BODY, then clean up.
Several end-to-end tests below (auto-push cycles, pull/import, batch
sync) need a real directory of denote-style files rather than a single
fixture file, since they exercise `denote-directory-files' or a
filename-embedded identifier directly.  Cleanup kills any buffer still
visiting a file under DIR-VAR -- `find-file-noselect'/`denote' leave
one behind, and a lingering modified buffer from one test can otherwise
bleed into another -- then deletes DIR-VAR itself, both unconditionally
via `unwind-protect' so a failing assertion in BODY still cleans up.

Also rebinds `denote-sync-cache-directory' to a fresh temp directory
for the duration of BODY, removed afterward -- same reasoning as
`test-denote-notion--with-fixture''s identical binding: several of
this macro's own callers (`denote-sync-push' end-to-end) write a
sync cache snapshot, and must never touch the real, user-configured
cache directory just because a test happened to exercise that path."
  (declare (indent 1))
  `(let ((,dir-var (make-temp-file "denote-notion-test-" t))
         (denote-sync-cache-directory (make-temp-file "denote-notion-cache-test-" t)))
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (when-let* ((f (buffer-file-name buf)))
           (when (string-prefix-p (expand-file-name ,dir-var) (expand-file-name f))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory ,dir-var t)
       (delete-directory denote-sync-cache-directory t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-frontmatter-get

(ert-deftest test-denote-notion/frontmatter-get-existing-key ()
  "Reads an existing front-matter key's value verbatim."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (equal (denote-sync-frontmatter-get file "notion_id")
                   "\"2f094bf7-31a4-8081-8280-f0a225af4db2\""))))

(ert-deftest test-denote-notion/frontmatter-get-missing-key ()
  "Returns nil for a key that is not present."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should (null (denote-sync-frontmatter-get file "notion_id")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-frontmatter-set

(ert-deftest test-denote-notion/frontmatter-set-replaces-existing ()
  "Replaces an existing key's value in place."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_edited" "2026-03-01T00:00:00.000Z")
    (should (equal (denote-sync-frontmatter-get file "notion_edited")
                   "\"2026-03-01T00:00:00.000Z\""))))

(ert-deftest test-denote-notion/frontmatter-set-appends-missing ()
  "Appends a new key when it is not already present."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (denote-sync-frontmatter-set file "notion_id" "new-page-id")
    (should (equal (denote-sync-frontmatter-get file "notion_id") "\"new-page-id\""))))

(ert-deftest test-denote-notion/frontmatter-set-formats-list-as-array ()
  "A list value is written as a JSON-array-like bracketed, quoted list."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (denote-sync-frontmatter-set file "notion_tags" '("design" "architecture"))
    (should (equal (denote-sync-frontmatter-get file "notion_tags")
                   "[\"design\", \"architecture\"]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--tracked-p

(ert-deftest test-denote-notion/tracked-p-true ()
  "A note with a non-empty notion_id is tracked."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (denote-notion--tracked-p file))))

(ert-deftest test-denote-notion/tracked-p-false ()
  "A note with no notion_id is not tracked."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should-not (denote-notion--tracked-p file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--conflicted-p

(ert-deftest test-denote-notion/conflicted-p-true ()
  "A note with notion_conflict set to t is conflicted."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_conflict" t)
    (should (denote-notion--conflicted-p file))))

(ert-deftest test-denote-notion/conflicted-p-false-absent ()
  "A note with no notion_conflict line is not conflicted."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should-not (denote-notion--conflicted-p file))))

(ert-deftest test-denote-notion/conflicted-p-false-cleared ()
  "A note whose notion_conflict was cleared to the empty string is not
conflicted -- `denote-notion--finish-conflict-resolution's clearing
convention."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_conflict" t)
    (denote-sync-frontmatter-set file "notion_conflict" "")
    (should-not (denote-notion--conflicted-p file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--body-without-front-matter

(ert-deftest test-denote-notion/body-without-front-matter-md ()
  "Strips the YAML front matter block from a markdown note."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (equal (denote-sync--body-without-front-matter file)
                   "## Body\n\nBody content here."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--org-to-markdown

(ert-deftest test-denote-notion/org-to-markdown-denote-link-keeps-id-form ()
  "An Org-source `denote:' link is exported to the same intermediate
Markdown shape a Markdown source has natively -- `[desc](denote:ID)' --
rather than Org's built-in absolute-file-path behavior."
  (cl-letf (((symbol-function 'denote-link--ol-resolve-link-to-target)
             (lambda (link &rest _)
               (list nil (string-remove-prefix "denote:" link) nil))))
    (let ((md (denote-sync--org-to-markdown "[[denote:20260105T175200][Example Note]]")))
      (should (string-match-p (regexp-quote "[Example Note](denote:20260105T175200)") md)))))

(ert-deftest test-denote-notion/org-to-markdown-table-exports-as-pipe-table ()
  "An Org table exports to a GFM pipe table, not a raw HTML table -- plain
`ox-md' has no Markdown table transcoder and always falls back to HTML,
which Notion's importer does not read back as a table."
  (let ((md (denote-sync--org-to-markdown "| a | b |\n|---+---|\n| 1 | 2 |\n")))
    (should (string-match-p (regexp-quote "| a | b |") md))
    (should-not (string-match-p "<table" md))))

(ert-deftest test-denote-notion/org-to-markdown-src-block-exports-as-fenced-code ()
  "An Org src block exports to a fenced \"```\" code block, not a
4-space-indented one -- the fenced form is what round-trips cleanly
through Notion."
  (let ((md (denote-sync--org-to-markdown "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n")))
    (should (string-match-p (regexp-quote "```emacs-lisp") md))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-body: Org-to-Markdown parity with a Markdown source

(ert-deftest test-denote-notion/export-body-org-source-resolves-same-as-markdown-source ()
  "`denote-notion--export-body' on an Org-source note whose body contains
an Org-form `denote:' link resolves to the exact same Notion URL that
`test-denote-notion/rewrite-denote-links-tracked-target' asserts for a
Markdown-source note with the equivalent link -- proving Org and
Markdown sources converge on an identical resolved result, not just an
identical intermediate `[desc](denote:ID)' form."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((org-file (make-temp-file "denote-notion-test" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file org-file
              (insert "#+title: Org Source Note\n#+identifier: 20260201T000000\n\n"
                      "See [[denote:20260101T000000][Other Note]] for context.\n"))
            (cl-letf (((symbol-function 'denote-link--ol-resolve-link-to-target)
                       (lambda (link &rest _)
                         (list nil (string-remove-prefix "denote:" link) nil)))
                      ((symbol-function 'denote-get-path-by-id)
                       (lambda (id) (and (equal id "20260101T000000") file))))
              (let ((result (denote-sync--export-body org-file (denote-sync-get-backend 'notion))))
                (should (equal (car result)
                               "See [Other Note](https://www.notion.so/2f094bf731a480818280f0a225af4db2) for context."))
                (should-not (cdr result)))))
        (delete-file org-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--rewrite-denote-links

(ert-deftest test-denote-notion/rewrite-denote-links-tracked-target ()
  "A `denote:' link to a tracked target is rewritten to a notion.so URL,
using the target's own notion_id (dashes stripped), and no dangling
entry is recorded."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-get-path-by-id)
               (lambda (id) (and (equal id "20260101T000000") file))))
      (let* ((body "See [Other Note](denote:20260101T000000) for context.")
             (result (denote-sync--rewrite-denote-links body "/src/source.md" (denote-sync-get-backend 'notion))))
        (should (equal (car result)
                        "See [Other Note](https://www.notion.so/2f094bf731a480818280f0a225af4db2) for context."))
        (should-not (cdr result))))))

(ert-deftest test-denote-notion/rewrite-denote-links-untracked-existing-target ()
  "A `denote:' link to an existing but untracked target is rewritten to
plain text (link syntax stripped), and one dangling entry with reason
`not-yet-pushed' is recorded."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (cl-letf (((symbol-function 'denote-get-path-by-id)
               (lambda (id) (and (equal id "20260105T175201") file))))
      (let* ((body "See [Untracked Note](denote:20260105T175201) for context.")
             (result (denote-sync--rewrite-denote-links body "/src/source.md" (denote-sync-get-backend 'notion))))
        (should (equal (car result) "See Untracked Note for context."))
        (should (equal (cdr result)
                       '(("Untracked Note" "20260105T175201" not-yet-pushed "/src/source.md"))))))))

(ert-deftest test-denote-notion/rewrite-denote-links-missing-file ()
  "A `denote:' link to a nonexistent id is rewritten to plain text, and
one dangling entry with reason `missing-file' is recorded."
  (cl-letf (((symbol-function 'denote-get-path-by-id) (lambda (_id) nil)))
    (let* ((body "See [Gone Note](denote:20991231T000000) for context.")
           (result (denote-sync--rewrite-denote-links body "/src/source.md" (denote-sync-get-backend 'notion))))
      (should (equal (car result) "See Gone Note for context."))
      (should (equal (cdr result)
                     '(("Gone Note" "20991231T000000" missing-file "/src/source.md")))))))

(ert-deftest test-denote-notion/rewrite-denote-links-mixed-resolved-and-unresolved ()
  "With two links, one that resolves to a tracked target and one that
does not, only the unresolved one is reported as dangling, and both
render correctly in the rewritten body."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-get-path-by-id)
               (lambda (id)
                 (cond
                  ((equal id "20260101T000000") file)
                  (t nil)))))
      (let* ((body (concat "First [Tracked](denote:20260101T000000) link. "
                           "Second [Missing](denote:20991231T000000) link."))
             (result (denote-sync--rewrite-denote-links body "/src/source.md" (denote-sync-get-backend 'notion))))
        (should (equal (car result)
                       (concat "First [Tracked](https://www.notion.so/2f094bf731a480818280f0a225af4db2) link. "
                               "Second Missing link.")))
        (should (equal (cdr result)
                       '(("Missing" "20991231T000000" missing-file "/src/source.md"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--auto-push-dependency

(ert-deftest test-denote-notion/auto-push-dependency-already-tracked-is-noop ()
  "Returns non-nil immediately, without calling `denote-sync-push', when
the target is already tracked."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-sync-push)
               (lambda (&rest args) (error "unexpected push: %S" args))))
      (should (denote-notion--auto-push-dependency "20260105T175200" file)))))

(ert-deftest test-denote-notion/auto-push-dependency-cycle-returns-nil ()
  "Returns nil, without recursing into `denote-sync-push', when ID is
already recorded as in-flight -- this is how a `denote:' link cycle is
broken."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let ((denote-notion--auto-push-in-flight (make-hash-table :test 'equal)))
      (puthash "20260105T175201" t denote-notion--auto-push-in-flight)
      (cl-letf (((symbol-function 'denote-sync-push)
                 (lambda (&rest args) (error "unexpected push: %S" args))))
        (should-not (denote-notion--auto-push-dependency "20260105T175201" file))))))

(ert-deftest test-denote-notion/auto-push-dependency-pushes-untracked-target ()
  "Records ID as in-flight and pushes the target when it is untracked and
not already in-flight, returning non-nil afterward."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let ((denote-notion--auto-push-in-flight (make-hash-table :test 'equal))
          (pushed nil))
      (cl-letf (((symbol-function 'denote-sync-push)
                 (lambda (target-file &rest _) (setq pushed target-file))))
        (should (denote-notion--auto-push-dependency "20260105T175201" file)))
      (should (equal pushed file))
      (should (gethash "20260105T175201" denote-notion--auto-push-in-flight)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--rewrite-denote-links: auto-push (denote-sync-export-auto-push-linked-notes)

(ert-deftest test-denote-notion/rewrite-denote-links-auto-push-resolves-untracked-target ()
  "With `denote-sync-export-auto-push-linked-notes' non-nil, a link to an
existing but untracked target is resolved to a Notion URL once
`denote-notion--auto-push-dependency' reports the target now tracked,
instead of falling back to plain text."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let ((denote-sync-export-auto-push-linked-notes t))
      (cl-letf (((symbol-function 'denote-get-path-by-id)
                 (lambda (id) (and (equal id "20260105T175201") file)))
                ((symbol-function 'denote-notion--auto-push-dependency)
                 (lambda (_id target-file)
                   ;; Simulate a successful recursive push: the target
                   ;; becomes tracked as a side effect.
                   (denote-sync-frontmatter-set target-file "notion_id" "pushed-page-id")
                   t)))
        (let* ((body "See [Untracked Note](denote:20260105T175201) for context.")
               (result (denote-sync--rewrite-denote-links body "/src/source.md" (denote-sync-get-backend 'notion))))
          (should (equal (car result)
                         "See [Untracked Note](https://www.notion.so/pushedpageid) for context."))
          (should-not (cdr result)))))))

(ert-deftest test-denote-notion/rewrite-denote-links-auto-push-cycle-falls-back-to-plain-text ()
  "With `denote-sync-export-auto-push-linked-notes' non-nil, a link whose
target is caught in a cycle (`denote-notion--auto-push-dependency' returns
nil) still falls back to plain text, and is recorded with reason
`cycle-detected' rather than `not-yet-pushed'."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let ((denote-sync-export-auto-push-linked-notes t))
      (cl-letf (((symbol-function 'denote-get-path-by-id)
                 (lambda (id) (and (equal id "20260105T175201") file)))
                ((symbol-function 'denote-notion--auto-push-dependency)
                 (lambda (&rest _) nil)))
        (let* ((body "See [Untracked Note](denote:20260105T175201) for context.")
               (result (denote-sync--rewrite-denote-links body "/src/source.md" (denote-sync-get-backend 'notion))))
          (should (equal (car result) "See Untracked Note for context."))
          (should (equal (cdr result)
                         '(("Untracked Note" "20260105T175201" cycle-detected "/src/source.md")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-push: end-to-end auto-push link cycle (A <-> B)
;;
;; A links to B, and B links back to A; auto-push is on.  Pushing A must
;; terminate rather than recursing forever: B's push is triggered as a
;; dependency, and when B's own body-export encounters its link back to A
;; -- whose identifier is already recorded in-flight from the top-level
;; push of A -- that link falls back to plain text as `cycle-detected'
;; instead of recursing into A again.  Once B's push completes, A's own
;; link to B still resolves normally, since B is now tracked.

(ert-deftest test-denote-notion/push-a-links-b-links-a-cycle-terminates-and-resolves ()
  "A push of A, where A links to B and B links back to A, terminates and
resolves A's link to B normally, while B's link back to A is reported as
`cycle-detected' rather than recursing forever.
Files are given real denote-style filenames (identifier embedded in the
filename, not just front matter) since
`denote-notion--auto-push-dependency's in-flight bookkeeping keys off
`denote-retrieve-filename-identifier', which parses the filename, not
the front matter."
  (test-denote-notion--with-temp-denote-dir dir
    (let* ((id-a "20260201T000000")
           (id-b "20260201T000001")
           (file-a (expand-file-name (format "%s--note-a__tag.md" id-a) dir))
           (file-b (expand-file-name (format "%s--note-b__tag.md" id-b) dir))
           (denote-sync-export-auto-push-linked-notes t)
           (denote-sync-default-parent '(database . "db-id"))
           (create-contents nil))
      (with-temp-file file-a
        (insert (format "---\ntitle: \"A\"\nidentifier: \"%s\"\n---\n\nSee [B](denote:%s) here.\n" id-a id-b)))
      (with-temp-file file-b
        (insert (format "---\ntitle: \"B\"\nidentifier: \"%s\"\n---\n\nSee [A](denote:%s) here.\n" id-b id-a)))
      (cl-letf (((symbol-function 'denote-get-path-by-id)
                 (lambda (id)
                   (cond ((equal id id-a) file-a)
                         ((equal id id-b) file-b))))
                ((symbol-function 'denote-sync-notion--run-json)
                 (lambda (args)
                   (when (member "create" args)
                     (push (nth (1+ (seq-position args "--content")) args) create-contents))
                   (json-parse-string test-denote-notion--create-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-sync-push file-a))
      ;; B's create (triggered as a dependency of A's export) happens
      ;; before A's own create call -- see the commentary above.
      (setq create-contents (nreverse create-contents))
      (should (denote-notion--tracked-p file-a))
      (should (denote-notion--tracked-p file-b))
      (should (equal (length create-contents) 2))
      (should (equal (nth 0 create-contents) "See A here."))
      (should (string-match-p (regexp-quote "https://www.notion.so/") (nth 1 create-contents)))
      (with-current-buffer (get-buffer-create denote-notion--debug-buffer-name)
        (should (string-match-p "cycle-detected" (buffer-string)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-push: end-to-end auto-push of a non-cyclic dependency
;; (A -> B, no link back)
;;
;; Unlike the A<->B cycle test above, this proves the ordinary, common
;; case works end-to-end on its own: if the cycle test were ever
;; simplified or removed, this still stands as the guarantee that a real
;; `denote-sync-push' call with auto-push enabled actually pushes an
;; untracked dependency and resolves the link to it.

(ert-deftest test-denote-notion/push-a-links-b-no-cycle-auto-pushes-and-resolves ()
  "A push of A, where A links to B and B does NOT link back to A, pushes B
as a dependency and resolves A's link to B to a normal Notion URL."
  (test-denote-notion--with-temp-denote-dir dir
    (let* ((id-a "20260301T000000")
           (id-b "20260301T000001")
           (file-a (expand-file-name (format "%s--note-a__tag.md" id-a) dir))
           (file-b (expand-file-name (format "%s--note-b__tag.md" id-b) dir))
           (denote-sync-export-auto-push-linked-notes t)
           (denote-sync-default-parent '(database . "db-id"))
           (create-contents nil))
      (with-temp-file file-a
        (insert (format "---\ntitle: \"A\"\nidentifier: \"%s\"\n---\n\nSee [B](denote:%s) here.\n" id-a id-b)))
      (with-temp-file file-b
        (insert (format "---\ntitle: \"B\"\nidentifier: \"%s\"\n---\n\nJust B, no links back.\n" id-b)))
      (cl-letf (((symbol-function 'denote-get-path-by-id)
                 (lambda (id)
                   (cond ((equal id id-a) file-a)
                         ((equal id id-b) file-b))))
                ((symbol-function 'denote-sync-notion--run-json)
                 (lambda (args)
                   (when (member "create" args)
                     (push (nth (1+ (seq-position args "--content")) args) create-contents))
                   (json-parse-string test-denote-notion--create-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-sync-push file-a))
      ;; B's create (triggered as a dependency of A's export) happens
      ;; before A's own create call, as in the cycle test above.
      (setq create-contents (nreverse create-contents))
      (should (denote-notion--tracked-p file-b))
      (should (equal (length create-contents) 2))
      (should (equal (nth 0 create-contents) "Just B, no links back."))
      (should (string-match-p "\\[B\\](https://www\\.notion\\.so/[[:alnum:]]+) here\\."
                              (nth 1 create-contents))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--extract-page-id

(ert-deftest test-denote-notion/extract-page-id-from-bare-id ()
  "A bare page id passes through unchanged."
  (should (equal (denote-sync-notion--extract-page-id "2e994bf731a4800d99fbf40866cc0e65")
                 "2e994bf731a4800d99fbf40866cc0e65")))

(ert-deftest test-denote-notion/extract-page-id-from-url ()
  "The 32-char hex id is extracted from a trailing Notion URL segment."
  (should (equal (denote-sync-notion--extract-page-id
                  "https://app.notion.com/p/Example-2e994bf731a4800d99fbf40866cc0e65")
                 "2e994bf731a4800d99fbf40866cc0e65")))

(ert-deftest test-denote-notion/extract-page-id-strips-trailing-view-query-param ()
  "A database URL's trailing \"?v=<view-id>\" is stripped before matching,
so the view id's own 32 hex characters -- last in the raw string -- are
never mistaken for the database id."
  (should (equal (denote-sync-notion--extract-page-id
                  "https://app.notion.com/p/ea4eefe5a1054e0599b49a1a11aadea8?v=ae682ebf81954ca7a910e101133a6085")
                 "ea4eefe5a1054e0599b49a1a11aadea8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--run / denote-sync-notion--run-json (call-process stubbed)

(ert-deftest test-denote-notion/run-returns-exit-code-and-output ()
  "Wraps `call-process' output as (EXIT-CODE STDOUT STDERR)."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               (with-current-buffer (car destination) (insert "hello"))
               0)))
    (should (equal (denote-sync-notion--run '("whoami")) '(0 "hello" "")))))

(ert-deftest test-denote-notion/run-json-parses-successful-output ()
  "Parses JSON output into an alist on a zero exit code."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               (with-current-buffer (car destination) (insert "{\"page\": {\"id\": \"abc\"}}"))
               0)))
    (should (equal (map-elt (map-elt (denote-sync-notion--run-json '("pages" "get" "abc")) 'page) 'id)
                   "abc"))))

(ert-deftest test-denote-notion/run-json-signals-on-failure ()
  "A non-zero exit code raises a `user-error' carrying the CLI output."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               (with-current-buffer (car destination) (insert "not found"))
               1)))
    (should-error (denote-sync-notion--run-json '("pages" "get" "missing")) :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--parse-parent-arg

(ert-deftest test-denote-notion/parse-parent-arg-roundtrips ()
  "Parsing the output of `denote-sync-notion--parent-arg' recovers the original cons."
  (should (equal (denote-sync-notion--parse-parent-arg
                  (denote-sync-notion--parent-arg '(database . "abc-123")))
                 '(database . "abc-123"))))

(ert-deftest test-denote-notion/parse-parent-arg-nil-for-empty ()
  "Returns nil for a nil or empty string, rather than erroring."
  (should-not (denote-sync-notion--parse-parent-arg nil))
  (should-not (denote-sync-notion--parse-parent-arg "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--resolve-data-source
;;
;; A pasted full-page database URL's id is the *database* id, not the
;; data source id the registry needs -- these fixtures mirror
;; "ntn datasources resolve"'s real response shape (a `database_id' and a
;; `data_sources' array) so the resolution step is covered without a
;; live network call.

(ert-deftest test-denote-notion/resolve-data-source-single-result-no-prompt ()
  "A database with exactly one data source resolves without prompting."
  (cl-letf (((symbol-function 'denote-sync-notion--run-json)
             (lambda (&rest _)
               (json-parse-string
                "{\"data_sources\": [{\"id\": \"ds-1\", \"name\": \"Scope and Design\"}], \"database_id\": \"db-1\"}"
                :object-type 'alist :array-type 'list)))
            ((symbol-function 'completing-read)
             (lambda (&rest _) (error "should not prompt for a single data source"))))
    (should (equal (denote-sync-notion--resolve-data-source "db-1")
                   '("ds-1" . "Scope and Design")))))

(ert-deftest test-denote-notion/resolve-data-source-extracts-id-from-url ()
  "Resolves via the 32-char hex id embedded in a full Notion URL."
  (cl-letf (((symbol-function 'denote-sync-notion--run-json)
             (lambda (args)
               (should (equal (nth 2 args) "ea4eefe5a1054e0599b49a1a11aadea8"))
               (json-parse-string
                "{\"data_sources\": [{\"id\": \"ds-1\", \"name\": \"Scope and Design\"}], \"database_id\": \"db-1\"}"
                :object-type 'alist :array-type 'list))))
    (should (equal (denote-sync-notion--resolve-data-source
                    "https://app.notion.com/p/ea4eefe5a1054e0599b49a1a11aadea8?v=ae682ebf81954ca7a910e101133a6085")
                   '("ds-1" . "Scope and Design")))))

(ert-deftest test-denote-notion/resolve-data-source-prompts-among-multiple ()
  "A database with more than one data source prompts to pick by name."
  (cl-letf (((symbol-function 'denote-sync-notion--run-json)
             (lambda (&rest _)
               (json-parse-string
                "{\"data_sources\": [{\"id\": \"ds-1\", \"name\": \"First\"}, {\"id\": \"ds-2\", \"name\": \"Second\"}], \"database_id\": \"db-1\"}"
                :object-type 'alist :array-type 'list)))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "Second")))
    (should (equal (denote-sync-notion--resolve-data-source "db-1")
                   '("ds-2" . "Second")))))

(ert-deftest test-denote-notion/resolve-data-source-errors-on-empty-result ()
  "Signals a `user-error' rather than returning a bogus cons when
\"ntn datasources resolve\" reports no data sources at all."
  (cl-letf (((symbol-function 'denote-sync-notion--run-json)
             (lambda (&rest _)
               (json-parse-string "{\"data_sources\": [], \"database_id\": \"db-1\"}"
                                  :object-type 'alist :array-type 'list))))
    (should-error (denote-sync-notion--resolve-data-source "db-1") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion-add-parent

(ert-deftest test-denote-notion/add-parent-adds-registry-entry ()
  "Adds a `(list NAME 'notion (data-source . ID))' entry built from the resolved id."
  (let ((denote-sync-parent-registry nil))
    (cl-letf (((symbol-function 'denote-sync-notion--resolve-data-source)
               (lambda (_url) '("ds-1" . "Scope and Design"))))
      (denote-sync-notion-add-parent "db-1" "Scope and Design"))
    (should (equal denote-sync-parent-registry
                   '(("Scope and Design" notion (data-source . "ds-1")))))))

(ert-deftest test-denote-notion/add-parent-replaces-existing-same-name-entry ()
  "Re-adding under a name already in the registry replaces that entry
rather than appending a duplicate."
  (let ((denote-sync-parent-registry '(("Scope and Design" notion (data-source . "stale-id")))))
    (cl-letf (((symbol-function 'denote-sync-notion--resolve-data-source)
               (lambda (_url) '("fresh-id" . "Scope and Design"))))
      (denote-sync-notion-add-parent "db-1" "Scope and Design"))
    (should (equal denote-sync-parent-registry
                   '(("Scope and Design" notion (data-source . "fresh-id")))))))

(ert-deftest test-denote-notion/add-parent-defaults-name-to-resolved-name-noninteractively ()
  "With no NAME argument and not called interactively, falls back to the
data source's own Notion name instead of prompting."
  (let ((denote-sync-parent-registry nil))
    (cl-letf (((symbol-function 'denote-sync-notion--resolve-data-source)
               (lambda (_url) '("ds-1" . "Scope and Design")))
              ((symbol-function 'read-string)
               (lambda (&rest _) (error "should not prompt for a name"))))
      (denote-sync-notion-add-parent "db-1"))
    (should (equal (caar denote-sync-parent-registry) "Scope and Design"))))

(ert-deftest test-denote-notion/add-parent-copies-form-to-kill-ring ()
  "Also copies the entry's literal sexp form to the kill ring, since the
registry itself lives in a `setq' this command does not edit."
  (let ((denote-sync-parent-registry nil))
    (cl-letf (((symbol-function 'denote-sync-notion--resolve-data-source)
               (lambda (_url) '("ds-1" . "Scope and Design"))))
      (denote-sync-notion-add-parent "db-1" "Scope and Design"))
    (should (equal (current-kill 0) "(\"Scope and Design\" notion (data-source . \"ds-1\"))"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-create
;;
;; `ntn pages create --json' returns the created page object directly at
;; its top level -- unlike `ntn pages get --json', which wraps it under a
;; `page' key alongside the converted markdown.  These fixtures are flat
;; for that reason.

(defconst test-denote-notion--create-response-json
  "{\"id\": \"new-page-id\", \"url\": \"https://app.notion.com/p/new\", \"created_time\": \"2026-03-01T00:00:00.000Z\", \"last_edited_time\": \"2026-03-01T00:00:00.000Z\", \"properties\": {}}")

(ert-deftest test-denote-notion/export-create-reads-flat-response ()
  "export-create reads id/url/timestamps from a flat (unwrapped) response
and writes them correctly, and does not write `source_url'."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (&rest _)
                 (json-parse-string test-denote-notion--create-response-json
                                    :object-type 'alist :array-type 'list))))
      (denote-sync--export-create file (denote-sync-get-backend 'notion) '(database . "db-id")))
    (should (equal (denote-sync-frontmatter-get file "notion_id") "\"new-page-id\""))
    (should (equal (denote-sync-frontmatter-get file "notion_created") "\"2026-03-01T00:00:00.000Z\""))
    (should (equal (denote-sync-frontmatter-get file "notion_edited") "\"2026-03-01T00:00:00.000Z\""))
    (should-not (denote-sync-frontmatter-get file "source_url"))))

(ert-deftest test-denote-notion/export-create-parent-stores-locator-not-registry-name ()
  "notion_parent stores the parent's type:id locator, not a registry
entry's display name -- so renaming or removing that registry entry
later can't bitrot an already-created note's record of its own parent."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let ((denote-sync-parent-registry '(("My Database" . ((database . "db-id") . nil)))))
      (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                 (lambda (&rest _)
                   (json-parse-string test-denote-notion--create-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-sync--export-create file (denote-sync-get-backend 'notion) '(database . "db-id"))))
    (should (equal (denote-sync-frontmatter-get file "notion_parent") "\"database:db-id\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-update
;;
;; `ntn pages edit --json' returns only a minimal confirmation object (no
;; `url'/`properties'/`last_edited_time') -- unlike `pages create', it does
;; NOT mirror the full page object, so `--export-update' re-fetches via
;; `pages get' (wrapped under a `page' key, like every other `pages get'
;; response) for everything past the content edit itself.

(defconst test-denote-notion--edit-response-json
  "{\"id\": \"tracked-id\", \"markdown\": \"Body content here.\", \"object\": \"page_markdown\", \"request_id\": \"req-1\", \"truncated\": false, \"unknown_block_ids\": []}")

(defconst test-denote-notion--get-response-json
  "{\"markdown\": {\"markdown\": \"Body content here.\"}, \"page\": {\"id\": \"tracked-id\", \"url\": \"https://app.notion.com/p/tracked\", \"last_edited_time\": \"2026-04-01T00:00:00.000Z\", \"properties\": {}}}")

(ert-deftest test-denote-notion/export-update-reads-last-edited-from-get-not-edit ()
  "export-update reads the updated `last_edited_time' from the post-edit
`pages get' response, not from `pages edit's own (minimal) response --
the latter carries no such field.  FORCE skips the conflict check, so no
pre-edit `pages get' call happens; only the edit and post-edit get calls
need stubbing here."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (args)
                 (cond
                  ((member "edit" args)
                   (json-parse-string test-denote-notion--edit-response-json
                                      :object-type 'alist :array-type 'list))
                  ((member "get" args)
                   (json-parse-string test-denote-notion--get-response-json
                                      :object-type 'alist :array-type 'list))
                  (t (error "unexpected call in force path: %S" args))))))
      (should (equal (denote-sync--export-update file (denote-sync-get-backend 'notion) t)
                     (cons "https://app.notion.com/p/tracked" nil))))
    (should (equal (denote-sync-frontmatter-get file "notion_edited")
                   "\"2026-04-01T00:00:00.000Z\""))))

(ert-deftest test-denote-notion/export-update-resolves-properties-by-parent-locator-not-name ()
  "Default properties are found by looking the stored `notion_parent'
locator (type:id) back up in the registry, so they still resolve even
after the registry entry's display name has been renamed."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_parent" "database:db-id")
    (let ((denote-sync-parent-registry
           '(("Renamed Database" . ((database . "db-id") . ((Status . "Synced")))))))
      (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                 (lambda (args)
                   (cond
                    ((member "edit" args)
                     (json-parse-string test-denote-notion--edit-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "get" args)
                     (json-parse-string test-denote-notion--get-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "api" args) nil)
                    (t (error "unexpected call: %S" args))))))
        (denote-sync--export-update file (denote-sync-get-backend 'notion) t)))
    (should (equal (denote-sync-frontmatter-get file "notion_edited")
                   "\"2026-04-01T00:00:00.000Z\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-update, FORCE non-nil: end-to-end through the
;; literal `denote-notion--export-update' call site, not just
;; `denote-notion--export-apply-pushed-page' in isolation -- confirms the
;; shared helper still records the pushed content's hash and cache
;; snapshot correctly when reached from this call site.

(ert-deftest test-denote-notion/export-update-force-records-synced-content-hash-and-cache ()
  "A FORCE push through `denote-notion--export-update' records the pushed
content's hash as `notion_sync_hash' and writes that same content to the
id's on-disk cache, via `denote-notion--record-synced-content' inside
`denote-notion--export-apply-pushed-page'."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((denote-sync-cache-directory (make-temp-file "denote-notion-cache-test-" t))
          (id "2f094bf7-31a4-8081-8280-f0a225af4db2"))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                       (lambda (args)
                         (cond
                          ((member "edit" args)
                           (json-parse-string test-denote-notion--edit-response-json
                                              :object-type 'alist :array-type 'list))
                          ((member "get" args)
                           (json-parse-string test-denote-notion--get-response-json
                                              :object-type 'alist :array-type 'list))
                          (t (error "unexpected call in force path: %S" args))))))
              (denote-sync--export-update file (denote-sync-get-backend 'notion) t))
            (should (equal (denote-sync-frontmatter-get file "notion_sync_hash")
                           (format "%S" (denote-sync--content-hash "## Body\n\nBody content here."))))
            (should (equal (denote-sync--cache-read 'notion id) "## Body\n\nBody content here.")))
        (delete-directory denote-sync-cache-directory t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--resolve-property-sentinels

(ert-deftest test-denote-notion/resolve-property-sentinels-replaces-today ()
  "The \"<today>\" sentinel is replaced by today's date, nested anywhere in
the property-value structure."
  (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2026-07-16")))
    (should (equal (denote-sync-notion--resolve-property-sentinels
                    '((Timestamp (date (start . "<today>")))))
                   '((Timestamp (date (start . "2026-07-16"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-create: built-in Timestamp default
;;
;; A newly created page's schema often expects a `Timestamp' date property
;; to be populated even when no registry entry or per-file
;; `notion_properties' sets it explicitly -- see
;; `denote-sync-notion--default-export-properties'.

(ert-deftest test-denote-notion/export-create-applies-default-timestamp ()
  "Timestamp is populated with today's date on creation with no override."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let (patched-data)
      (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2026-07-16"))
                ((symbol-function 'denote-sync-notion--run-json)
                 (lambda (args)
                   (cond
                    ((member "create" args)
                     (json-parse-string test-denote-notion--create-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "api" args)
                     (setq patched-data (nth (1+ (seq-position args "--data")) args))
                     nil)
                    (t (error "unexpected call: %S" args))))))
        (denote-sync--export-create file (denote-sync-get-backend 'notion) '(database . "db-id")))
      (should patched-data)
      (should (string-match-p "\"Timestamp\"" patched-data))
      (should (string-match-p "2026-07-16" patched-data))
      (should-not (string-match-p "<today>" patched-data)))))

(ert-deftest test-denote-notion/export-create-file-properties-override-default-timestamp ()
  "A file's own `notion_properties' Timestamp value takes precedence over
the built-in default."
  (test-denote-notion--with-fixture test-denote-notion--untracked-with-properties-fixture
    (let (patched-data)
      (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                 (lambda (args)
                   (cond
                    ((member "create" args)
                     (json-parse-string test-denote-notion--create-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "api" args)
                     (setq patched-data (nth (1+ (seq-position args "--data")) args))
                     nil)
                    (t (error "unexpected call: %S" args))))))
        (denote-sync--export-create file (denote-sync-get-backend 'notion) '(database . "db-id")))
      (should (string-match-p "2020-01-01" patched-data))
      (should-not (string-match-p "<today>" patched-data)))))

(ert-deftest test-denote-notion/export-update-does-not-apply-default-timestamp ()
  "Update does not force the built-in Timestamp default on every edit --
only creation guarantees a value; an update with no properties configured
sends no PATCH at all."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (args)
                 (cond
                  ((member "edit" args)
                   (json-parse-string test-denote-notion--edit-response-json
                                      :object-type 'alist :array-type 'list))
                  ((member "get" args)
                   (json-parse-string test-denote-notion--get-response-json
                                      :object-type 'alist :array-type 'list))
                  ((member "api" args)
                   (error "unexpected properties PATCH on update with no configured properties"))
                  (t (error "unexpected call: %S" args))))))
      (denote-sync--export-update file (denote-sync-get-backend 'notion) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--clean-imported-body

(ert-deftest test-denote-notion/clean-imported-body-br-to-newline ()
  "A literal <br> tag (Notion's soft-line-break artifact) becomes a real newline."
  (should (equal (denote-sync-notion--clean-imported-body "one<br>two")
                 "one\ntwo")))

(ert-deftest test-denote-notion/clean-imported-body-br-variants ()
  "Self-closing and spaced <br/> / <br /> variants are also converted."
  (should (equal (denote-sync-notion--clean-imported-body "one<br/>two<br />three")
                 "one\ntwo\nthree")))

(ert-deftest test-denote-notion/clean-imported-body-unescapes-brackets ()
  "Backslash-escaped brackets (CommonMark link-syntax escaping) are unescaped,
so imported prose like a bracketed label reads naturally instead of
carrying literal backslashes."
  (should (equal (denote-sync-notion--clean-imported-body "- \\[design\\] a question")
                 "- [design] a question")))

(ert-deftest test-denote-notion/clean-imported-body-widens-block-newlines-to-paragraphs ()
  "Each single newline joining separate Notion blocks becomes a blank line,
so Markdown renders them as separate paragraphs instead of one run-on
paragraph."
  (should (equal (denote-sync-notion--clean-imported-body "## Heading\nFirst paragraph.\nSecond paragraph.")
                 "## Heading\n\nFirst paragraph.\n\nSecond paragraph.")))

(ert-deftest test-denote-notion/clean-imported-body-soft-break-stays-single-newline ()
  "A <br> soft break within one block becomes a single newline, not a
paragraph break, even after block-level newlines are widened to blank
lines."
  (should (equal (denote-sync-notion--clean-imported-body "One line<br>continues.\nNext paragraph.")
                 "One line\ncontinues.\n\nNext paragraph.")))

(ert-deftest test-denote-notion/clean-imported-body-preserves-fenced-code-block ()
  "A fenced code block's internal newlines are left as single newlines,
not widened to blank lines, so the code's own line breaks survive
instead of a blank line appearing between every line of code; the
surrounding prose newlines are still widened into paragraph breaks."
  (should (equal (denote-sync-notion--clean-imported-body
                  "Intro.\n```python\ndef f():\n    return 1\n```\nOutro.")
                 "Intro.\n\n```python\ndef f():\n    return 1\n```\n\nOutro.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--rich-text-plain

(ert-deftest test-denote-notion/rich-text-plain-single-segment ()
  "A single rich_text segment's plain_text is returned as-is."
  (should (equal (denote-sync-notion--rich-text-plain '(((plain_text . "Hello"))))
                 "Hello")))

(ert-deftest test-denote-notion/rich-text-plain-concatenates-segments ()
  "Multiple rich_text segments (e.g. mixed formatting runs within one
title) are concatenated into a single string."
  (should (equal (denote-sync-notion--rich-text-plain
                  '(((plain_text . "Hello, "))
                    ((plain_text . "world"))))
                 "Hello, world")))

(ert-deftest test-denote-notion/rich-text-plain-empty-array ()
  "An empty rich_text array (e.g. an untitled page) returns the empty string."
  (should (equal (denote-sync-notion--rich-text-plain nil) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-pull: create-new-note path
;;
;; `properties.Name.title' is a Notion rich_text array, not a plain
;; string, so this exercises the whole create path end to end rather
;; than just the extraction helper in isolation.

(defconst test-denote-notion--get-page-response-json
  "{\"markdown\": {\"markdown\": \"one<br>two \\\\[design\\\\] done\"}, \"page\": {\"id\": \"page-id\", \"url\": \"https://app.notion.com/p/x\", \"created_time\": \"2020-05-04T10:00:00.000Z\", \"last_edited_time\": \"2020-05-04T11:00:00.000Z\", \"properties\": {\"Name\": {\"type\": \"title\", \"title\": [{\"plain_text\": \"Imported Title\"}]}, \"Tags\": {\"type\": \"multi_select\", \"multi_select\": [{\"name\": \"engineering\"}]}}}}")

(ert-deftest test-denote-notion/import-page-creates-note-with-correct-title-and-date ()
  "Importing a new page extracts the plain-text title (not the raw
rich_text array), backdates the note's identifier/date to the page's
`created_time' instead of \"now\", and cleans up the body."
  (test-denote-notion--with-temp-denote-dir dir
    (let ((denote-directory (list dir)))
      (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                 (lambda (&rest _)
                   (json-parse-string test-denote-notion--get-page-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-sync-pull "page-id"))
      (let ((file (car (directory-files dir t "imported-title"))))
        (should file)
        (should (string-suffix-p ".md" file))
        (should (string-prefix-p
                 (format-time-string "%Y%m%d" (date-to-time "2020-05-04T10:00:00.000Z"))
                 (file-name-nondirectory file)))
        (should (equal (denote-sync-frontmatter-get file "notion_id") "\"page-id\""))
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "one\ntwo" (buffer-string)))
          (should (string-match-p "\\[design\\]" (buffer-string)))
          (should-not (string-match-p "<br>" (buffer-string))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-pull: dwim refresh with no page id

(ert-deftest test-denote-notion/import-page-nil-id-refreshes-tracked-target-file ()
  "With no PAGE-ID, TARGET-FILE's own tracked notion_id is re-pulled."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (&rest _)
                 (json-parse-string test-denote-notion--get-page-response-json
                                    :object-type 'alist :array-type 'list))))
      (denote-sync-pull nil file))
    (should (equal (denote-sync-frontmatter-get file "notion_edited")
                   "\"2020-05-04T11:00:00.000Z\""))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "one\ntwo" (buffer-string))))))

(ert-deftest test-denote-notion/import-page-nil-id-errors-when-untracked ()
  "With no PAGE-ID and an untracked target, errors instead of silently
doing nothing or trying to guess a page to pull."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should-error (denote-sync-pull nil file) :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--refresh-file: metadata sync

(ert-deftest test-denote-notion/import-refresh-syncs-notion-tags ()
  "A refresh updates `notion_tags' from the page's current Tags property,
not just the body and `notion_edited' -- the fixture starts with two
stale tags; the fetched page now has only one."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (equal (denote-sync-frontmatter-get file "notion_tags")
                   "[\"engineering\", \"observation\"]"))
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (&rest _)
                 (json-parse-string test-denote-notion--get-page-response-json
                                    :object-type 'alist :array-type 'list))))
      (denote-sync-pull nil file))
    (should (equal (denote-sync-frontmatter-get file "notion_tags")
                   "[\"engineering\"]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--find-tracked-file / directory-wide dedup on import

(ert-deftest test-denote-notion/find-tracked-file-locates-match-anywhere ()
  "Finds the tracking file by notion_id across the whole denote directory,
not just the current buffer."
  (test-denote-notion--with-temp-denote-dir dir
    (let ((denote-directory (list dir)))
      (let ((f1 (expand-file-name "20260101T000000--one__tag.md" dir))
            (f2 (expand-file-name "20260101T000001--two__tag.md" dir)))
        (with-temp-file f1 (insert "---\ntitle: \"One\"\nidentifier: \"20260101T000000\"\nnotion_id: \"other-page\"\n---\n\nbody\n"))
        (with-temp-file f2 (insert "---\ntitle: \"Two\"\nidentifier: \"20260101T000001\"\nnotion_id: \"page-id\"\n---\n\nbody\n"))
        (should (equal (denote-sync--find-tracked-file "page-id") f2))
        (should-not (denote-sync--find-tracked-file "no-such-id"))))))

(ert-deftest test-denote-notion/import-page-refreshes-existing-tracked-file-instead-of-duplicating ()
  "Importing a page id already tracked by some other note (not the
current buffer, and not an explicit TARGET-FILE) refreshes that note
instead of creating a duplicate."
  (test-denote-notion--with-temp-denote-dir dir
    (let ((denote-directory (list dir))
          (tracked (expand-file-name "20260101T000000--already-tracked__tag.md" dir)))
      (with-temp-file tracked
        (insert "---\ntitle:      \"Already Tracked\"\nidentifier: \"20260101T000000\"\nnotion_id:  \"page-id\"\nnotion_edited: \"2019-01-01T00:00:00.000Z\"\n---\n\nold body\n"))
      (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                 (lambda (&rest _)
                   (json-parse-string test-denote-notion--get-page-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-sync-pull "page-id"))
      (should (equal (length (directory-files dir nil "\\.md\\'")) 1))
      (should (equal (denote-sync-frontmatter-get tracked "notion_edited")
                     "\"2020-05-04T11:00:00.000Z\"")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--file-at-point

(ert-deftest test-denote-notion/file-at-point-prefers-denote-dash-when-loaded ()
  "Resolves via `denote-dash--file-at-point' when `denote-dash' is loaded,
so a note merely selected at point in a `denote-dash' or
sequence-hierarchy listing (which has no `buffer-file-name' of its own)
still resolves to the right file."
  (cl-letf (((symbol-function 'denote-dash--file-at-point) (lambda () "/from/denote-dash.md")))
    (should (equal (denote-sync--file-at-point) "/from/denote-dash.md"))))

(ert-deftest test-denote-notion/file-at-point-falls-back-to-buffer-file-name ()
  "Falls back to `buffer-file-name' when `denote-dash--file-at-point' is unavailable.
Stubs `fboundp' itself (rather than asserting `denote-dash' is unloaded)
because CI runs every test file in one Emacs process — `test-denote-dash.el'
loads first and permanently defines `denote-dash--file-at-point' for the
rest of the run, so the real function is fboundp by the time this test
runs even though this test only cares about the fallback branch."
  (let ((real-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'denote-dash--file-at-point)
                     nil
                   (funcall real-fboundp sym)))))
      (test-denote-notion--with-fixture test-denote-notion--md-fixture
        (with-current-buffer (find-file-noselect file)
          (should (equal (denote-sync--file-at-point) file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--rich-text-value

(ert-deftest test-denote-notion/rich-text-value-shape ()
  "Produces a Notion rich_text array (a vector of one text-segment alist)."
  (should (equal (denote-sync-notion--rich-text-value "Hello")
                 [((text . ((content . "Hello"))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-notion--set-page-title

(ert-deftest test-denote-notion/set-page-title-patches-discovered-title-key ()
  "Finds the property whose type is `title' (whatever it's named) and
PATCHes it, rather than assuming a fixed key like \"Name\"."
  (let (patched-data)
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (args)
                 (setq patched-data (nth (1+ (seq-position args "--data")) args))
                 nil)))
      (denote-sync-notion--set-page-title
       "page-id"
       '((Created . ((type . "created_time")))
         (Name . ((type . "title"))))
       "My Title"))
    (should patched-data)
    (should (string-match-p "\"Name\"" patched-data))
    (should (string-match-p "My Title" patched-data))))

(ert-deftest test-denote-notion/set-page-title-noop-without-title-property ()
  "Does nothing (no PATCH call) when PROPERTIES has no `title'-typed entry."
  (cl-letf (((symbol-function 'denote-sync-notion--run-json)
             (lambda (&rest args) (error "unexpected call: %S" args))))
    (denote-sync-notion--set-page-title "page-id" '((Created . ((type . "created_time")))) "My Title")))

(ert-deftest test-denote-notion/set-page-title-noop-for-nil-or-empty-title ()
  "Does nothing when TITLE is nil or empty, even with a title property present."
  (cl-letf (((symbol-function 'denote-sync-notion--run-json)
             (lambda (&rest args) (error "unexpected call: %S" args))))
    (denote-sync-notion--set-page-title "page-id" '((Name . ((type . "title")))) nil)
    (denote-sync-notion--set-page-title "page-id" '((Name . ((type . "title")))) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-create / --export-update: no title heading in
;; the exported content

(ert-deftest test-denote-notion/export-create-does-not-embed-title-in-content ()
  "The content sent to `ntn pages create' is the plain body -- no leading
H1 duplicating the page's own title."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let (create-content)
      (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                 (lambda (args)
                   (when (member "create" args)
                     (setq create-content (nth (1+ (seq-position args "--content")) args)))
                   (json-parse-string test-denote-notion--create-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-sync--export-create file (denote-sync-get-backend 'notion) '(database . "db-id")))
      (should (equal create-content "## Body\n\nUntracked body.")))))

(ert-deftest test-denote-notion/export-update-does-not-embed-title-in-content ()
  "The content sent to `ntn pages edit' is the plain body -- no leading H1."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let (edit-content)
      (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                 (lambda (args)
                   (if (member "edit" args)
                       (progn
                         (setq edit-content (nth (1+ (seq-position args "--content")) args))
                         (json-parse-string test-denote-notion--edit-response-json
                                            :object-type 'alist :array-type 'list))
                     nil))))
        (denote-sync--export-update file (denote-sync-get-backend 'notion) t))
      (should (equal edit-content "## Body\n\nBody content here.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--content-hash

(ert-deftest test-denote-notion/content-hash-stable-for-same-input ()
  "Hashing the same content twice returns the same hash."
  (should (equal (denote-sync--content-hash "same content")
                 (denote-sync--content-hash "same content"))))

(ert-deftest test-denote-notion/content-hash-differs-for-different-input ()
  "Hashing different content returns different hashes."
  (should-not (equal (denote-sync--content-hash "content one")
                     (denote-sync--content-hash "content two"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--cache-read / denote-notion--cache-write

(ert-deftest test-denote-notion/cache-write-then-read-roundtrips ()
  "Content written for an id is read back unchanged."
  (let ((denote-sync-cache-directory (make-temp-file "denote-notion-cache-test-" t)))
    (unwind-protect
        (progn
          (denote-sync--cache-write 'notion "some-page-id" "cached body content")
          (should (equal (denote-sync--cache-read 'notion "some-page-id") "cached body content")))
      (delete-directory denote-sync-cache-directory t))))

(ert-deftest test-denote-notion/cache-read-never-written-returns-nil ()
  "Reading an id with no cache file yet returns nil, rather than erroring."
  (let ((denote-sync-cache-directory (make-temp-file "denote-notion-cache-test-" t)))
    (unwind-protect
        (should-not (denote-sync--cache-read 'notion "never-written-id"))
      (delete-directory denote-sync-cache-directory t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--sync-state
;;
;; Each test pins FILE's own exportable body to "## Body\n\nBody content
;; here." (via `test-denote-notion--md-fixture'), so `stored-hash' is set
;; to exactly that body's hash to simulate "local unchanged", or to some
;; other string to simulate "local changed".  The remote side is stubbed
;; via `denote-sync-notion--run-json' to return a page with a controllable
;; `last_edited_time', compared against the fixture's own stored
;; `notion_edited' ("2026-02-23T18:18:00.000Z").

(defun test-denote-notion--stub-remote-edited (edited-time)
  "Return a `denote-sync-notion--run-json' stub reporting EDITED-TIME."
  (lambda (&rest _)
    `((page . ((last_edited_time . ,edited-time))))))

(ert-deftest test-denote-notion/sync-state-unchanged ()
  "Neither side changed: local hash matches, remote not newer than stored."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((local-hash (denote-sync--content-hash (car (denote-sync--export-body file (denote-sync-get-backend 'notion))))))
      (denote-sync-frontmatter-set file "notion_sync_hash" local-hash))
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (test-denote-notion--stub-remote-edited "2026-02-23T18:18:00.000Z")))
      (should (eq (denote-sync--sync-state file (denote-sync-get-backend 'notion)) 'unchanged)))))

(ert-deftest test-denote-notion/sync-state-local-only ()
  "Local changed (stored hash stale), remote not newer than stored."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_sync_hash" "stale-hash")
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (test-denote-notion--stub-remote-edited "2026-02-23T18:18:00.000Z")))
      (should (eq (denote-sync--sync-state file (denote-sync-get-backend 'notion)) 'local-only)))))

(ert-deftest test-denote-notion/sync-state-remote-only ()
  "Local unchanged (stored hash current), remote newer than stored."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((local-hash (denote-sync--content-hash (car (denote-sync--export-body file (denote-sync-get-backend 'notion))))))
      (denote-sync-frontmatter-set file "notion_sync_hash" local-hash))
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (test-denote-notion--stub-remote-edited "2026-03-01T00:00:00.000Z")))
      (should (eq (denote-sync--sync-state file (denote-sync-get-backend 'notion)) 'remote-only)))))

(ert-deftest test-denote-notion/sync-state-both-changed ()
  "Local changed (stored hash stale) and remote newer than stored."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_sync_hash" "stale-hash")
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (test-denote-notion--stub-remote-edited "2026-03-01T00:00:00.000Z")))
      (should (eq (denote-sync--sync-state file (denote-sync-get-backend 'notion)) 'both-changed)))))

(ert-deftest test-denote-notion/sync-state-errors-on-untracked-file ()
  "Signals a `user-error' when called on a file with no notion_id."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should-error (denote-sync--sync-state file (denote-sync-get-backend 'notion)) :type 'user-error)))

(ert-deftest test-denote-notion/sync-state-missing-stored-hash-is-local-changed ()
  "A missing `notion_sync_hash' (never recorded) is treated as local-changed,
the conservative default, even when the remote side has not moved."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (test-denote-notion--stub-remote-edited "2026-02-23T18:18:00.000Z")))
      (should (eq (denote-sync--sync-state file (denote-sync-get-backend 'notion)) 'local-only)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--sync-state-async: the two edge cases confirmed on the
;; sync path above, confirmed independently on the async path -- the
;; sync and async sibling share `denote-sync--classify-sync-state' for
;; the classification rule itself, but each computes `local-changed-p'
;; and performs its own tracked-p guard independently, so neither case
;; is guaranteed covered by the other's test.

(ert-deftest test-denote-notion/sync-state-async-missing-stored-hash-is-local-changed ()
  "The async sibling also treats a missing `notion_sync_hash' as
local-changed, matching the synchronous path's conservative default."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-sync-notion--run-json-async)
               (lambda (_args callback)
                 (funcall callback nil '((page . ((last_edited_time . "2026-02-23T18:18:00.000Z"))))))))
      (let (result)
        (denote-sync--sync-state-async file (denote-sync-get-backend 'notion) (lambda (error state) (setq result (list error state))))
        (should (equal result (list nil 'local-only)))))))

(ert-deftest test-denote-notion/sync-state-async-errors-on-untracked-file ()
  "Signals a `user-error' synchronously (before any async call is even
attempted) when called on a file with no notion_id -- the same
programming-error guard `denote-notion--sync-state' applies, confirmed
independently on the async path."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should-error (denote-sync--sync-state-async file (denote-sync-get-backend 'notion) #'ignore) :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-update: unchanged short-circuit

(ert-deftest test-denote-notion/export-update-unchanged-skips-pages-edit ()
  "When `denote-notion--sync-state' reports `unchanged', `ntn pages edit' is
never invoked at all."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((local-hash (denote-sync--content-hash (car (denote-sync--export-body file (denote-sync-get-backend 'notion))))))
      (denote-sync-frontmatter-set file "notion_sync_hash" local-hash))
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (args)
                 (cond
                  ((member "edit" args) (error "unexpected pages edit call on an unchanged file"))
                  ((member "get" args) `((page . ((last_edited_time . "2026-02-23T18:18:00.000Z")))))
                  (t (error "unexpected call: %S" args))))))
      (denote-sync--export-update file (denote-sync-get-backend 'notion) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-update: both-changed sets notion_conflict,
;; does not user-error

(ert-deftest test-denote-notion/export-update-both-changed-sets-conflict-flag-no-error ()
  "On `both-changed', `notion_conflict' is set and no `user-error' is raised;
`ntn pages edit' is never invoked."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_sync_hash" "stale-hash")
    (cl-letf (((symbol-function 'denote-sync-notion--run-json)
               (lambda (args)
                 (cond
                  ((member "edit" args) (error "unexpected pages edit call on a both-changed file"))
                  ((member "get" args) `((page . ((last_edited_time . "2026-03-01T00:00:00.000Z")))))
                  (t (error "unexpected call: %S" args))))))
      (denote-sync--export-update file (denote-sync-get-backend 'notion) nil))
    (should (denote-notion--conflicted-p file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--build-conflict-buffers

(ert-deftest test-denote-notion/build-conflict-buffers-with-ancestor ()
  "Local, remote, and ancestor buffers are all populated when a cache entry
exists for the note's notion_id."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let* ((id (string-trim (denote-sync-frontmatter-get file "notion_id") "\"" "\""))
           (denote-sync-cache-directory (make-temp-file "denote-notion-cache" t)))
      (unwind-protect
          (progn
            (denote-sync--cache-write 'notion id "ancestor body")
            (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                       (lambda (&rest _)
                         `((page . ((id . ,id)))
                           (markdown . ((markdown . "remote body")))))))
              (let* ((buffers (denote-sync--build-conflict-buffers file (denote-sync-get-backend 'notion) id))
                     (local (plist-get buffers :local))
                     (remote (plist-get buffers :remote))
                     (ancestor (plist-get buffers :ancestor)))
                (unwind-protect
                    (progn
                      (should (equal (with-current-buffer local (buffer-string))
                                     (car (denote-sync--export-body file (denote-sync-get-backend 'notion)))))
                      (should (equal (with-current-buffer remote (buffer-string)) "remote body"))
                      (should (buffer-live-p ancestor))
                      (should (equal (with-current-buffer ancestor (buffer-string)) "ancestor body")))
                  (dolist (buf (list local remote ancestor))
                    (when (buffer-live-p buf) (kill-buffer buf)))))))
        (delete-directory denote-sync-cache-directory t)))))

(ert-deftest test-denote-notion/build-conflict-buffers-without-ancestor ()
  "The :ancestor slot is nil, not a buffer, when no cache entry exists --
this is what `denote-sync-resolve-conflict' checks to pick
`ediff-buffers' over `ediff-merge-buffers-with-ancestor'."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let* ((id (string-trim (denote-sync-frontmatter-get file "notion_id") "\"" "\""))
           (denote-sync-cache-directory (make-temp-file "denote-notion-cache" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                     (lambda (&rest _)
                       `((page . ((id . ,id)))
                         (markdown . ((markdown . "remote body")))))))
            (let* ((buffers (denote-sync--build-conflict-buffers file (denote-sync-get-backend 'notion) id))
                   (local (plist-get buffers :local))
                   (remote (plist-get buffers :remote))
                   (ancestor (plist-get buffers :ancestor)))
              (unwind-protect
                  (should-not ancestor)
                (dolist (buf (list local remote))
                  (when (buffer-live-p buf) (kill-buffer buf))))))
        (delete-directory denote-sync-cache-directory t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-resolve-conflict: two-way `ediff-buffers' fallback
;; (no cached ancestor) -- confirms the buffer-selection logic inside
;; `denote-sync-resolve-conflict''s own finish lambda: with no
;; ancestor, the LOCAL buffer's text -- not `ediff-buffer-C', which only
;; a three-way `ediff-merge-buffers-with-ancestor' session ever
;; populates -- is what gets passed to
;; `denote-notion--finish-conflict-resolution'.

(ert-deftest test-denote-notion/resolve-conflict-two-way-fallback-uses-local-buffer-as-merged-text ()
  "With no cached ancestor, the two-way `ediff-buffers' fallback's finish
logic treats the local buffer as the final merged text.  `ediff-buffers'
is stubbed to immediately run its startup hook (which buffer-locally adds
the real finish function to `ediff-quit-hook') and then run that hook,
simulating an immediate quit without a real interactive ediff session."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_conflict" t)
    (let ((denote-sync-cache-directory (make-temp-file "denote-notion-cache-test-" t))
          (captured-file nil) (captured-content nil))
      (unwind-protect
          (with-temp-buffer
            (cl-letf (((symbol-function 'denote-sync-notion--run-json)
                       (lambda (&rest _)
                         '((page . ((id . "ignored")))
                           (markdown . ((markdown . "remote body differs"))))))
                      ((symbol-function 'ediff-buffers)
                       (lambda (_buf-a _buf-b startup-hooks)
                         (dolist (h startup-hooks) (funcall h))
                         (run-hooks 'ediff-quit-hook)))
                      ((symbol-function 'ediff-merge-buffers-with-ancestor)
                       (lambda (&rest _) (error "should use the two-way fallback, not the three-way merge")))
                      ((symbol-function 'denote-notion--finish-conflict-resolution)
                       (lambda (f content) (setq captured-file f captured-content content))))
              (denote-sync-resolve-conflict file)))
        (delete-directory denote-sync-cache-directory t))
      (should (equal captured-file file))
      (should (equal captured-content "## Body\n\nBody content here.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--finish-conflict-resolution

(ert-deftest test-denote-notion/finish-conflict-resolution-writes-clears-and-force-pushes ()
  "Writes MERGED-CONTENT as FILE's body, clears notion_conflict, and force-pushes."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-sync-frontmatter-set file "notion_conflict" t)
    (let (push-args)
      (cl-letf (((symbol-function 'denote-sync-push)
                 (lambda (&optional f p force) (push (list f p force) push-args))))
        (denote-sync--finish-conflict-resolution file (denote-sync-get-backend 'notion) "## Merged\n\nResolved body."))
      (should (equal (denote-sync--body-without-front-matter file) "## Merged\n\nResolved body."))
      (should-not (denote-notion--conflicted-p file))
      (should (equal push-args (list (list file nil t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--dash-register-view

(ert-deftest test-denote-notion/dash-register-view-adds-entry ()
  "Registers a `denote-dash-view' with the given name and grep-filter."
  (let ((denote-dash-saved-views nil))
    (denote-sync--dash-register-view "some-view" "some-regexp")
    (should (equal (length denote-dash-saved-views) 1))
    (let ((view (car denote-dash-saved-views)))
      (should (equal (denote-dash-view-name view) "some-view"))
      (should (equal (denote-dash-view-grep-filter view) "some-regexp")))))

(ert-deftest test-denote-notion/dash-register-view-replaces-existing-same-name ()
  "Re-registering under a name already present replaces that entry rather
than appending a duplicate."
  (let ((denote-dash-saved-views
         (list (make-denote-dash-view :name "some-view" :grep-filter "stale"))))
    (denote-sync--dash-register-view "some-view" "fresh")
    (should (equal (length denote-dash-saved-views) 1))
    (should (equal (denote-dash-view-grep-filter (car denote-dash-saved-views)) "fresh"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-dash-view-tracked

(ert-deftest test-denote-notion/dash-view-tracked-registers-and-opens ()
  "Registers a view named `denote-notion--dash-view-tracked-name' and opens
it via `denote-dash-open-view'."
  (let ((denote-dash-saved-views nil)
        (opened nil))
    (cl-letf (((symbol-function 'denote-dash-open-view)
               (lambda (name) (setq opened name))))
      (denote-sync-dash-view-tracked))
    (should (equal opened denote-notion--dash-view-tracked-name))
    (should (seq-find (lambda (v) (equal (denote-dash-view-name v)
                                         denote-notion--dash-view-tracked-name))
                      denote-dash-saved-views))))

(ert-deftest test-denote-notion/dash-view-tracked-grep-filter-matches-tracked-fixture ()
  "The registered grep-filter matches a tracked note's content."
  (let ((denote-dash-saved-views nil))
    (cl-letf (((symbol-function 'denote-dash-open-view) #'ignore))
      (denote-sync-dash-view-tracked))
    (let ((regexp (denote-dash-view-grep-filter
                   (seq-find (lambda (v) (equal (denote-dash-view-name v)
                                                denote-notion--dash-view-tracked-name))
                             denote-dash-saved-views))))
      (should (string-match-p regexp test-denote-notion--md-fixture))
      (should-not (string-match-p regexp test-denote-notion--untracked-fixture)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-dash-view-conflicts

(ert-deftest test-denote-notion/dash-view-conflicts-registers-and-opens ()
  "Registers a view named `denote-notion--dash-view-conflicts-name' and
opens it via `denote-dash-open-view'."
  (let ((denote-dash-saved-views nil)
        (opened nil))
    (cl-letf (((symbol-function 'denote-dash-open-view)
               (lambda (name) (setq opened name))))
      (denote-sync-dash-view-conflicts))
    (should (equal opened denote-notion--dash-view-conflicts-name))))

(ert-deftest test-denote-notion/dash-view-conflicts-grep-filter-matches-conflicted-only ()
  "The registered grep-filter matches a note with `notion_conflict' set to
t, but not an untouched or cleared note -- the same distinction
`denote-notion--conflicted-p' itself makes."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((denote-dash-saved-views nil))
      (cl-letf (((symbol-function 'denote-dash-open-view) #'ignore))
        (denote-sync-dash-view-conflicts))
      (let ((regexp (denote-dash-view-grep-filter
                     (seq-find (lambda (v) (equal (denote-dash-view-name v)
                                                  denote-notion--dash-view-conflicts-name))
                               denote-dash-saved-views))))
        (should-not (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (re-search-forward regexp nil t)))
        (denote-sync-frontmatter-set file "notion_conflict" t)
        (should (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (re-search-forward regexp nil t)))
        (denote-sync-frontmatter-set file "notion_conflict" "")
        (should-not (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (re-search-forward regexp nil t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--remote-dirty-p / denote-notion--mark-remote-dirty

(ert-deftest test-denote-notion/remote-dirty-p-true-when-marked ()
  "A note with notion_remote_dirty set to t is remote-dirty."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-notion--mark-remote-dirty file t)
    (should (denote-notion--remote-dirty-p file))))

(ert-deftest test-denote-notion/remote-dirty-p-false-absent ()
  "A note with no notion_remote_dirty line is not remote-dirty."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should-not (denote-notion--remote-dirty-p file))))

(ert-deftest test-denote-notion/remote-dirty-p-false-cleared ()
  "A note whose notion_remote_dirty was cleared to the empty string is not
remote-dirty."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-notion--mark-remote-dirty file t)
    (denote-notion--mark-remote-dirty file nil)
    (should-not (denote-notion--remote-dirty-p file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-dash-view-remote-updated

(ert-deftest test-denote-notion/dash-view-remote-updated-registers-and-opens ()
  "Registers a view named `denote-notion--dash-view-remote-updated-name',
opens it immediately via `denote-dash-open-view', and does not block on
the per-note async refresh (stubbed here to simply record which files it
was invoked for, rather than running any real process)."
  (let ((denote-dash-saved-views nil)
        (opened nil)
        (refreshed-for nil))
    (cl-letf (((symbol-function 'denote-dash-open-view)
               (lambda (name) (setq opened name)))
              ((symbol-function 'denote-directory-files) (lambda (&rest _) nil))
              ((symbol-function 'denote-notion--refresh-remote-dirty-marker-async)
               (lambda (file on-done) (push file refreshed-for) (funcall on-done))))
      (denote-sync-dash-view-remote-updated))
    (should (equal opened denote-notion--dash-view-remote-updated-name))
    (should (seq-find (lambda (v) (equal (denote-dash-view-name v)
                                         denote-notion--dash-view-remote-updated-name))
                      denote-dash-saved-views))
    (should-not refreshed-for)))

(ert-deftest test-denote-notion/dash-view-remote-updated-kicks-off-refresh-per-tracked-file ()
  "Kicks off the per-note async refresh for every currently Notion-tracked
file, and only tracked files."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((untracked-file (make-temp-file "denote-notion-test" nil ".md")))
      (unwind-protect
          (progn
            (with-temp-file untracked-file (insert test-denote-notion--untracked-fixture))
            (let ((denote-dash-saved-views nil)
                  (refreshed-for nil))
              (cl-letf (((symbol-function 'denote-dash-open-view) #'ignore)
                        ((symbol-function 'denote-directory-files)
                         (lambda (&rest _) (list file untracked-file)))
                        ((symbol-function 'denote-notion--refresh-remote-dirty-marker-async)
                         (lambda (f on-done) (push f refreshed-for) (funcall on-done))))
                (denote-sync-dash-view-remote-updated))
              (should (equal refreshed-for (list file)))))
        (delete-file untracked-file)))))

(ert-deftest test-denote-notion/dash-view-remote-updated-grep-filter-matches-dirty-only ()
  "The registered grep-filter matches a note marked `notion_remote_dirty',
but not an untouched or cleared one."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((denote-dash-saved-views nil))
      (cl-letf (((symbol-function 'denote-dash-open-view) #'ignore)
                ((symbol-function 'denote-directory-files) (lambda (&rest _) nil)))
        (denote-sync-dash-view-remote-updated))
      (let ((regexp (denote-dash-view-grep-filter
                     (seq-find (lambda (v) (equal (denote-dash-view-name v)
                                                  denote-notion--dash-view-remote-updated-name))
                               denote-dash-saved-views))))
        (should-not (string-match-p regexp (with-temp-buffer
                                              (insert-file-contents file)
                                              (buffer-string))))
        (denote-notion--mark-remote-dirty file t)
        (should (string-match-p regexp (with-temp-buffer
                                          (insert-file-contents file)
                                          (buffer-string))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion dash-view grep filters: four-state fixture matrix
;;
;; Checks all three `denote-notion-dash-view-*' grep filters together
;; against one four-file matrix (tracked+conflicted, tracked+clean,
;; untracked, tracked+remote-dirty) in a single temp directory,
;; confirming each filter discriminates correctly across every
;; combination of state.

(ert-deftest test-denote-notion/dash-views-grep-filters-against-four-state-fixture-matrix ()
  "The tracked/conflict/remote-dirty grep filters each match exactly the
right subset of a four-file fixture matrix covering every combination of
tracked, conflicted, and remote-dirty state."
  (test-denote-notion--with-temp-denote-dir dir
    (let* ((tracked-conflicted (expand-file-name "20260101T000000--a__tag.md" dir))
           (tracked-clean (expand-file-name "20260101T000001--b__tag.md" dir))
           (untracked (expand-file-name "20260101T000002--c__tag.md" dir))
           (tracked-remote-dirty (expand-file-name "20260101T000003--d__tag.md" dir)))
      (with-temp-file tracked-conflicted
        (insert "---\ntitle: \"A\"\nidentifier: \"20260101T000000\"\nnotion_id: \"id-a\"\n---\n\nbody\n"))
      (with-temp-file tracked-clean
        (insert "---\ntitle: \"B\"\nidentifier: \"20260101T000001\"\nnotion_id: \"id-b\"\n---\n\nbody\n"))
      (with-temp-file untracked (insert test-denote-notion--untracked-fixture))
      (with-temp-file tracked-remote-dirty
        (insert "---\ntitle: \"D\"\nidentifier: \"20260101T000003\"\nnotion_id: \"id-d\"\n---\n\nbody\n"))
      (denote-sync-frontmatter-set tracked-conflicted "notion_conflict" t)
      (denote-notion--mark-remote-dirty tracked-remote-dirty t)
      (let* ((tracked-regexp (denote-sync--frontmatter-nonempty-value-regexp "notion_id"))
             (conflict-regexp denote-notion--dash-conflict-grep-filter)
             (remote-dirty-regexp denote-notion--dash-remote-dirty-grep-filter)
             (matches-p (lambda (regexp f)
                          (with-temp-buffer
                            (insert-file-contents f)
                            (goto-char (point-min))
                            (and (re-search-forward regexp nil t) t)))))
        ;; tracked filter: matches every tracked file, excludes the untracked one
        (should (funcall matches-p tracked-regexp tracked-conflicted))
        (should (funcall matches-p tracked-regexp tracked-clean))
        (should (funcall matches-p tracked-regexp tracked-remote-dirty))
        (should-not (funcall matches-p tracked-regexp untracked))
        ;; conflict filter: matches only the conflicted file
        (should (funcall matches-p conflict-regexp tracked-conflicted))
        (should-not (funcall matches-p conflict-regexp tracked-clean))
        (should-not (funcall matches-p conflict-regexp untracked))
        (should-not (funcall matches-p conflict-regexp tracked-remote-dirty))
        ;; remote-dirty filter: matches only the remote-dirty file
        (should-not (funcall matches-p remote-dirty-regexp tracked-conflicted))
        (should-not (funcall matches-p remote-dirty-regexp tracked-clean))
        (should-not (funcall matches-p remote-dirty-regexp untracked))
        (should (funcall matches-p remote-dirty-regexp tracked-remote-dirty))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--remote-timestamp-stale-p

(ert-deftest test-denote-notion/remote-timestamp-stale-p-true-when-remote-newer ()
  "A remote timestamp lexically greater than the stored one is stale."
  (should (denote-sync--remote-timestamp-stale-p
           "2026-01-01T00:00:00.000Z" "2026-02-01T00:00:00.000Z")))

(ert-deftest test-denote-notion/remote-timestamp-stale-p-false-when-equal ()
  "An unchanged remote timestamp (equal to the stored one) is not stale."
  (should-not (denote-sync--remote-timestamp-stale-p
               "2026-01-01T00:00:00.000Z" "2026-01-01T00:00:00.000Z")))

(ert-deftest test-denote-notion/remote-timestamp-stale-p-false-when-older ()
  "A remote timestamp lexically less than the stored one is not stale."
  (should-not (denote-sync--remote-timestamp-stale-p
               "2026-02-01T00:00:00.000Z" "2026-01-01T00:00:00.000Z")))

(ert-deftest test-denote-notion/remote-timestamp-stale-p-false-when-stored-empty ()
  "A missing/empty stored timestamp (nothing recorded as synced yet) is
never considered stale -- the conservative default."
  (should-not (denote-sync--remote-timestamp-stale-p "" "2026-02-01T00:00:00.000Z")))

(ert-deftest test-denote-notion/remote-timestamp-stale-p-false-when-remote-nil ()
  "A nil remote timestamp (fetch failed to report one) is never stale."
  (should-not (denote-sync--remote-timestamp-stale-p "2026-01-01T00:00:00.000Z" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--refresh-remote-dirty-marker-async
;;
;; `denote-sync-notion--run-json-async' is stubbed throughout -- these tests
;; exercise only the pure "given this stored/remote timestamp pair, is the
;; marker set correctly" decision, not any live process or network call.

(ert-deftest test-denote-notion/refresh-remote-dirty-marker-async-marks-dirty-when-stale ()
  "Marks the file dirty when the stubbed fetch reports a newer
`last_edited_time' than the file's own stored `notion_edited'."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let (on-done-called)
      (cl-letf (((symbol-function 'denote-sync-notion--run-json-async)
                 (lambda (_args callback)
                   (funcall callback nil '((page . ((last_edited_time . "2026-03-01T00:00:00.000Z"))))))))
        (denote-sync--refresh-remote-dirty-marker-async file (denote-sync-get-backend 'notion) (lambda () (setq on-done-called t))))
      (should on-done-called)
      (should (denote-notion--remote-dirty-p file)))))

(ert-deftest test-denote-notion/refresh-remote-dirty-marker-async-clears-when-unchanged ()
  "Clears (or leaves unset) the dirty marker when the stubbed fetch reports
the same `last_edited_time' already stored."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-sync-notion--run-json-async)
               (lambda (_args callback)
                 (funcall callback nil '((page . ((last_edited_time . "2026-02-23T18:18:00.000Z"))))))))
      (denote-sync--refresh-remote-dirty-marker-async file (denote-sync-get-backend 'notion) #'ignore))
    (should-not (denote-notion--remote-dirty-p file))))

(ert-deftest test-denote-notion/refresh-remote-dirty-marker-async-calls-on-done-on-failed-fetch ()
  "Still calls ON-DONE, without touching the marker, when the stubbed
fetch reports failure (a non-nil ERROR)."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let (on-done-called)
      (cl-letf (((symbol-function 'denote-sync-notion--run-json-async)
                 (lambda (_args callback) (funcall callback "ntn failed" nil))))
        (denote-sync--refresh-remote-dirty-marker-async file (denote-sync-get-backend 'notion) (lambda () (setq on-done-called t))))
      (should on-done-called)
      (should-not (denote-notion--remote-dirty-p file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync--batch-run
;;
;; Items are held open (DONE-FN collected rather than invoked immediately)
;; so the test controls exactly when each "in-flight" item finishes,
;; letting it assert the max-concurrent bound is actually enforced --
;; invoking DONE-FN synchronously and immediately, as a real `ntn' call
;; never does, would make every dispatch-call complete before the next
;; one starts, masking any concurrency bug entirely.

(defun test-denote-notion--list-generator (items)
  "Return a `gen' struct yielding each of ITEMS in order."
  (gen-wrap (iter-make (dolist (item items) (iter-yield item)))))

(ert-deftest test-denote-notion/batch-run-respects-max-concurrent-bound ()
  "Never more than MAX-CONCURRENT items are in flight at once, across a
run of more items than the bound, and `on-complete' fires exactly once,
after every item has finished."
  (let* ((max-concurrent 3)
         (items (number-sequence 1 10))
         (current-in-flight 0)
         (max-seen 0)
         (pending nil)
         (completed nil)
         (finished-count 0))
    (denote-sync--batch-run
     (test-denote-notion--list-generator items)
     max-concurrent
     (lambda (item done)
       (setq current-in-flight (1+ current-in-flight))
       (setq max-seen (max max-seen current-in-flight))
       (push (cons item done) pending))
     (lambda () (setq finished-count (1+ finished-count))))
    (should (equal current-in-flight max-concurrent))
    (should (equal max-seen max-concurrent))
    (should (zerop finished-count))
    ;; Release every pending item one at a time, in whatever order they
    ;; were collected, re-checking the bound after each release -- the
    ;; dispatcher must never let more than MAX-CONCURRENT back in flight
    ;; even as slots are freed one by one.
    (while pending
      (let* ((entry (pop pending))
             (done (cdr entry)))
        (setq current-in-flight (1- current-in-flight))
        (funcall done)
        (should (<= current-in-flight max-concurrent))))
    (should (equal max-seen max-concurrent))
    (should (equal finished-count 1))
    (should (equal (length completed) 0))))

(ert-deftest test-denote-notion/batch-run-empty-generator-completes-immediately ()
  "`on-complete' fires immediately, with nothing ever dispatched, for an
already-empty generator."
  (let ((dispatched nil) (completed nil))
    (denote-sync--batch-run
     (test-denote-notion--list-generator nil)
     4
     (lambda (_item _done) (setq dispatched t))
     (lambda () (setq completed t)))
    (should-not dispatched)
    (should completed)))

(ert-deftest test-denote-notion/batch-run-zero-max-concurrent-completes-without-dispatch ()
  "A nonpositive MAX-CONCURRENT (the `(<= max-concurrent 0)' branch in
`denote-sync--batch-run') calls ON-COMPLETE immediately without ever
pulling an item from GENERATOR, even when the generator is nonempty --
distinct from the already-covered degenerate empty-generator case, which
exercises the same early return via an empty generator instead of a
nonpositive bound."
  (let ((dispatched nil) (completed nil))
    (denote-sync--batch-run
     (test-denote-notion--list-generator '(1 2 3))
     0
     (lambda (_item _done) (setq dispatched t))
     (lambda () (setq completed t)))
    (should-not dispatched)
    (should completed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--sync-all-process-note: per-classification dispatch
;;
;; Each test stubs `denote-notion--sync-state-async' directly (rather than
;; its own dependencies) to pin the classification outcome, then asserts
;; both which action function ran and which COUNTS cell was incremented.

(ert-deftest test-denote-notion/sync-all-process-note-unchanged-takes-no-action ()
  "`unchanged' increments the `unchanged' count and calls neither push nor
pull nor sets a conflict flag."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((counts (list (cons 'unchanged 0) (cons 'pushed 0) (cons 'pulled 0)
                         (cons 'conflicted 0) (cons 'errored 0)))
          (done-called nil))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (_file callback) (funcall callback nil 'unchanged)))
                ((symbol-function 'denote-notion--export-push-async)
                 (lambda (&rest args) (error "unexpected push: %S" args)))
                ((symbol-function 'denote-sync-notion--async-refresh)
                 (lambda (&rest args) (error "unexpected pull: %S" args))))
        (denote-notion--sync-all-process-note file counts (lambda () (setq done-called t))))
      (should done-called)
      (should (equal (cdr (assq 'unchanged counts)) 1))
      (should (equal (cdr (assq 'pushed counts)) 0))
      (should-not (denote-notion--conflicted-p file)))))

(ert-deftest test-denote-notion/sync-all-process-note-local-only-pushes ()
  "`local-only' calls `denote-notion--export-push-async' (not a second
sync-state fetch) and increments the `pushed' count."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((counts (list (cons 'unchanged 0) (cons 'pushed 0) (cons 'pulled 0)
                         (cons 'conflicted 0) (cons 'errored 0)))
          (done-called nil) (pushed-file nil))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (_file callback) (funcall callback nil 'local-only)))
                ((symbol-function 'denote-notion--export-push-async)
                 (lambda (f callback) (setq pushed-file f) (funcall callback nil (cons "url" nil))))
                ((symbol-function 'denote-sync-notion--async-refresh)
                 (lambda (&rest args) (error "unexpected pull: %S" args))))
        (denote-notion--sync-all-process-note file counts (lambda () (setq done-called t))))
      (should done-called)
      (should (equal pushed-file file))
      (should (equal (cdr (assq 'pushed counts)) 1)))))

(ert-deftest test-denote-notion/sync-all-process-note-remote-only-pulls ()
  "`remote-only' calls `denote-sync-notion--async-refresh' (not a
second sync-state fetch) and increments the `pulled' count."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((counts (list (cons 'unchanged 0) (cons 'pushed 0) (cons 'pulled 0)
                         (cons 'conflicted 0) (cons 'errored 0)))
          (done-called nil) (pulled-file nil) (pulled-id nil))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (_file callback) (funcall callback nil 'remote-only)))
                ((symbol-function 'denote-notion--export-push-async)
                 (lambda (&rest args) (error "unexpected push: %S" args)))
                ((symbol-function 'denote-sync-notion--async-refresh)
                 (lambda (f id callback) (setq pulled-file f pulled-id id) (funcall callback nil))))
        (denote-notion--sync-all-process-note file counts (lambda () (setq done-called t))))
      (should done-called)
      (should (equal pulled-file file))
      (should (equal pulled-id "2f094bf7-31a4-8081-8280-f0a225af4db2"))
      (should (equal (cdr (assq 'pulled counts)) 1)))))

(ert-deftest test-denote-notion/sync-all-process-note-both-changed-marks-conflict ()
  "`both-changed' sets `notion_conflict' directly, without a `user-error',
and increments the `conflicted' count, calling neither push nor pull."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((counts (list (cons 'unchanged 0) (cons 'pushed 0) (cons 'pulled 0)
                         (cons 'conflicted 0) (cons 'errored 0)))
          (done-called nil))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (_file callback) (funcall callback nil 'both-changed)))
                ((symbol-function 'denote-notion--export-push-async)
                 (lambda (&rest args) (error "unexpected push: %S" args)))
                ((symbol-function 'denote-sync-notion--async-refresh)
                 (lambda (&rest args) (error "unexpected pull: %S" args))))
        (denote-notion--sync-all-process-note file counts (lambda () (setq done-called t))))
      (should done-called)
      (should (denote-notion--conflicted-p file))
      (should (equal (cdr (assq 'conflicted counts)) 1)))))

(ert-deftest test-denote-notion/sync-all-process-note-classification-error-is-errored ()
  "A classification failure (ERROR non-nil from `denote-notion--sync-state-async')
increments the `errored' count rather than signaling or hanging."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let ((counts (list (cons 'unchanged 0) (cons 'pushed 0) (cons 'pulled 0)
                         (cons 'conflicted 0) (cons 'errored 0)))
          (done-called nil))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (_file callback) (funcall callback "ntn failed" nil))))
        (denote-notion--sync-all-process-note file counts (lambda () (setq done-called t))))
      (should done-called)
      (should (equal (cdr (assq 'errored counts)) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-all: end to end, one failing note among several
;; does not halt the batch

(ert-deftest test-denote-notion/sync-all-one-error-does-not-halt-other-notes ()
  "With three tracked notes -- one that errors classifying, one `unchanged',
one `local-only' -- the batch still finishes, with all three outcomes
correctly reflected in the summary rather than the run stopping at the
first failure."
  (test-denote-notion--with-temp-denote-dir dir
    (let* ((denote-directory (list dir))
           (file-err (expand-file-name "20260101T000000--errors__tag.md" dir))
           (file-unchanged (expand-file-name "20260101T000001--unchanged__tag.md" dir))
           (file-push (expand-file-name "20260101T000002--pushes__tag.md" dir))
           (summary nil))
      (dolist (f (list file-err file-unchanged file-push))
        (with-temp-file f
          (insert (format "---\ntitle: \"%s\"\nidentifier: \"%s\"\nnotion_id: \"%s\"\n---\n\nbody\n"
                          (file-name-base f) (file-name-base f) (file-name-base f)))))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (f callback)
                   (cond
                    ((equal f file-err) (funcall callback "ntn failed" nil))
                    ((equal f file-unchanged) (funcall callback nil 'unchanged))
                    ((equal f file-push) (funcall callback nil 'local-only)))))
                ((symbol-function 'denote-notion--export-push-async)
                 (lambda (_f callback) (funcall callback nil (cons "url" nil))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq summary (apply #'format fmt args)))))
        (denote-sync-all))
      (should summary)
      (should (string-match-p "1 unchanged" summary))
      (should (string-match-p "1 pushed" summary))
      (should (string-match-p "1 errored" summary))
      (should (string-match-p "0 pulled" summary))
      (should (string-match-p "0 conflicted" summary)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-sync-all: two notes erroring simultaneously, serial
;; (max-concurrent 1) degradation, and the zero-tracked-notes degenerate
;; case

(ert-deftest test-denote-notion/sync-all-two-simultaneous-errors-do-not-halt-batch ()
  "With four tracked notes -- two that error classifying and two
`unchanged' -- the batch still finishes, correctly counting both errors.
Since the stubbed `denote-notion--sync-state-async' invokes its callback
synchronously, all four notes' dispatch-and-complete cycles happen within
the same synchronous tick at the default concurrency of 4, confirming the
dispatcher's slot-refill/`in-flight' bookkeeping does not get stuck or
miscount when two failures land back to back rather than one at a time."
  (test-denote-notion--with-temp-denote-dir dir
    (let* ((denote-directory (list dir))
           (file-err1 (expand-file-name "20260101T000000--err1__tag.md" dir))
           (file-err2 (expand-file-name "20260101T000001--err2__tag.md" dir))
           (file-ok1 (expand-file-name "20260101T000002--ok1__tag.md" dir))
           (file-ok2 (expand-file-name "20260101T000003--ok2__tag.md" dir))
           (summary nil))
      (dolist (f (list file-err1 file-err2 file-ok1 file-ok2))
        (with-temp-file f
          (insert (format "---\ntitle: \"%s\"\nidentifier: \"%s\"\nnotion_id: \"%s\"\n---\n\nbody\n"
                          (file-name-base f) (file-name-base f) (file-name-base f)))))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (f callback)
                   (if (member f (list file-err1 file-err2))
                       (funcall callback "ntn failed" nil)
                     (funcall callback nil 'unchanged))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq summary (apply #'format fmt args)))))
        (denote-sync-all))
      (should summary)
      (should (string-match-p "2 unchanged" summary))
      (should (string-match-p "2 errored" summary))
      (should (string-match-p "0 pushed" summary))
      (should (string-match-p "0 pulled" summary))
      (should (string-match-p "0 conflicted" summary)))))

(ert-deftest test-denote-notion/batch-run-max-concurrent-one-is-fully-serial ()
  "With MAX-CONCURRENT 1, `denote-sync--batch-run' never has more than one
item dispatched at once -- the pool degrades to fully serial processing
at the boundary of the smallest nonzero concurrency, not just the default
of 3 `--batch-run-respects-max-concurrent-bound' already covers.  Uses the
same held-open-DONE-FN technique as that test, for the same reason: a
synchronously-completing PROCESS-FN would collapse any MAX-CONCURRENT
value down to serial via plain call-stack recursion, masking a real bound
violation rather than proving its absence."
  (let* ((max-concurrent 1)
         (items (number-sequence 1 5))
         (current-in-flight 0) (max-seen 0) (pending nil) (finished-count 0))
    (denote-sync--batch-run
     (test-denote-notion--list-generator items)
     max-concurrent
     (lambda (item done)
       (setq current-in-flight (1+ current-in-flight))
       (setq max-seen (max max-seen current-in-flight))
       (push (cons item done) pending))
     (lambda () (setq finished-count (1+ finished-count))))
    (should (equal current-in-flight 1))
    (should (equal max-seen 1))
    (while pending
      (let* ((entry (pop pending)) (done (cdr entry)))
        (setq current-in-flight (1- current-in-flight))
        (funcall done)
        (should (<= current-in-flight 1))))
    (should (equal max-seen 1))
    (should (equal finished-count 1))))

(ert-deftest test-denote-notion/sync-all-max-concurrent-one-completes-correctly ()
  "With `denote-sync-batch-max-concurrent-processes' set to 1, a full
`denote-sync-all' run over several tracked notes still completes
and reports every note's outcome correctly -- the true concurrency bound
itself is proven at the `denote-sync--batch-run' level (see
`--batch-run-max-concurrent-one-is-fully-serial'); this confirms the
value actually threads through to a working end-to-end serial run."
  (test-denote-notion--with-temp-denote-dir dir
    (let* ((denote-directory (list dir))
           (denote-sync-batch-max-concurrent-processes 1)
           (files (list (expand-file-name "20260101T000000--one__tag.md" dir)
                        (expand-file-name "20260101T000001--two__tag.md" dir)
                        (expand-file-name "20260101T000002--three__tag.md" dir)))
           (summary nil))
      (dolist (f files)
        (with-temp-file f
          (insert (format "---\ntitle: \"%s\"\nidentifier: \"%s\"\nnotion_id: \"%s\"\n---\n\nbody\n"
                          (file-name-base f) (file-name-base f) (file-name-base f)))))
      (cl-letf (((symbol-function 'denote-notion--sync-state-async)
                 (lambda (_f callback) (funcall callback nil 'unchanged)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq summary (apply #'format fmt args)))))
        (denote-sync-all))
      (should (string-match-p "3 unchanged" summary)))))

(ert-deftest test-denote-notion/sync-all-zero-tracked-notes-reports-all-zero-summary ()
  "With no tracked notes at all under `denote-directory' (only an untracked
note present), `denote-sync-all' still completes and reports an
all-zero summary -- the degenerate already-empty generator case
`denote-sync--batch-run''s own docstring calls out, exercised here
through the real `denote-sync-all' entry point."
  (test-denote-notion--with-temp-denote-dir dir
    (let* ((denote-directory (list dir))
           (untracked (expand-file-name "20260101T000000--untracked__tag.md" dir))
           (summary nil))
      (with-temp-file untracked (insert test-denote-notion--untracked-fixture))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq summary (apply #'format fmt args)))))
        (denote-sync-all))
      (should summary)
      (should (string-match-p "0 unchanged" summary))
      (should (string-match-p "0 pushed" summary))
      (should (string-match-p "0 pulled" summary))
      (should (string-match-p "0 conflicted" summary))
      (should (string-match-p "0 errored" summary)))))

(provide 'test-denote-sync-notion)
;;; test-denote-sync-notion.el ends here
