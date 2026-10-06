;;; test-denote-sync.el --- ERT tests for denote-sync generic engine -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'denote-sync)

(defconst test-denote-sync--fixture-empty-frontmatter
  "---
title: \"Test Note\"
date: 2026-01-05T17:52:00-05:00
tags: [\"test\"]
identifier: \"20260105T175200\"
---

Hello world body.
")

(ert-deftest test-denote-sync/backend-registration-and-retrieval ()
  "Backends can be registered, retrieved by name, and listed."
  (let ((dummy (denote-sync-backend-create :name 'test-backend :frontmatter-prefix "test")))
    (denote-sync-register-backend dummy)
    (should (eq (denote-sync-get-backend 'test-backend) dummy))
    (should (memq dummy (denote-sync-registered-backends)))
    (setq denote-sync-backends (assq-delete-all 'test-backend denote-sync-backends))))

(ert-deftest test-denote-sync/unknown-backend-signals-error ()
  "Querying an unregistered backend name signals an error."
  (should-error (denote-sync-get-backend 'nonexistent-backend)))

(ert-deftest test-denote-sync/frontmatter-get-and-set ()
  "Front-matter values can be retrieved and mutated."
  (let ((temp-file (make-temp-file "denote-sync-test-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert test-denote-sync--fixture-empty-frontmatter))
          (should (equal (denote-sync-frontmatter-get temp-file "title") "\"Test Note\""))
          (should-not (denote-sync-frontmatter-get temp-file "test_prop"))
          (denote-sync-frontmatter-set temp-file "test_prop" "value123")
          (should (equal (denote-sync-frontmatter-get temp-file "test_prop") "\"value123\""))
          ;; Replace existing
          (denote-sync-frontmatter-set temp-file "test_prop" "newval")
          (should (equal (denote-sync-frontmatter-get temp-file "test_prop") "\"newval\"")))
      (delete-file temp-file))))

(ert-deftest test-denote-sync/frontmatter-set-creates-delimiters-when-missing ()
  "When a file has no YAML delimiters, `denote-sync-frontmatter-set' creates them."
  (let ((temp-file (make-temp-file "denote-sync-test-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert "Plain body without front matter.\n"))
          (denote-sync-frontmatter-set temp-file "my_key" "my_val")
          (should (equal (denote-sync-frontmatter-get temp-file "my_key") "\"my_val\""))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (should (string-prefix-p "---\nmy_key: \"my_val\"\n---" (buffer-string)))))
      (delete-file temp-file))))

(ert-deftest test-denote-sync/cache-compound-naming ()
  "Cache files are compound-named <backend>__<id> in `denote-sync-cache-directory'."
  (let ((denote-sync-cache-directory (make-temp-file "sync-cache-" t)))
    (unwind-protect
        (let ((path (denote-sync--cache-file-for 'notion "abc-123")))
          (should (equal (file-name-nondirectory path) "notion__abc-123"))
          (should (equal (file-name-directory path) (file-name-as-directory denote-sync-cache-directory))))
      (delete-directory denote-sync-cache-directory t))))

(ert-deftest test-denote-sync/cache-write-and-read ()
  "Content can be written and read back from the cache."
  (let ((denote-sync-cache-directory (make-temp-file "sync-cache-" t)))
    (unwind-protect
        (progn
          (should-not (denote-sync--cache-read 'google-docs "doc-456"))
          (denote-sync--cache-write 'google-docs "doc-456" "cached markdown text")
          (should (equal (denote-sync--cache-read 'google-docs "doc-456") "cached markdown text")))
      (delete-directory denote-sync-cache-directory t))))

(ert-deftest test-denote-sync/detect-backend-from-id ()
  "Target backend is inferred from known URL shapes."
  (let ((notion-b (denote-sync-backend-create :name 'notion))
        (gdoc-b (denote-sync-backend-create :name 'google-docs)))
    (let ((denote-sync-backends (list (cons 'notion notion-b) (cons 'google-docs gdoc-b))))
      (should (eq (denote-sync--detect-backend-from-id "https://www.notion.so/myworkspace/page-1234") notion-b))
      (should (eq (denote-sync--detect-backend-from-id "https://docs.google.com/document/d/1AbC-xyz/edit") gdoc-b))
      (should-not (denote-sync--detect-backend-from-id "bare-id-string")))))

(ert-deftest test-denote-sync/batch-run-bounded-concurrency ()
  "Batch runner runs items up to the max concurrent limit."
  (let* ((items '(1 2 3 4 5 6 7 8))
         (generator (gen-wrap (iter-make (dolist (item items) (iter-yield item)))))
         (in-flight 0)
         (max-seen 0)
         (completed 0)
         (done-called nil))
    (denote-sync--batch-run
     generator 3
     (lambda (item done)
       (setq in-flight (1+ in-flight))
       (when (> in-flight max-seen) (setq max-seen in-flight))
       (setq completed (1+ completed))
       (setq in-flight (1- in-flight))
       (funcall done))
     (lambda () (setq done-called t)))
    (should done-called)
    (should (= completed 8))
    (should (<= max-seen 3))))


(ert-deftest test-denote-sync/tracked-by-multiple-backends-disambiguation ()
  "A note tracked by both backends detects both and can target each individually."
  (let ((notion-b (denote-sync-backend-create :name 'notion :frontmatter-prefix "notion"))
        (gdoc-b (denote-sync-backend-create :name 'google-docs :frontmatter-prefix "gdoc"))
        (temp-file (make-temp-file "denote-sync-multitrack-" nil ".md")))
    (unwind-protect
        (let ((denote-sync-backends (list (cons 'notion notion-b) (cons 'google-docs gdoc-b))))
          (with-temp-file temp-file
            (insert "---\ntitle: \"Multi Note\"\nnotion_id: \"nid-1\"\ngdoc_id: \"gid-2\"\n---\n\nBody.\n"))
          (should (equal (denote-sync--tracked-backends temp-file) (list notion-b gdoc-b)))
          (should (denote-sync-tracked-p temp-file notion-b))
          (should (denote-sync-tracked-p temp-file gdoc-b))
          (should (equal (denote-sync--get-id temp-file notion-b) "nid-1"))
          (should (equal (denote-sync--get-id temp-file gdoc-b) "gid-2")))
      (delete-file temp-file))))

(provide 'test-denote-sync)
;;; test-denote-sync.el ends here
