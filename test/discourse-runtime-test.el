;;; discourse-runtime-test.el --- Runtime contracts for discourse.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'discourse-runtime)
(require 'discourse-state)

(ert-deftest discourse-runtime-normalizes-and-validates-https-origin ()
  (should (equal "https://example.test"
                 (discourse-runtime-normalize-origin
                  "https://EXAMPLE.test:443/")))
  (should (equal "https://example.test:8443"
                 (discourse-runtime-normalize-origin
                  "https://example.test:8443")))
  (dolist (invalid '("http://example.test"
                     "https://user@example.test"
                     "https://example.test/forum"
                     "https://example.test/?q=x"))
    (should-error (discourse-runtime-normalize-origin invalid))))

(ert-deftest discourse-runtime-reuses-one-live-anonymous-app-per-origin ()
  (let ((first (discourse-runtime-create-account "https://example.test")))
    (unwind-protect
        (let ((second
               (discourse-runtime-create-account
                "https://EXAMPLE.test:443/")))
          (should (eq first second))
          (should (appkit-app-live-p (discourse-account-app first))))
      (discourse-runtime-stop-account first))))

(ert-deftest discourse-state-partial-observations-merge-without-data-loss ()
  (let ((state (discourse-state-create))
        (first (make-hash-table :test #'equal))
        (second (make-hash-table :test #'equal)))
    (puthash "id" 42 first)
    (puthash "title" "Complete title" first)
    (puthash "posts_count" 3 second)
    (puthash "id" "42" second)
    (discourse-state-merge-topic state first)
    (discourse-state-merge-topic state second)
    (let ((topic (discourse-state-topic state "42")))
      (should (equal "Complete title" (gethash "title" topic)))
      (should (= 3 (gethash "posts_count" topic))))))

(provide 'discourse-runtime-test)

;;; discourse-runtime-test.el ends here
