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

(ert-deftest discourse-state-indexes-site-categories-users-and-profile ()
  (let ((state (discourse-state-create))
        (category (make-hash-table :test #'equal))
        (user (make-hash-table :test #'equal))
        (profile (make-hash-table :test #'equal)))
    (puthash "id" 5 category)
    (puthash "name" "General" category)
    (puthash "id" 7 user)
    (puthash "username" "alice" user)
    (puthash "title" "Example Forum" profile)
    (discourse-state-merge-categories state (list category))
    (discourse-state-merge-user state user)
    (discourse-state-set-site-profile state profile)
    (should (discourse-state-categories-loaded-p state))
    (should (equal "General"
                   (gethash "name"
                            (discourse-state-category state "5"))))
    (should (equal "alice"
                   (gethash "username"
                            (discourse-state-user state "7"))))
    (should (equal "Example Forum"
                   (gethash "title"
                            (discourse-state-site-profile state))))))

(provide 'discourse-runtime-test)

;;; discourse-runtime-test.el ends here
