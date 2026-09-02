;;; discourse-media-test.el --- Discourse media contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'discourse-media)

(defun discourse-media-test--object (&rest pairs)
  "Return a string-keyed hash table from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest discourse-media-resolves-avatar-template-against-account ()
  (let* ((account (discourse-runtime-create-account "https://example.test"))
         (state (discourse-account-state account)))
    (unwind-protect
        (progn
          (discourse-state-merge-user
           state
           (discourse-media-test--object
            "id" 7 "username" "alice"
            "avatar_template" "/user/alice/{size}.png"))
          (should
           (equal "https://example.test/user/alice/64.png"
                  (discourse-media--avatar-url account "7"))))
      (discourse-runtime-stop-account account))))

(ert-deftest discourse-media-deduplicates-account-owned-avatar-transfer ()
  (let* ((account (discourse-runtime-create-account "https://example.test"))
         (state (discourse-account-state account))
         (starts 0))
    (unwind-protect
        (progn
          (discourse-state-merge-user
           state
           (discourse-media-test--object
            "id" 7 "username" "alice"
            "avatar_template" "/user/alice/{size}.png"))
          (cl-letf (((symbol-function
                     'appkit-media-inline-image-rendering-available-p)
                    (lambda () t))
                   ((symbol-function
                     'appkit-media-image-cache-existing-file)
                    (lambda (_base) nil))
                   ((symbol-function
                     'appkit-media-cache-image-resource-async)
                    (lambda (&rest _arguments)
                      (cl-incf starts)
                      'transfer))
                   ((symbol-function 'appkit-media-transfer-p)
                    (lambda (value) (eq value 'transfer)))
                   ((symbol-function 'appkit-register-handle)
                    (lambda (&rest _arguments) 'handle)))
            (should-not (discourse-media-avatar-image account "7"))
            (should-not (discourse-media-avatar-image account 7))
            (should (= starts 1))))
      (discourse-runtime-stop-account account))))

(ert-deftest discourse-media-deduplicates-image-preview-and-tracks-post ()
  (let* ((account
          (discourse-runtime-create-account "https://example.test"))
         (url "https://example.test/preview.png")
         (starts 0)
         success)
    (unwind-protect
        (cl-letf
            (((symbol-function
               'appkit-media-inline-image-rendering-available-p)
              (lambda () t))
             ((symbol-function
               'appkit-media-image-cache-existing-file)
              (lambda (_base) nil))
             ((symbol-function
               'appkit-media-cache-image-resource-async)
              (lambda (_resource _base success-callback
                        _error-callback &rest _arguments)
                (cl-incf starts)
                (setq success success-callback)
                'transfer))
             ((symbol-function 'appkit-media-transfer-p)
              (lambda (value) (eq value 'transfer)))
             ((symbol-function 'appkit-register-handle)
              (lambda (&rest _arguments) 'handle))
             ((symbol-function 'appkit-cancel-handle)
              (lambda (&rest _arguments) t))
             ((symbol-function
               'discourse-media--preview-image-from-file)
              (lambda (_file) 'preview-image)))
          (should-not
           (discourse-media-image-preview account url "91"))
          (should-not
           (discourse-media-image-preview account url 91))
          (should (= starts 1))
          (let* ((cache (discourse-media--preview-cache account))
                 (posts
                  (gethash
                   url
                   (discourse-media--preview-cache-consumers cache))))
            (should (gethash "91" posts)))
          (funcall success "/tmp/preview.png")
          (should (eq 'preview-image
                      (discourse-media-image-preview
                       account url "91")))
          (should (eq 'ready
                      (discourse-media-image-preview-status
                       account url))))
      (discourse-runtime-stop-account account))))

(provide 'discourse-media-test)

;;; discourse-media-test.el ends here
