;;; discourse-media-test.el --- Discourse media contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'discourse-test-helper)
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

(defun discourse-media-test--row (key &rest demands)
  "Return a real projected KEY retaining non-nil image DEMANDS."
  (setq demands (delq nil demands))
  (appkit-projection-row-create
   :key key :payload key :resource-demands demands
   :dependencies (mapcar #'appkit-resource-demand-key demands)))

(defun discourse-media-test--surface (account identity project printer)
  "Mount ACCOUNT IDENTITY with real Resource-projecting PROJECT and PRINTER."
  (appkit-open-generated-surface
   (appkit-surface-type-create
    :name 'discourse-media-test :mode #'special-mode
    :init #'discourse-runtime--surface-init
    :update #'discourse-runtime--surface-update
    :renderer-factory
    (lambda (_surface)
      (appkit-projection-renderer-create
       :project-all (lambda (_surface _app _model) (funcall project))
       :printer (lambda (_surface _app row)
                  (when printer (funcall printer (appkit-projection-row-key row)))
                  (insert (appkit-projection-row-key row) "\n")))))
   :app (discourse-account-app account) :identity identity))

(ert-deftest discourse-media-deduplicates-account-owned-avatar-transfer ()
  (let* ((account (discourse-runtime-create-account "https://example.test"))
         (other (discourse-runtime-create-authenticated-account
                 "https://example.test" "7" "alice" "client"))
         (state (discourse-account-state account))
         (discourse-show-avatar-images t)
         (starts 0) canceled bases first second third)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p) (lambda () t))
                  ((symbol-function 'appkit-media-image-cache-existing-file) (lambda (_base) nil))
                  ((symbol-function 'appkit-media-cache-image-resource-async)
                   (lambda (_resource base &rest _arguments)
                     (push base bases)
                     (cl-incf starts)
                     (appkit-media--transfer-handle-create)))
                  ((symbol-function 'appkit-media-cancel-transfer)
                   (lambda (handle) (push handle canceled))))
          (discourse-state-merge-user
           state (discourse-media-test--object "id" 7 "avatar_template" "/alice/{size}.png"))
          (let ((project (lambda ()
                           (list (discourse-media-test--row
                                  "91" (discourse-media-avatar-demand account "7"))))))
            (setq first (discourse-media-test--surface account 'first project nil)
                  second (discourse-media-test--surface account 'second project nil)))
          (discourse-test-drain account)
          (should (= starts 1))
          (discourse-state-merge-user
           (discourse-account-state other)
           (discourse-media-test--object "id" 7 "avatar_template" "/alice/{size}.png"))
          (setq third
                (discourse-media-test--surface
                 other 'third
                 (lambda () (list (discourse-media-test--row
                                   "91" (discourse-media-avatar-demand other "7")))) nil))
          (discourse-test-drain other)
          (should (= starts 2))
          (should-not (equal (car bases) (cadr bases)))
          (appkit-surface-stop first)
          (should-not canceled)
          (appkit-surface-stop second)
          (should (= 1 (length canceled)))
          (appkit-surface-stop third)
          (should (= 2 (length canceled))))
      (discourse-runtime-stop-account account)
      (discourse-runtime-stop-account other))))

(ert-deftest discourse-media-deduplicates-image-preview-and-tracks-post ()
  (let* ((account (discourse-runtime-create-account "https://example.test"))
         (url "https://example.test/preview.png")
         (document (discourse-markup-parse
                    "<p><img src=\"https://example.test/preview.png\"></p>"
                    "https://example.test"))
         (discourse-show-image-previews t)
         (starts 0) success surface rendered)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p) (lambda () t))
                  ((symbol-function 'appkit-media-image-cache-existing-file) (lambda (_base) nil))
                  ((symbol-function 'appkit-media-cache-image-resource-async)
                   (lambda (_resource _base resolve _reject &rest _arguments)
                     (cl-incf starts) (setq success resolve)
                     (appkit-media--transfer-handle-create)))
                  ((symbol-function 'appkit-media-cancel-transfer) #'ignore)
                  ((symbol-function 'discourse-media--preview-image-from-file)
                   (lambda (_file) 'preview-image)))
          (setq surface
                (discourse-media-test--surface
                 account 'preview
                 (lambda ()
                   (let ((demand (car (discourse-media-document-demands account document))))
                     (list (discourse-media-test--row "91" demand)
                           (discourse-media-test--row "92" demand))))
                 (lambda (key) (push key rendered))))
          (discourse-test-drain account)
          (should (= starts 1))
          (setq rendered nil)
          (funcall success "/tmp/preview.png")
          (discourse-test-drain account)
          (should (equal '("91" "92") (sort rendered #'string<)))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (eq 'preview-image (discourse-media-image-preview account url "91")))
            (should (eq 'preview-image (discourse-media-image-preview account url "92")))
            (should (eq 'ready (discourse-media-image-preview-status account url)))))
      (discourse-runtime-stop-account account))))

(ert-deftest
    discourse-media-image-open-commits-before-presentation-and-fences-reopen
    ()
  (require 'discourse-topic)
  (let
      ((account
        (discourse-runtime-create-account "https://example.test"))
       transfers opened buffer surface)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-topic--request) #'ignore)
             ((symbol-function 'discourse-topic--request-site-metadata)
              #'ignore)
             ((symbol-function 'appkit-media-image-cache-existing-file)
              (lambda (_base) nil))
             ((symbol-function
               'appkit-media-cache-image-resource-async)
              (lambda
                (_resource _base resolve _reject &rest _arguments)
                (push resolve transfers)
                (appkit-media--transfer-handle-create)))
             ((symbol-function 'appkit-media-cancel-transfer) #'ignore)
             ((symbol-function 'appkit-media-open-file)
              (lambda (file)
                (should
                 (eq 'ready
                     (discourse-topic-state-media-phase
                      (appkit-surface-model surface))))
                (should
                 (equal file
                        (discourse-topic-state-media-file
                         (appkit-surface-model surface))))
                (push file opened))))
          (setq buffer
                (prog1 (discourse-topic-open account "42")
                  (discourse-test-drain account))
                surface
                (with-current-buffer buffer (appkit-current-surface)))
          (prog1
              (discourse-media-open-image account
                                          "https://example.test/first.png"
                                          :owner surface)
            (discourse-test-drain account))
          (let ((old (car transfers)))
            (prog1
                (discourse-media-open-image account
                                            "https://example.test/second.png"
                                            :owner surface)
              (discourse-test-drain account))
            (prog1 (funcall old "/tmp/stale.png")
              (discourse-test-drain account))
            (funcall (car transfers) "/tmp/current.png")
            (prog1 (appkit-surface-send surface 'synchronize)
              (discourse-test-drain account))
            (should (equal '("/tmp/current.png") opened)))
          (prog1
              (discourse-media-open-image account
                                          "https://example.test/late.png"
                                          :owner surface)
            (discourse-test-drain account))
          (let ((late (car transfers)))
            (appkit-surface-stop surface)
            (setq buffer
                  (prog1 (discourse-topic-open account "42")
                    (discourse-test-drain account))
                  surface
                  (with-current-buffer buffer
                    (appkit-current-surface)))
            (prog1 (funcall late "/tmp/late.png")
              (discourse-test-drain account))
            (prog1 (appkit-surface-send surface 'synchronize)
              (discourse-test-drain account))
            (should (equal '("/tmp/current.png") opened)))
          (cl-letf
              (((symbol-function
                 'appkit-media-image-cache-existing-file)
                (lambda (_base) "/tmp/cached.png")))
            (prog1
                (discourse-media-open-image account
                                            "https://example.test/cached.png"
                                            :owner surface)
              (discourse-test-drain account))
            (should
             (equal '("/tmp/cached.png" "/tmp/current.png") opened))))
      (discourse-runtime-stop-account account))))

(ert-deftest discourse-media-avatar-replacement-rejects-late-url-and-sync-settlement ()
  (let* ((account (discourse-runtime-create-account "https://example.test"))
         (state (discourse-account-state account))
         (discourse-show-avatar-images t) callbacks handles immediate canceled surface)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p) (lambda () t))
                  ((symbol-function 'appkit-media-image-cache-existing-file) (lambda (_base) nil))
                  ((symbol-function 'appkit-media-cache-image-resource-async)
                   (lambda (_resource _base resolve _reject &rest _arguments)
                     (let ((handle (appkit-media--transfer-handle-create)))
                       (push resolve callbacks) (push handle handles)
                       (when immediate (funcall resolve immediate))
                       handle)))
                  ((symbol-function 'appkit-media-cancel-transfer)
                   (lambda (handle) (push handle canceled)))
                  ((symbol-function 'discourse-media--image-from-file) #'identity))
          (discourse-state-merge-user
           state (discourse-media-test--object "id" 7 "avatar_template" "/old/{size}.png"))
          (setq surface
                (discourse-media-test--surface
                 account 'avatar
                 (lambda ()
                   (list (discourse-media-test--row
                          "91" (discourse-media-avatar-demand account 7)))) nil))
          (discourse-test-drain account)
          (let ((old (car callbacks)) (old-handle (car handles)))
            (discourse-state-merge-user
             state (discourse-media-test--object "id" 7 "avatar_template" "/new/{size}.png"))
            (setq immediate "/tmp/new.png")
            (appkit-surface-send surface (appkit-projection-change-create :full-p t))
            (discourse-test-drain account)
            (should (memq old-handle canceled))
            (funcall old "/tmp/old.png")
            (discourse-test-drain account)
            (with-current-buffer (appkit-surface-buffer surface)
              (should (equal "/tmp/new.png" (discourse-media-avatar-image account 7))))))
      (discourse-runtime-stop-account account))))

(ert-deftest discourse-media-preview-notifies-every-dependent-row-with-account-isolation ()
  (let* ((account (discourse-runtime-create-account "https://example.test"))
         (other (discourse-runtime-create-authenticated-account
                 "https://example.test" "7" "alice" "client"))
         (url "https://cdn.test/shared.png")
         (discourse-show-image-previews t) callbacks bases rendered other-surface)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p) (lambda () t))
                  ((symbol-function 'appkit-media-image-cache-existing-file) (lambda (_base) nil))
                  ((symbol-function 'appkit-media-cache-image-resource-async)
                   (lambda (_resource base resolve _reject &rest _arguments)
                     (push base bases) (push resolve callbacks)
                     (appkit-media--transfer-handle-create)))
                  ((symbol-function 'appkit-media-cancel-transfer) #'ignore)
                  ((symbol-function 'discourse-media--preview-image-from-file) #'identity))
          (discourse-media-test--surface
           account 'preview
           (lambda ()
             (let ((demand (discourse-media-preview-demand account url)))
               (list (discourse-media-test--row "91" demand)
                     (discourse-media-test--row "92" demand)
                     (discourse-media-test--row "93"))))
           (lambda (key) (push key rendered)))
          (discourse-test-drain account)
          (let ((first (car callbacks)))
            (setq other-surface
                  (discourse-media-test--surface
                   other 'preview
                   (lambda () (list (discourse-media-test--row
                                     "91" (discourse-media-preview-demand other url)))) nil))
            (discourse-test-drain other)
            (should-not (equal (car bases) (cadr bases)))
            (setq rendered nil)
            (funcall first "/tmp/first.png")
            (discourse-test-drain account)
            (should (equal '("91" "92") (sort rendered #'string<)))
            (with-current-buffer (appkit-surface-buffer other-surface)
              (should (eq 'loading (discourse-media-image-preview-status other url))))))
      (discourse-runtime-stop-account account)
      (discourse-runtime-stop-account other))))

(provide 'discourse-media-test)

;;; discourse-media-test.el ends here
