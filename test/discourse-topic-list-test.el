;;; discourse-topic-list-test.el --- Topic-list UI contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'discourse-api)
(require 'discourse-runtime)
(require 'discourse-topic-list)

(defun discourse-topic-list-test--object (&rest pairs)
  "Return a string-keyed hash table from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest discourse-topic-list-formats-web-style-counts-tags-and-age ()
  (let* ((tag (discourse-topic-list-test--object "name" "emacs"))
         (topic
          (discourse-topic-list-test--object
           "tags" (vector tag)
           "bumped_at" "2026-09-02T07:00:00Z")))
    (should (equal "1.5k" (discourse-topic-list--number 1540)))
    (should (equal "2m" (discourse-topic-list--number 2000000)))
    (should (equal "#emacs" (substring-no-properties
                              (discourse-ui-topic-tags topic))))
    (should (equal "5h"
                   (discourse-topic-list--format-activity
                    topic (date-to-time "2026-09-02T12:00:00Z"))))))

(ert-deftest discourse-topic-list-renders-discourse-information-hierarchy ()
  (let* ((category
          (discourse-topic-list-test--object
           "id" 5 "name" "General" "color" "3AB54A"))
         (profile
          (discourse-topic-list-test--object
           "title" "Example Forum"
           "description" "An example community"))
         (original-user
          (discourse-topic-list-test--object
           "id" 7 "username" "alice"
           "avatar_template" "/alice/{size}.png"))
         (latest-user
          (discourse-topic-list-test--object
           "id" 8 "username" "bob"
           "avatar_template" "/bob/{size}.png"))
         (original-poster
          (discourse-topic-list-test--object
           "user_id" 7 "description" "Original poster"))
         (latest-poster
          (discourse-topic-list-test--object
           "user_id" 8 "extras" "latest"
           "description" "Latest poster"))
         (tag
          (discourse-topic-list-test--object "name" "emacs"))
         (first
          (discourse-topic-list-test--object
           "id" 42
           "title" "A deliberately structured topic"
           "category_id" 5
           "posts_count" 13
           "views" 1540
           "bumped_at" "2026-09-02T07:00:00Z"
           "posters" (vector original-poster latest-poster)
           "tags" (vector tag)))
         (second
          (discourse-topic-list-test--object
           "id" 43
           "title" "Another topic"
           "category_id" 5
           "posts_count" 1
           "views" 9
           "bumped_at" "2026-09-02T08:00:00Z"
           "posters" (vector latest-poster)
           "tags" []))
         (page
          (discourse-topic-page-create
           :topics (list first second)
           :users (list original-user latest-user)
           :more-url nil))
         (account (discourse-runtime-create-account "https://example.test"))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'discourse-api-topic-page)
                   (lambda (_account callback &rest _arguments)
                     (funcall callback
                              (discourse-http-result-create
                               :ok-p t :data page))
                     nil))
                  ((symbol-function 'discourse-api-site-categories)
                   (lambda (_account callback &rest _arguments)
                     (funcall callback
                              (discourse-http-result-create
                               :ok-p t :data (list category)))
                     nil))
                  ((symbol-function 'discourse-api-site-profile)
                   (lambda (_account callback &rest _arguments)
                     (funcall callback
                              (discourse-http-result-create
                               :ok-p t :data profile))
                     nil))
                  ((symbol-function 'appkit-view-responsive-width)
                   (lambda (&rest _arguments)
                     (ert-fail "Topic rendering queried window width")))
                  ((symbol-function 'discourse-media-avatar-image)
                   (lambda (_account user-id)
                     (list 'image :type 'png :data user-id)))
                  ((symbol-function 'appkit-chat-avatar-resize-image)
                   (lambda (image _pixel-size) image))
                  ((symbol-function 'appkit-chat-avatar-line-pixel-height)
                   (lambda () 16))
                  ((symbol-function 'current-time)
                   (lambda () (date-to-time "2026-09-02T12:00:00Z"))))
          (setq buffer (discourse-topic-list-open-latest account nil))
          (with-current-buffer buffer
            (appkit-sync-invalidations (appkit-current-view))
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (let ((header
                     (substring-no-properties
                      (discourse-topic-list--header-line))))
                (should (string-match-p "Example Forum" header))
                (should (string-match-p "Latest" header))
                (should (string-match-p "anonymous" header)))
              (should-not (string-match-p "An example community" text))
              (should (string-match-p "A deliberately structured topic" text))
              (should (string-match-p "General" text))
              (should (string-match-p "#emacs" text))
              (should (string-match-p "@alice.*@bob" text))
              (should (string-match-p "1\\.5k" text))
              (should-not (string-match-p "\\[c:5\\]" text))
              (should-not (string-match-p "13 posts" text))
              (should-not (string-match-p "likes" text))
              (should (string-match-p "12 replies" text))
              (should (string-match-p "1\\.5k views" text))
              (let ((category-position (string-match "General" text))
                    (tags-position (string-match "#emacs" text))
                    (activity-position (string-match "5h by @bob" text))
                    (replies-position (string-match "12 replies" text))
                    (views-position (string-match "1\\.5k views" text)))
                (should
                 (< category-position tags-position activity-position
                    replies-position views-position)))
              (should
               (string-match-p
                (concat
                 "A deliberately structured topic\n"
                 "  .*General.*#emacs\n"
                 "  5h by @bob  ·  posters @alice @bob\n"
                 "  12 replies  ·  1\\.5k views\n"
                 "Another topic")
                text))
              (should-not
               (string-match-p "Replies[[:space:]]+Views[[:space:]]+Activity"
                               text))
              (should
               (equal '((:user "7") (:user "8"))
                      (discourse-topic-list--topic-dependencies first)))
              (save-excursion
                (goto-char (point-min))
                (search-forward "@alice")
                (let ((position (- (point) (length "@alice"))))
                  (should (equal '(image :type png :data "7")
                                 (get-text-property position 'display)))
                  (should (equal "@alice — Original poster"
                                 (get-text-property
                                  position 'help-echo)))))
              (let ((view (appkit-current-view)))
                (setq-local fill-column 40)
                (appkit-invalidate
                 view :structure t :parts '(frame entries))
                (appkit-sync-invalidations view)
                (should
                 (equal text
                        (buffer-substring-no-properties
                         (point-min) (point-max))))))
            (should-not
             (lookup-key discourse-topic-list-mode-map (kbd "N")))
            (should
             (eq (lookup-key discourse-topic-list-mode-map (kbd "R"))
                 #'discourse-topic-list-retry))
            (goto-char (point-min))
            (search-forward "A deliberately structured topic")
            (beginning-of-line)
            (should (equal "42"
                           (get-text-property
                            (point) discourse-topic-list-id-property)))
            (save-excursion
              (dotimes (_ 4)
                (should (equal "42"
                               (get-text-property
                                (point)
                                discourse-topic-list-id-property)))
                (forward-line 1)))
            (discourse-topic-list-next)
            (should (equal "43"
                           (get-text-property
                            (point) discourse-topic-list-id-property)))
            (let* ((view (appkit-current-view))
                   (state (appkit-view-state view))
                   requested-phase)
              (setf (discourse-topic-list-state-more-url state)
                    "/latest?page=1"
                    (discourse-topic-list-state-exhausted-p state) nil)
              (cl-letf
                  (((symbol-function 'discourse-topic-list--request)
                    (lambda (_view phase &optional _endpoint)
                      (setq requested-phase phase))))
                (discourse-topic-list--maybe-auto-load
                 view nil (point-max) (point-max)))
              (should (eq requested-phase 'older))
              (let (retried-phase retried-endpoint)
                (setf (discourse-topic-list-state-phase state) 'error
                      (discourse-topic-list-state-retry-phase state) 'older
                      (discourse-topic-list-state-retry-endpoint state)
                      "/latest?page=1")
                (should (discourse-topic-list-retry-available-p))
                (cl-letf
                    (((symbol-function 'discourse-topic-list--request)
                      (lambda (_view phase &optional endpoint)
                        (setq retried-phase phase
                              retried-endpoint endpoint))))
                  (discourse-topic-list-retry))
                (should (eq retried-phase 'older))
                (should (equal retried-endpoint "/latest?page=1")))))
          (should (string-match-p "Example Forum" (buffer-name buffer))))
          (let* ((view (with-current-buffer buffer (appkit-current-view)))
                 (state (appkit-view-state view)))
            (should
             (eq buffer
                 (discourse-topic-list-open-latest account nil)))
            (should (eq state (appkit-view-state view)))
            (should (= 2
                       (length
                        (discourse-topic-list-state-topics state)))))
      (when (discourse-account-p account)
        (discourse-runtime-stop-account account)))))


(ert-deftest discourse-topic-list-exposes-create-only-from-server-capability ()
  (let* ((account
          (discourse-runtime-create-authenticated-account
           "https://example.test" "7" "alice" "client-7"))
         (category
          (discourse-topic-list-test--object
           "id" 5 "name" "General" "permission" 1))
         (page
          (discourse-topic-page-create
           :topics nil :users nil :more-url nil
           :can-create-topic-p t))
         buffer
         composed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'discourse-api-topic-page)
              (lambda (_account callback &rest _arguments)
                (funcall callback
                         (discourse-http-result-create
                          :ok-p t :data page))
                nil))
             ((symbol-function 'discourse-api-site-categories)
              (lambda (_account callback &rest _arguments)
                (funcall callback
                         (discourse-http-result-create
                          :ok-p t :data (list category)))
                nil))
             ((symbol-function 'discourse-api-site-profile)
              (lambda (_account callback &rest _arguments)
                (funcall
                 callback
                 (discourse-http-result-create
                  :ok-p t
                  :data
                  (discourse-topic-list-test--object
                   "title" "Example Forum")))
                nil))
             ((symbol-function 'discourse-compose-new-topic)
              (lambda (sent-account &rest options)
                (setq composed (cons sent-account options))
                'compose-buffer)))
          (setq buffer
                (discourse-topic-list-open-latest account nil))
          (with-current-buffer buffer
            (appkit-sync-invalidations (appkit-current-view))
            (should (discourse-topic-list-can-create-topic-p))
            (should
             (string-match-p
              "@alice"
              (substring-no-properties
               (discourse-topic-list--header-line))))
            (should
             (eq #'discourse-topic-list-compose-topic
                 (lookup-key discourse-topic-list-mode-map (kbd "c"))))
            (discourse-topic-list-compose-topic)
            (should (eq account (car composed)))
            (should
             (eq (appkit-current-view)
                 (plist-get (cdr composed) :source-view)))))
      (discourse-runtime-stop-account account))))
(provide 'discourse-topic-list-test)

;;; discourse-topic-list-test.el ends here
