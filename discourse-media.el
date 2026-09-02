;;; discourse-media.el --- Account-owned Discourse media adapters -*- lexical-binding: t; -*-

;;; Commentary:

;; Discourse supplies protocol objects and avatar templates.  Appkit owns the
;; asynchronous transfer, atomic disk cache, image validation, and lifecycle
;; handles.  Avatar transfers belong to the account application so every live
;; view of that account shares one result.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-expand)
(require 'url-parse)
(require 'appkit-chat-avatar)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'discourse-customize)
(require 'discourse-runtime)
(require 'discourse-state)

(cl-defstruct (discourse-media--avatar-cache
               (:constructor discourse-media--avatar-cache-create))
  "Account-scoped mutable state for progressively loaded avatars."
  images
  inflight
  failures
  urls)

(defconst discourse-media--avatar-store-key '(discourse-media avatar-cache)
  "Appkit resource-store key for one account's avatar cache.")

(defun discourse-media--new-table ()
  "Return an equal-tested media table."
  (make-hash-table :test #'equal))

(defun discourse-media--cache (account)
  "Return ACCOUNT's application-owned avatar cache."
  (let* ((app (and (discourse-account-p account)
                   (discourse-account-app account)))
         (store (and (appkit-app-p app)
                     (appkit-app-resource-store app))))
    (unless store
      (error "Discourse account has no Appkit resource store"))
    (or (gethash discourse-media--avatar-store-key store)
        (let ((cache
               (discourse-media--avatar-cache-create
                :images (discourse-media--new-table)
                :inflight (discourse-media--new-table)
                :failures (discourse-media--new-table)
                :urls (discourse-media--new-table))))
          (puthash discourse-media--avatar-store-key cache store)
          cache))))

(defun discourse-media--user (account user-id)
  "Return ACCOUNT's canonical USER-ID object, or nil."
  (and (discourse-account-p account)
       (discourse-state-user
        (discourse-account-state account) user-id)))

(defun discourse-media--avatar-template (account user-id)
  "Return ACCOUNT USER-ID's canonical avatar template, or nil."
  (when-let* ((user (discourse-media--user account user-id))
              (template (gethash "avatar_template" user)))
    (and (stringp template)
         (not (string-empty-p template))
         (substring-no-properties template))))

(defun discourse-media--avatar-url (account user-id)
  "Resolve ACCOUNT USER-ID's avatar template to a safe HTTP(S) URL."
  (when-let* ((template (discourse-media--avatar-template account user-id))
              (sized
               (replace-regexp-in-string
                "{size}" "64" template t t))
              (url
               (condition-case nil
                   (url-expand-file-name
                    sized (concat (discourse-account-origin account) "/"))
                 (error nil)))
              (parsed (and url (ignore-errors
                                 (url-generic-parse-url url))))
              (scheme (and parsed
                           (downcase (or (url-type parsed) "")))))
    (and (member scheme '("http" "https")) url)))

(defun discourse-media--cache-base (account user-id url)
  "Return a stable avatar cache base for ACCOUNT USER-ID and URL."
  (expand-file-name
   (md5
    (prin1-to-string
     (list (discourse-account-origin account) user-id url)))
   (expand-file-name "avatars/" discourse-media-cache-directory)))

(defun discourse-media--image-from-file (file)
  "Return a circular Appkit-valid avatar descriptor for FILE, or nil."
  (when (appkit-media-file-present-p file)
    (or (appkit-media-circular-image-from-file file 64)
        (condition-case nil
            (let ((image (create-image file nil nil :ascent 'center)))
              (and (appkit-media-image-object-valid-p image) image))
          (error nil)))))

(defun discourse-media--notify-avatar (account user-id)
  "Redraw ACCOUNT rows that depend on USER-ID's presentation."
  (let ((app (discourse-account-app account))
        (resource (list :user user-id)))
    (when (appkit-app-live-p app)
      (maphash
       (lambda (_view-id view)
         (when (and (appkit-view-live-p view)
                    (memq (appkit-view-mode view)
                          '(discourse-topic-list-mode
                            discourse-topic-mode)))
           (appkit-request-sync view :resource resource)))
       (appkit-app-view-registry app)))))

(defun discourse-media--current-url-p
    (account cache user-id url token)
  "Return non-nil when URL and TOKEN still identify USER-ID in CACHE."
  (let ((record
         (gethash user-id
                  (discourse-media--avatar-cache-inflight cache)))
        (canonical (discourse-media--avatar-url account user-id)))
    (and (appkit-app-live-p (discourse-account-app account))
         (eq token (plist-get record :token))
         (equal url
                (gethash user-id
                         (discourse-media--avatar-cache-urls cache)))
         (or (null canonical) (equal canonical url)))))

(defun discourse-media--finish-inflight (cache user-id token)
  "Finish USER-ID's TOKEN record in CACHE."
  (let* ((table (discourse-media--avatar-cache-inflight cache))
         (record (gethash user-id table)))
    (when (eq token (plist-get record :token))
      (remhash user-id table)
      (when-let* ((handle (plist-get record :lifecycle)))
        (appkit-cancel-handle handle))
      t)))

(defun discourse-media--avatar-succeeded
    (account cache user-id url token file)
  "Install ACCOUNT USER-ID avatar FILE for current URL and TOKEN."
  (let ((current-p
         (discourse-media--current-url-p
          account cache user-id url token)))
    (discourse-media--finish-inflight cache user-id token)
    (when current-p
      (let ((image (discourse-media--image-from-file file)))
        (if image
            (progn
              (puthash user-id (cons url image)
                       (discourse-media--avatar-cache-images cache))
              (remhash user-id
                       (discourse-media--avatar-cache-failures cache)))
          (puthash user-id url
                   (discourse-media--avatar-cache-failures cache)))
        (discourse-media--notify-avatar account user-id)))))

(defun discourse-media--avatar-failed
    (account cache user-id url token _reason)
  "Record ACCOUNT USER-ID avatar failure for current URL and TOKEN."
  (let ((current-p
         (discourse-media--current-url-p
          account cache user-id url token)))
    (discourse-media--finish-inflight cache user-id token)
    (when current-p
      (puthash user-id url
               (discourse-media--avatar-cache-failures cache))
      (discourse-media--notify-avatar account user-id))))

(defun discourse-media--cancel-old-inflight (cache user-id)
  "Cancel CACHE's superseded USER-ID avatar transfer."
  (let* ((table (discourse-media--avatar-cache-inflight cache))
         (record (gethash user-id table)))
    (when record
      (remhash user-id table)
      (when-let* ((handle (plist-get record :lifecycle)))
        (appkit-cancel-handle handle)))))

(defun discourse-media--select-url (cache user-id url)
  "Make URL current for USER-ID in CACHE and clear superseded values."
  (let ((urls (discourse-media--avatar-cache-urls cache)))
    (unless (equal url (gethash user-id urls))
      (discourse-media--cancel-old-inflight cache user-id)
      (remhash user-id (discourse-media--avatar-cache-images cache))
      (remhash user-id (discourse-media--avatar-cache-failures cache))
      (puthash user-id url urls))))

(defun discourse-media--start-avatar-fetch (account cache user-id url)
  "Start one avatar transfer owned by ACCOUNT for USER-ID and URL."
  (let* ((app (discourse-account-app account))
         (inflight (discourse-media--avatar-cache-inflight cache))
         (token (list 'avatar url (float-time)))
         (record (list :token token :lifecycle nil))
         transfer)
    (puthash user-id record inflight)
    (setq transfer
          (appkit-media-cache-image-resource-async
           (appkit-media-resource-create
            :url url
            :name (or (appkit-media-url-filename url) "avatar.img"))
           (discourse-media--cache-base account user-id url)
           (lambda (file)
             (discourse-media--avatar-succeeded
              account cache user-id url token file))
           (lambda (reason)
             (discourse-media--avatar-failed
              account cache user-id url token reason))
           :headers (copy-tree appkit-media-image-accept-headers)))
    (cond
     ((not (appkit-media-transfer-p transfer)) nil)
     ((not (eq token (plist-get (gethash user-id inflight) :token)))
      (appkit-media-cancel-transfer transfer)
      nil)
     ((not (appkit-app-live-p app))
      (remhash user-id inflight)
      (appkit-media-cancel-transfer transfer)
      nil)
     (t
      (let ((handle
             (appkit-register-handle
              app 'media-transfer transfer
              #'appkit-media-cancel-transfer)))
        (setq record (plist-put record :lifecycle handle))
        (puthash user-id record inflight)
        handle)))))

(defun discourse-media-avatar-image (account user-id)
  "Return cached ACCOUNT USER-ID avatar image or start its acquisition.

Until acquisition completes, return nil so callers retain stable textual
fallback.  A completion invalidates only rows depending on this user."
  (when (and discourse-show-avatar-images
             (discourse-account-p account)
             (appkit-app-live-p (discourse-account-app account))
             (appkit-media-inline-image-rendering-available-p))
    (when-let* ((user-id (discourse-state-id user-id))
                (url (discourse-media--avatar-url account user-id)))
      (let* ((cache (discourse-media--cache account))
             (images (discourse-media--avatar-cache-images cache))
             (inflight (discourse-media--avatar-cache-inflight cache))
             (failures (discourse-media--avatar-cache-failures cache)))
        (discourse-media--select-url cache user-id url)
        (let ((cached (gethash user-id images)))
          (cond
           ((and (equal (car-safe cached) url)
                 (appkit-media-image-object-valid-p (cdr-safe cached)))
            (cdr cached))
           ((gethash user-id inflight) nil)
           ((equal (gethash user-id failures) url) nil)
           (t
            (let* ((base
                    (discourse-media--cache-base account user-id url))
                   (file
                    (appkit-media-image-cache-existing-file base))
                   (image (and file
                               (discourse-media--image-from-file file))))
              (if image
                  (progn
                    (puthash user-id (cons url image) images)
                    image)
                (discourse-media--start-avatar-fetch
                 account cache user-id url)
                nil)))))))))


(cl-defstruct (discourse-media--preview-cache
               (:constructor discourse-media--preview-cache-create))
  "Account-scoped mutable state for post image previews."
  images
  inflight
  failures
  consumers)

(defconst discourse-media--preview-store-key '(discourse-media preview-cache)
  "Appkit resource-store key for one account's post image previews.")

(defun discourse-media--preview-cache (account)
  "Return ACCOUNT's application-owned post image preview cache."
  (let* ((app (and (discourse-account-p account)
                   (discourse-account-app account)))
         (store (and (appkit-app-p app)
                     (appkit-app-resource-store app))))
    (unless store
      (error "Discourse account has no Appkit resource store"))
    (or (gethash discourse-media--preview-store-key store)
        (let ((cache
               (discourse-media--preview-cache-create
                :images (discourse-media--new-table)
                :inflight (discourse-media--new-table)
                :failures (discourse-media--new-table)
                :consumers (discourse-media--new-table))))
          (puthash discourse-media--preview-store-key cache store)
          cache))))

(defun discourse-media--safe-http-url (url)
  "Return property-free URL when it uses HTTP(S), otherwise nil."
  (when (and (stringp url) (not (string-empty-p url)))
    (let* ((clean (substring-no-properties url))
           (parsed (ignore-errors (url-generic-parse-url clean)))
           (scheme (and parsed
                        (downcase (or (url-type parsed) "")))))
      (and (member scheme '("http" "https")) clean))))

(defun discourse-media--preview-cache-base (account url)
  "Return persistent preview cache base for ACCOUNT and URL."
  (expand-file-name
   (md5 (prin1-to-string
         (list (discourse-account-origin account) url)))
   (expand-file-name "previews/" discourse-media-cache-directory)))

(defun discourse-media--preview-image-from-file (file)
  "Return a bounded Appkit preview image for FILE, or nil."
  (when (appkit-media-file-present-p file)
    (appkit-media-preview-image-from-file file)))

(defun discourse-media--register-preview-consumer
    (cache url post-id)
  "Record that POST-ID presentation consumes CACHE URL."
  (when post-id
    (let* ((consumers (discourse-media--preview-cache-consumers cache))
           (posts
            (or (gethash url consumers)
                (let ((table (discourse-media--new-table)))
                  (puthash url table consumers)
                  table))))
      (puthash post-id t posts))))

(defun discourse-media--notify-preview (account cache url)
  "Redraw ACCOUNT topic rows consuming CACHE URL."
  (let ((app (discourse-account-app account))
        (posts
         (gethash url
                  (discourse-media--preview-cache-consumers cache))))
    (when (and (appkit-app-live-p app) (hash-table-p posts))
      (maphash
       (lambda (_view-id view)
         (when (and (appkit-view-live-p view)
                    (eq (appkit-view-mode view) 'discourse-topic-mode))
           (maphash
            (lambda (post-id _present)
              (appkit-request-sync
               view :resource (list :post post-id)))
            posts)))
       (appkit-app-view-registry app)))))

(defun discourse-media--finish-preview (cache url token)
  "Finish CACHE URL's TOKEN record."
  (let* ((inflight (discourse-media--preview-cache-inflight cache))
         (record (gethash url inflight)))
    (when (eq token (plist-get record :token))
      (remhash url inflight)
      (when-let* ((handle (plist-get record :lifecycle)))
        (appkit-cancel-handle handle))
      t)))

(defun discourse-media--preview-current-p
    (account cache url token)
  "Return non-nil when URL TOKEN may still update ACCOUNT CACHE."
  (and (appkit-app-live-p (discourse-account-app account))
       (eq token
           (plist-get
            (gethash url
                     (discourse-media--preview-cache-inflight cache))
            :token))))

(defun discourse-media--preview-succeeded
    (account cache url token file)
  "Install ACCOUNT CACHE preview URL from FILE for current TOKEN."
  (let ((current-p
         (discourse-media--preview-current-p
          account cache url token)))
    (discourse-media--finish-preview cache url token)
    (when current-p
      (let ((image (discourse-media--preview-image-from-file file)))
        (if image
            (progn
              (puthash url image
                       (discourse-media--preview-cache-images cache))
              (remhash url
                       (discourse-media--preview-cache-failures cache)))
          (puthash url t
                   (discourse-media--preview-cache-failures cache)))
        (discourse-media--notify-preview account cache url)))))

(defun discourse-media--preview-failed
    (account cache url token _reason)
  "Record ACCOUNT CACHE preview URL failure for current TOKEN."
  (let ((current-p
         (discourse-media--preview-current-p
          account cache url token)))
    (discourse-media--finish-preview cache url token)
    (when current-p
      (puthash url t
               (discourse-media--preview-cache-failures cache))
      (discourse-media--notify-preview account cache url))))

(defun discourse-media--start-preview-fetch (account cache url)
  "Start one application-owned preview transfer for ACCOUNT URL."
  (let* ((app (discourse-account-app account))
         (inflight (discourse-media--preview-cache-inflight cache))
         (token (list 'preview url (float-time)))
         (record (list :token token :lifecycle nil))
         transfer)
    (puthash url record inflight)
    (setq transfer
          (appkit-media-cache-image-resource-async
           (appkit-media-resource-create
            :url url
            :name (or (appkit-media-url-filename url)
                      "preview.img"))
           (discourse-media--preview-cache-base account url)
           (lambda (file)
             (discourse-media--preview-succeeded
              account cache url token file))
           (lambda (reason)
             (discourse-media--preview-failed
              account cache url token reason))
           :headers (copy-tree appkit-media-image-accept-headers)))
    (cond
     ((not (appkit-media-transfer-p transfer)) nil)
     ((not (eq token
               (plist-get (gethash url inflight) :token)))
      (appkit-media-cancel-transfer transfer)
      nil)
     ((not (appkit-app-live-p app))
      (remhash url inflight)
      (appkit-media-cancel-transfer transfer)
      nil)
     (t
      (let ((handle
             (appkit-register-handle
              app 'media-transfer transfer
              #'appkit-media-cancel-transfer)))
        (setq record (plist-put record :lifecycle handle))
        (puthash url record inflight)
        handle)))))

(defun discourse-media-image-preview (account url post-id)
  "Return cached ACCOUNT URL preview and register POST-ID as its consumer.
Start one deduplicated account-owned acquisition when needed."
  (when (and discourse-show-image-previews
             (discourse-account-p account)
             (appkit-app-live-p (discourse-account-app account))
             (appkit-media-inline-image-rendering-available-p))
    (when-let* ((url (discourse-media--safe-http-url url)))
      (let* ((post-id
              (condition-case nil
                  (and post-id (discourse-state-id post-id))
                (error nil)))
             (cache (discourse-media--preview-cache account))
             (images (discourse-media--preview-cache-images cache))
             (inflight (discourse-media--preview-cache-inflight cache))
             (failures (discourse-media--preview-cache-failures cache)))
        (discourse-media--register-preview-consumer
         cache url post-id)
        (or (gethash url images)
            (unless (or (gethash url inflight)
                        (gethash url failures))
              (let* ((base
                      (discourse-media--preview-cache-base account url))
                     (file
                      (appkit-media-image-cache-existing-file base))
                     (image
                      (and file
                           (discourse-media--preview-image-from-file file))))
                (if image
                    (progn
                      (puthash url image images)
                      image)
                  (discourse-media--start-preview-fetch
                   account cache url)
                  nil))))))))

(defun discourse-media-image-preview-status (account url)
  "Return ACCOUNT URL preview status.
The result is one of `ready', `loading', `missing', or `disabled'."
  (let ((url (discourse-media--safe-http-url url)))
    (cond
     ((or (not discourse-show-image-previews)
          (not (appkit-media-inline-image-rendering-available-p)))
      'disabled)
     ((null url) 'missing)
     (t
      (let ((cache (discourse-media--preview-cache account)))
        (cond
         ((gethash url (discourse-media--preview-cache-images cache))
          'ready)
         ((gethash url (discourse-media--preview-cache-failures cache))
          'missing)
         (t 'loading)))))))

(cl-defun discourse-media-open-image (account url &key owner)
  "Open ACCOUNT image URL inside Emacs, lifecycle-owned by OWNER."
  (when-let* ((url (discourse-media--safe-http-url url)))
    (appkit-media-open-resource
     (appkit-media-resource-create
      :url url
      :name (or (appkit-media-url-filename url) "image"))
     :kind 'image
     :cache-key
     (md5 (prin1-to-string
           (list (discourse-account-origin account) url)))
     :cache-directory
     (expand-file-name "open/" discourse-media-cache-directory)
     :client-label "Discourse"
     :owner owner)))

(defun discourse-media-copy-url (url)
  "Copy media URL to the kill ring."
  (unless (discourse-media--safe-http-url url)
    (user-error "Discourse media URL is invalid"))
  (kill-new (substring-no-properties url))
  (message "Copied Discourse media URL"))
(provide 'discourse-media)

;;; discourse-media.el ends here
