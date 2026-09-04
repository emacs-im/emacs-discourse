;;; discourse-media.el --- Discourse image Resources and presentation Effects -*- lexical-binding: t; -*-

;;; Commentary:

;; Projection rows declare account-private avatar and preview Resources.
;; Explicit image actions acquire and present through Surface-owned Effects.

;;; Code:

(require 'cl-lib)

(require 'subr-x)

(require 'url-expand)

(require 'url-parse)

(require 'appkit-chat-avatar)

(require 'appkit-core)

(require 'appkit-media-image)

(require 'appkit-media-resource)

(require 'appkit-media-effect)

(require 'discourse-customize)

(require 'discourse-runtime)

(require 'discourse-state)
(require 'appkit-resource)
(require 'discourse-markup)

(declare-function discourse-topic--set-media-state "discourse-topic"
                  (state phase file error))

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
     (list (discourse-account-id account) user-id url)))
   (expand-file-name "avatars/" discourse-media-cache-directory)))

(defun discourse-media--image-from-file (file)
  "Return a circular Appkit-valid avatar descriptor for FILE, or nil."
  (when (appkit-media-file-present-p file)
    (or (appkit-media-circular-image-from-file file 64)
        (condition-case nil
            (let ((image (create-image file nil nil :ascent 'center)))
              (and (appkit-media-image-object-valid-p image) image))
          (error nil)))))

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
         (list (discourse-account-id account) url)))
   (expand-file-name "previews/" discourse-media-cache-directory)))

(defun discourse-media--preview-image-from-file (file)
  "Return a bounded Appkit preview image for FILE, or nil."
  (when (appkit-media-file-present-p file)
    (appkit-media-preview-image-from-file file)))

(cl-defun discourse-media-open-image (account url &key owner)
  "Request image acquisition and presentation on the initiating Surface."
  (when-let* ((url (discourse-media--safe-http-url url)))
    (unless (and (appkit-surface-live-p owner)
                 (eq (appkit-surface-app owner) (discourse-account-app account)))
      (user-error "Discourse image owner is no longer live"))
    (appkit-surface-send owner (list 'media-open account url))))

(defun discourse-media-copy-url (url)
  "Copy media URL to the kill ring."
  (unless (discourse-media--safe-http-url url)
    (user-error "Discourse media URL is invalid"))
  (kill-new (substring-no-properties url))
  (message "Copied Discourse media URL"))

(defun discourse-media--update (model message)
  "Commit image acquisition before starting a presentation Effect."
  (let (effect)
    (pcase message
      (`(media-open ,account ,url)
       (discourse-topic--set-media-state model 'opening nil nil)
       (setq effect
             (appkit-effect-create
              :key 'discourse-image-open
              :input
              (appkit-media-image-acquisition-create
               (appkit-media-resource-create
                :url url :name (or (appkit-media-url-filename url) "image"))
               (expand-file-name
                (md5 (md5 (prin1-to-string
                           (list (discourse-account-origin account) url))))
                (expand-file-name "open/" discourse-media-cache-directory))
               :headers appkit-media-image-accept-headers)
              :start #'appkit-media-image-acquisition-start
              :success (lambda (_input file) (list 'media-acquired file))
              :failure (lambda (_input reason) (list 'media-failed reason))
              :cancellation-requirement 'transport)))
      (`(media-acquired ,file)
       (discourse-topic--set-media-state model 'ready file nil)
       (setq effect
             (appkit-effect-create
              :key 'discourse-image-open :input file
              :start #'appkit-media-file-presentation-start
              :success (lambda (_input _result) '(media-presented))
              :failure (lambda (_input reason) (list 'media-failed reason))
              :cancellation-requirement 'logical)))
      ('(media-presented)
       (discourse-topic--set-media-state
        model 'presented (discourse-topic-state-media-file model) nil))
      (`(media-failed ,reason)
       (discourse-topic--set-media-state
        model 'error (discourse-topic-state-media-file model) reason)))
    (appkit-next :model model :render
                 (if (memq (car-safe message)
                           '(media-open media-acquired media-presented media-failed))
                     (appkit-projection-change-create :frame-p t)
                   appkit-render-none)
                 :commands (and effect (list (appkit-command-start-effect effect))))))

(defun discourse-media--demand (key url base)
  "Declare private image KEY at URL with persistent cache BASE."
  (appkit-resource-demand-create
   :key key
   :input (appkit-media-image-acquisition-create
           (appkit-media-resource-create
            :url url :name (or (appkit-media-url-filename url) "image.img"))
           base :headers appkit-media-image-accept-headers)
   :loader #'appkit-media-image-resource-load
   :acquisition-identity (list 'discourse-image key url)
   :sharing-policy 'app-private
   :cache-policy 'while-interested))

(defun discourse-media-avatar-demand (account user-id)
  "Return the declarative Resource demand for ACCOUNT USER-ID's avatar."
  (when (and user-id discourse-show-avatar-images
             (appkit-media-inline-image-rendering-available-p))
    (let* ((user-id (discourse-state-id user-id))
           (url (discourse-media--avatar-url account user-id)))
      (when url
        (discourse-media--demand
         (list 'avatar user-id url) url
         (discourse-media--cache-base account user-id url))))))

(defun discourse-media-preview-demand (account url)
  "Return the declarative Resource demand for ACCOUNT's preview URL."
  (when (and discourse-show-image-previews
             (appkit-media-inline-image-rendering-available-p))
    (when-let* ((url (discourse-media--safe-http-url url)))
      (discourse-media--demand
       (list 'preview url) url
       (discourse-media--preview-cache-base account url)))))

(defun discourse-media--resource-state (account key)
  "Return KEY's state in ACCOUNT's currently rendering Surface."
  (when-let* ((surface (appkit-current-surface))
              ((eq (appkit-surface-app surface) (discourse-account-app account))))
    (appkit-resource-state surface key)))

(defun discourse-media--resource-image (account key decode)
  "Return ACCOUNT KEY's ready image, decoding its local file with DECODE."
  (when-let* ((state (discourse-media--resource-state account key))
              ((eq (appkit-resource-state-status state) 'ready))
              (file (appkit-resource-state-value state)))
    (let* ((cache (discourse-account-resources account))
           (cache-key (list 'decoded-image key))
           (cached (gethash cache-key cache)))
      (if (equal file (car-safe cached))
          (cdr cached)
        (let ((image (funcall decode file)))
          (puthash cache-key (cons file image) cache)
          image)))))

(defun discourse-media-avatar-image (account user-id)
  "Return ACCOUNT USER-ID's ready avatar without starting acquisition."
  (when (and user-id discourse-show-avatar-images
             (appkit-media-inline-image-rendering-available-p))
    (let* ((user-id (discourse-state-id user-id))
           (url (discourse-media--avatar-url account user-id)))
      (when url
        (discourse-media--resource-image
         account (list 'avatar user-id url) #'discourse-media--image-from-file)))))

(defun discourse-media-image-preview (account url _post-id)
  "Return ACCOUNT URL's ready preview without starting acquisition.
The projected row's Resource interest owns its lifetime and dependent redraw."
  (when (and discourse-show-image-previews
             (appkit-media-inline-image-rendering-available-p))
    (when-let* ((url (discourse-media--safe-http-url url)))
      (discourse-media--resource-image
       account (list 'preview url) #'discourse-media--preview-image-from-file))))

(defun discourse-media-image-preview-status (account url)
  "Return ACCOUNT URL's ready, loading, missing, or disabled preview status."
  (cond
   ((or (not discourse-show-image-previews)
        (not (appkit-media-inline-image-rendering-available-p))) 'disabled)
   ((not (discourse-media--safe-http-url url)) 'missing)
   (t
    (let ((state (discourse-media--resource-state account (list 'preview url))))
      (pcase (and state (appkit-resource-state-status state))
        ('ready 'ready)
        ('failed 'missing)
        (_ 'loading))))))

(defun discourse-media-document-demands (account document)
  "Collect preview Resource demands from ACCOUNT's semantic DOCUMENT."
  (let (demands)
    (cl-labels
        ((visit (node)
           (cond
            ((appkit-markup-document-p node)
             (mapc #'visit (appkit-markup-document-blocks node)))
            ((appkit-markup-paragraph-p node)
             (mapc #'visit (appkit-markup-paragraph-children node)))
            ((appkit-markup-heading-p node)
             (mapc #'visit (appkit-markup-heading-children node)))
            ((appkit-markup-quote-p node)
             (mapc #'visit (appkit-markup-quote-blocks node)))
            ((appkit-markup-list-p node)
             (mapc #'visit (appkit-markup-list-items node)))
            ((appkit-markup-list-item-p node)
             (mapc #'visit (appkit-markup-list-item-blocks node)))
            ((appkit-markup-link-p node)
             (mapc #'visit (appkit-markup-link-children node)))
            ((or (appkit-markup-object-p node) (appkit-markup-object-block-p node))
             (let ((value (if (appkit-markup-object-p node)
                              (appkit-markup-object-value node)
                            (appkit-markup-object-block-value node))))
               (when (discourse-markup-provider-object-p value)
                 (let* ((kind (discourse-markup-provider-object-kind value))
                        (data (discourse-markup-provider-object-data value))
                        (url (pcase kind
                               ((or 'image 'media)
                                (or (discourse-media--safe-http-url
                                     (plist-get data :preview-url))
                                    (plist-get data :url)))
                               ('onebox (plist-get data :image-url)))))
                   (when-let* ((demand (discourse-media-preview-demand account url)))
                     (push demand demands)))))))))
      (visit document))
    (nreverse demands)))

(provide 'discourse-media)

;;; discourse-media.el ends here
