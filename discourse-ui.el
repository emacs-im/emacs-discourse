;;; discourse-ui.el --- Shared Discourse presentation semantics -*- lexical-binding: t; -*-

;;; Commentary:

;; Width-independent labels shared by collection and topic views.  This module
;; maps canonical Discourse objects to semantic text; Appkit views still own
;; geometry, actions, and lifecycle.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'discourse-state)

(defun discourse-ui--field (object key &optional default)
  "Return OBJECT string KEY or DEFAULT."
  (if (hash-table-p object) (gethash key object default) default))

(defun discourse-ui--tag-name (tag)
  "Return display name from Discourse TAG, or nil."
  (let ((name
         (cond
          ((stringp tag) tag)
          ((hash-table-p tag) (gethash "name" tag)))))
    (and (stringp name)
         (not (string-empty-p name))
         (substring-no-properties name))))

(defun discourse-ui-topic-tags (topic)
  "Return styled tag text for TOPIC."
  (let ((tags
         (condition-case nil
             (discourse-state-sequence-list
              (discourse-ui--field topic "tags"))
           (error nil))))
    (string-join
     (delq nil
           (mapcar
            (lambda (tag)
              (when-let* ((name (discourse-ui--tag-name tag)))
                (propertize (concat "#" name)
                            'face 'font-lock-constant-face)))
            tags))
     "  ")))

(defun discourse-ui-topic-category (topic state)
  "Return TOPIC's category object from canonical STATE, or nil."
  (condition-case nil
      (discourse-state-category
       state (discourse-ui--field topic "category_id"))
    (error nil)))

(defun discourse-ui-topic-category-text (topic state)
  "Return category marker and name for TOPIC from canonical STATE."
  (let* ((category-id
          (condition-case nil
              (discourse-state-id
               (discourse-ui--field topic "category_id"))
            (error "?")))
         (category (discourse-ui-topic-category topic state))
         (name-value (and category (gethash "name" category)))
         (name
          (if (stringp name-value)
              (substring-no-properties name-value)
            (format "Category %s" category-id)))
         (color (and category (gethash "color" category)))
         (marker-face
          (and (stringp color)
               (string-match-p
                "\\`\\(?:[[:xdigit:]]\\{3\\}\\|[[:xdigit:]]\\{6\\}\\)\\'"
                color)
               `(:foreground ,(concat "#" color)))))
    (concat
     (propertize "■" 'face (or marker-face 'shadow))
     " "
     (propertize name 'face 'shadow))))

(defun discourse-ui-topic-unseen-p (topic)
  "Return non-nil when TOPIC is unseen or has new posts."
  (or (eq t (discourse-ui--field topic "unseen"))
      (eq t (discourse-ui--field topic "new_posts"))
      (let ((count (discourse-ui--field topic "unread_posts")))
        (and (integerp count) (> count 0)))))

(defun discourse-ui-topic-status-text (topic &optional include-unseen)
  "Return compact server-backed status text for TOPIC.
INCLUDE-UNSEEN includes the topic's new/unread state."
  (let ((statuses
         (delq nil
               (list
                (and include-unseen
                     (discourse-ui-topic-unseen-p topic)
                     "new")
                (and (eq t (discourse-ui--field topic "pinned"))
                     "pinned")
                (and (eq t (discourse-ui--field topic "closed"))
                     "closed")
                (and (eq t (discourse-ui--field topic "archived"))
                     "archived")
                (and (eq t
                         (discourse-ui--field topic "has_accepted_answer"))
                     "solved")))))
    (if statuses
        (propertize
         (format "[%s] " (string-join statuses ","))
         'face 'font-lock-keyword-face)
      "")))

(provide 'discourse-ui)

;;; discourse-ui.el ends here
