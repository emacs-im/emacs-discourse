;;; discourse-state.el --- Canonical Discourse domain state -*- lexical-binding: t; -*-

;;; Commentary:

;; App-owned canonical topic and post observations.  Remote identifiers remain
;; decimal strings at every state and view boundary.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (discourse-state
               (:constructor discourse-state--create)
               (:copier nil))
  revision
  topics
  posts
  users
  categories
  categories-loaded-p
  site-profile)

(defun discourse-state-create ()
  "Return fresh canonical Discourse state."
  (discourse-state--create
   :revision 0
   :topics (make-hash-table :test #'equal)
   :posts (make-hash-table :test #'equal)
   :users (make-hash-table :test #'equal)
   :categories (make-hash-table :test #'equal)
   :categories-loaded-p nil
   :site-profile nil))

(defun discourse-state-id (value)
  "Return positive remote identifier VALUE as a decimal string."
  (let ((text
         (cond
          ((and (integerp value) (> value 0)) (number-to-string value))
          ((stringp value) (substring-no-properties value))
          (t nil))))
    (unless (and text (string-match-p "\\`[1-9][0-9]*\\'" text))
      (error "Invalid Discourse remote ID"))
    text))

(defun discourse-state-object-get (object key &optional default)
  "Return OBJECT's string KEY, or DEFAULT when absent."
  (unless (hash-table-p object)
    (error "Invalid Discourse object"))
  (gethash key object default))

(defun discourse-state-sequence-list (value)
  "Return JSON sequence VALUE as an owned list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((proper-list-p value) (copy-sequence value))
   (t (error "Invalid Discourse JSON sequence"))))

(defun discourse-state--merge-object (table object)
  "Merge hash-table OBJECT into canonical TABLE and return its ID."
  (unless (hash-table-p object)
    (error "Invalid Discourse domain object"))
  (let* ((id (discourse-state-id (gethash "id" object)))
         (current (gethash id table))
         (merged (if (hash-table-p current)
                     (copy-hash-table current)
                   (make-hash-table :test #'equal))))
    (maphash (lambda (key value) (puthash key value merged)) object)
    (puthash id merged table)
    id))

(defun discourse-state-merge-topic (state topic)
  "Merge TOPIC into canonical STATE and return its ID."
  (unless (discourse-state-p state)
    (error "Invalid Discourse state"))
  (prog1 (discourse-state--merge-object
          (discourse-state-topics state) topic)
    (cl-incf (discourse-state-revision state))))

(defun discourse-state-merge-post (state post)
  "Merge POST into canonical STATE and return its ID."
  (unless (discourse-state-p state)
    (error "Invalid Discourse state"))
  (prog1 (discourse-state--merge-object
          (discourse-state-posts state) post)
    (cl-incf (discourse-state-revision state))))

(defun discourse-state-merge-user (state user)
  "Merge USER into canonical STATE and return its ID."
  (unless (discourse-state-p state)
    (error "Invalid Discourse state"))
  (prog1 (discourse-state--merge-object
          (discourse-state-users state) user)
    (cl-incf (discourse-state-revision state))))

(defun discourse-state--merge-category-tree (table category)
  "Merge CATEGORY and nested subcategories into canonical TABLE."
  (discourse-state--merge-object table category)
  (dolist (subcategory
           (discourse-state-sequence-list
            (gethash "subcategory_list" category)))
    (unless (hash-table-p subcategory)
      (error "Invalid Discourse subcategory object"))
    (discourse-state--merge-category-tree table subcategory)))

(defun discourse-state-merge-categories (state categories)
  "Replace STATE's observed category catalog with CATEGORIES."
  (unless (discourse-state-p state)
    (error "Invalid Discourse state"))
  (let ((table (make-hash-table :test #'equal)))
    (dolist (category (discourse-state-sequence-list categories))
      (unless (hash-table-p category)
        (error "Invalid Discourse category object"))
      (discourse-state--merge-category-tree table category))
    (setf (discourse-state-categories state) table
          (discourse-state-categories-loaded-p state) t)
    (cl-incf (discourse-state-revision state))
    table))

(defun discourse-state-set-site-profile (state profile)
  "Install string-keyed site PROFILE in canonical STATE."
  (unless (and (discourse-state-p state) (hash-table-p profile))
    (error "Invalid Discourse site profile"))
  (setf (discourse-state-site-profile state) (copy-hash-table profile))
  (cl-incf (discourse-state-revision state))
  (discourse-state-site-profile state))

(defun discourse-state-topic (state topic-id)
  "Return STATE's canonical TOPIC-ID observation, or nil."
  (gethash (discourse-state-id topic-id) (discourse-state-topics state)))

(defun discourse-state-post (state post-id)
  "Return STATE's canonical POST-ID observation, or nil."
  (gethash (discourse-state-id post-id) (discourse-state-posts state)))

(defun discourse-state-user (state user-id)
  "Return STATE's canonical USER-ID observation, or nil."
  (gethash (discourse-state-id user-id) (discourse-state-users state)))

(defun discourse-state-category (state category-id)
  "Return STATE's canonical CATEGORY-ID observation, or nil."
  (gethash (discourse-state-id category-id)
           (discourse-state-categories state)))

(provide 'discourse-state)

;;; discourse-state.el ends here
