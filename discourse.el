;;; discourse.el --- Appkit-based Discourse client -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (appkit "0.3.0") (plz "0.8") (transient "0.7"))
;; Keywords: comm

;;; Commentary:

;; Anonymous Discourse reading through Appkit-owned applications, stable topic
;; and post projections, and native Appkit semantic markup insertion.

;;; Code:

(require 'discourse-customize)
(require 'discourse-runtime)
(require 'discourse-topic-list)
(require 'discourse-topic)
(require 'discourse-transient)
(require 'discourse-evil)

(defconst discourse-version "0.1.0"
  "Current discourse.el package version.")

;;;###autoload
(defun discourse (&optional origin)
  "Open the Latest view for Discourse HTTPS ORIGIN."
  (interactive
   (list
    (if current-prefix-arg
        (read-string "Discourse HTTPS origin: " discourse-default-origin)
      discourse-default-origin)))
  (let ((account
         (discourse-runtime-create-account
          (or origin discourse-default-origin))))
    (discourse-topic-list-open-latest account t)))

;;;###autoload
(defun discourse-open-topic (topic-id &optional origin)
  "Open TOPIC-ID from Discourse HTTPS ORIGIN."
  (interactive
   (list (read-string "Discourse topic ID: ")
         (if current-prefix-arg
             (read-string "Discourse HTTPS origin: " discourse-default-origin)
           discourse-default-origin)))
  (let ((account
         (discourse-runtime-create-account
          (or origin discourse-default-origin))))
    (discourse-topic-open account topic-id t)))

;;;###autoload
(defun discourse-disconnect (&optional origin)
  "Stop the anonymous application for Discourse ORIGIN."
  (interactive
   (list (read-string "Discourse HTTPS origin: " discourse-default-origin)))
  (if-let* ((account
             (discourse-runtime-account
              (or origin discourse-default-origin))))
      (discourse-runtime-stop-account account)
    (user-error "No live anonymous Discourse application for this origin")))

(provide 'discourse)

;;; discourse.el ends here
