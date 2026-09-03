;;; discourse-evil.el --- Optional Evil integration for discourse.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Ordinary mode maps remain the Emacs-state contract.  This adapter installs
;; deliberate application commands in Evil state maps without replacing native
;; motions, operators, or prefixes.

;;; Code:

(require 'appkit-evil)
(require 'discourse-customize)

(declare-function discourse-topic-list-next
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-compose-topic
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-open-topic
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-previous
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-refresh
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-transient
                  "discourse-transient" ())
(declare-function discourse-topic-list-retry
                  "discourse-topic-list" ())
(declare-function discourse-compose-submit "discourse-compose" ())
(declare-function discourse-compose-cancel "discourse-compose" ())
(declare-function discourse-compose-preview "discourse-compose" ())
(declare-function discourse-topic-compose-reply "discourse-topic" ())
(declare-function discourse-topic-refresh "discourse-topic" ())
(declare-function discourse-topic-open-latest "discourse-topic" ())
(declare-function discourse-topic-jump-back "discourse-topic" ())
(declare-function discourse-topic-retry "discourse-topic" ())
(declare-function discourse-topic-transient "discourse-transient" ())
(declare-function appkit-discussion-next-entry "appkit-discussion" ())
(declare-function appkit-discussion-previous-entry "appkit-discussion" ())
(defvar discourse-compose-mode-map)

(defgroup discourse-evil nil
  "Optional native Evil integration for discourse.el."
  :group 'discourse
  :prefix "discourse-evil-")

(defcustom discourse-evil-enable-integration t
  "If non-nil, install discourse.el Evil bindings automatically."
  :type 'boolean
  :group 'discourse-evil)

(defcustom discourse-evil-initial-state 'normal
  "Initial Evil state for discourse.el application buffers.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'discourse-evil)

(defconst discourse-evil--application-modes
  '(discourse-topic-list-mode discourse-topic-mode)
  "Major modes participating in discourse.el Evil integration.")

(defun discourse-evil--define-topic-list-keys ()
  "Install modal bindings for Discourse topic lists."
  (appkit-evil-define-readonly-keys 'discourse-topic-list-mode-map)
  (appkit-evil-map
    (:map discourse-topic-list-mode-map
     :nm
     "RET" #'discourse-topic-list-open-topic
     "<return>" #'discourse-topic-list-open-topic
     "g r" #'discourse-topic-list-refresh
     "g R" #'discourse-topic-list-retry
     "g j" #'discourse-topic-list-next
     "g k" #'discourse-topic-list-previous
     "?" #'discourse-topic-list-transient))
  (appkit-evil-map
    (:map discourse-topic-list-mode-map
     :nm
     "g c" #'discourse-topic-list-compose-topic)))

(defun discourse-evil--define-topic-keys ()
  "Install modal bindings for Discourse topic streams."
  (appkit-evil-define-readonly-keys 'discourse-topic-mode-map)
  (appkit-evil-map
    (:map discourse-topic-mode-map
     :nm
     "g r" #'discourse-topic-refresh
     "g R" #'discourse-topic-retry
     "g b" #'discourse-topic-open-latest
     "g j" #'appkit-discussion-next-entry
     "g l" #'discourse-topic-jump-back
     "g k" #'appkit-discussion-previous-entry
     "?" #'discourse-topic-transient))
  (appkit-evil-map
    (:map discourse-topic-mode-map
     :nm
     "g c" #'discourse-topic-compose-reply)))

(defun discourse-evil--define-compose-keys ()
  "Install modal bindings for editable Discourse compose buffers."
  (appkit-evil-map
    (:map discourse-compose-mode-map
     :nmi
     "C-c C-c" #'discourse-compose-submit
     "C-c C-k" #'discourse-compose-cancel
     "C-c C-p" #'discourse-compose-preview)))

;;;###autoload
(defun discourse-evil-setup ()
  "Install discourse.el's native Evil integration.
Safe to call multiple times and before Evil is loaded."
  (interactive)
  (when discourse-evil-enable-integration
    (discourse-evil--define-topic-list-keys)
    (discourse-evil--define-topic-keys)
    (discourse-evil--define-compose-keys)
    (when (featurep 'evil)
      (appkit-evil-set-initial-states
       discourse-evil--application-modes discourse-evil-initial-state)
      (appkit-evil-set-initial-states '(discourse-compose-mode) 'insert)
      (appkit-evil-normalize-buffers
       (append discourse-evil--application-modes
               '(discourse-compose-mode))))))

(discourse-evil-setup)

(with-eval-after-load 'evil
  (discourse-evil-setup))

(provide 'discourse-evil)

;;; discourse-evil.el ends here
