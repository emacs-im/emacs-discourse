;;; discourse-evil.el --- Optional Evil integration for discourse.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Ordinary mode maps remain the Emacs-state contract.  This adapter installs
;; deliberate application commands in Evil state maps without replacing native
;; motions, operators, or prefixes.

;;; Code:

(require 'appkit-evil)
(require 'discourse-customize)

(declare-function discourse-topic-list-load-more
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-next
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-open-topic
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-previous
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-refresh
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-transient
                  "discourse-transient" ())
(declare-function discourse-topic-load-more "discourse-topic" ())
(declare-function discourse-topic-refresh "discourse-topic" ())
(declare-function discourse-topic-transient "discourse-transient" ())

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
     "g ]" #'discourse-topic-list-load-more
     "g j" #'discourse-topic-list-next
     "g k" #'discourse-topic-list-previous
     "?" #'discourse-topic-list-transient)))

(defun discourse-evil--define-topic-keys ()
  "Install modal bindings for Discourse topic streams."
  (appkit-evil-define-readonly-keys 'discourse-topic-mode-map)
  (appkit-evil-map
    (:map discourse-topic-mode-map
     :nm
     "g r" #'discourse-topic-refresh
     "g ]" #'discourse-topic-load-more
     "g j" #'appkit-discussion-next-entry
     "g k" #'appkit-discussion-previous-entry
     "?" #'discourse-topic-transient)))

;;;###autoload
(defun discourse-evil-setup ()
  "Install discourse.el's native Evil integration.
Safe to call multiple times and before Evil is loaded."
  (interactive)
  (when discourse-evil-enable-integration
    (discourse-evil--define-topic-list-keys)
    (discourse-evil--define-topic-keys)
    (when (featurep 'evil)
      (appkit-evil-set-initial-states
       discourse-evil--application-modes discourse-evil-initial-state)
      (appkit-evil-normalize-buffers discourse-evil--application-modes))))

(discourse-evil-setup)

(with-eval-after-load 'evil
  (discourse-evil-setup))

(provide 'discourse-evil)

;;; discourse-evil.el ends here
