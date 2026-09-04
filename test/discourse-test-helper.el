;;; discourse-test-helper.el --- Discourse runtime fixtures -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'discourse-runtime)

(defun discourse-test-drain (account)
  "Drain ACCOUNT's queued Effects and generated Surface work deterministically."
  (let ((app (discourse-account-app account)))
    (when (appkit-app-live-p app)
      (let ((remaining 100) pending)
        (while
            (progn
              (setq pending nil)
              (let ((loops (list (appkit-app-loop app))))
                (maphash (lambda (_identity entry)
                           (push (appkit-surface-loop (cdr entry)) loops))
                         (appkit-app-surfaces app))
                (dolist (loop loops)
                  (when (> (appkit-loop-pending-count loop) 0)
                    (setq pending t)
                    (appkit-loop-run-pass loop))
                  (when (eq (appkit-loop-status loop) 'faulted)
                    (error "Discourse fixture loop fault: %S" (appkit-loop-fault loop)))))
              (when (and pending (<= (cl-decf remaining) 0))
                (error "Discourse fixture did not quiesce"))
              pending))))))

(provide 'discourse-test-helper)
;;; discourse-test-helper.el ends here
