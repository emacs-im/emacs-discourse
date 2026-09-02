;;; discourse-transient.el --- Discoverable discourse.el actions -*- lexical-binding: t; -*-

;;; Commentary:

;; Context-specific read actions for topic lists and topic streams.  Permission
;; controlled write suffixes will join these prefixes when authentication lands.

;;; Code:

(require 'transient)

(declare-function discourse-topic-list-next
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-open-topic
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-previous
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-refresh
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-retry
                  "discourse-topic-list" ())
(declare-function discourse-topic-list-retry-available-p
                  "discourse-topic-list" ())
(declare-function discourse-topic-refresh "discourse-topic" ())
(declare-function discourse-topic-open-latest "discourse-topic" ())
(declare-function discourse-topic-jump-back "discourse-topic" ())
(declare-function discourse-topic-retry "discourse-topic" ())
(declare-function discourse-topic-retry-available-p
                  "discourse-topic" ())
(declare-function appkit-discussion-next-entry "appkit-discussion" ())
(declare-function appkit-discussion-previous-entry "appkit-discussion" ())

;;;###autoload(autoload 'discourse-topic-list-transient "discourse-transient" nil t)
(transient-define-prefix discourse-topic-list-transient ()
  "Actions for the current Discourse topic list."
  [["Navigate"
    ("o" "Open topic" discourse-topic-list-open-topic)
    ("n" "Next topic" discourse-topic-list-next :transient t)
    ("p" "Previous topic" discourse-topic-list-previous :transient t)]
   ["Network"
    ("g" "Refresh" discourse-topic-list-refresh)
    ("R" "Retry failed request" discourse-topic-list-retry
     :if discourse-topic-list-retry-available-p)]])

;;;###autoload(autoload 'discourse-topic-transient "discourse-transient" nil t)
(transient-define-prefix discourse-topic-transient ()
  "Actions for the current Discourse topic stream."
  [["Navigate"
    ("b" "Latest" discourse-topic-open-latest)
    ("l" "Previous anchor" discourse-topic-jump-back)
    ("n" "Next post" appkit-discussion-next-entry :transient t)
    ("p" "Previous post" appkit-discussion-previous-entry :transient t)]
   ["Network"
    ("g" "Refresh" discourse-topic-refresh)
    ("R" "Retry failed request" discourse-topic-retry
     :if discourse-topic-retry-available-p)]])

(provide 'discourse-transient)

;;; discourse-transient.el ends here
