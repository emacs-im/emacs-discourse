;; -*- mode: eask; lexical-binding: t -*-

(package "discourse"
         "0.1.0"
         "Appkit-based Discourse client")

(keywords "comm")

(package-file "discourse.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "32.0")
(depends-on "plz" "0.8")
(depends-on "transient" "0.7")
(depends-on "browser-session" :file
            (expand-file-name "../browser-session/browser-session.el"
                              (file-truename default-directory)))
(depends-on "video" :file
            (expand-file-name "../video.el"
                              (file-truename default-directory)))
(depends-on "appkit" :file
            (expand-file-name "../appkit.el"
                              (file-truename default-directory)))

(development
 (depends-on "evil"))

(script "refresh-appkit"
        "(cd ../appkit.el && eask -f install-file ../video.el) && eask -f install-file ../appkit.el")
(script "test" "eask test ert test/*.el --quiet")
(script "test-local"
        "eask -f install-file ../browser-session/browser-session.el && (cd ../appkit.el && eask -f install-file ../video.el) && eask -f install-file ../appkit.el && eask recompile && eask test ert test/*.el --quiet")
