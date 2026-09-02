;; -*- mode: eask; lexical-binding: t -*-

(package "discourse"
         "0.1.0"
         "Appkit-based Discourse client")

(keywords "comm")

(package-file "discourse.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "31.1")
(depends-on "plz" "0.8")
(depends-on "transient" "0.7")
(depends-on "appkit" :file "../appkit-apps/appkit.el")

(development
 (depends-on "evil"))

(script "refresh-appkit" "eask -f install-file ../appkit-apps/appkit.el")
(script "test" "eask test ert test/*.el --quiet")
(script "test-local"
        "eask -f install-file ../appkit-apps/appkit.el && eask recompile && eask test ert test/*.el --quiet")
