(require 'f)

(defvar emacs-widget-mvc-support-path
  (f-dirname load-file-name))

(defvar emacs-widget-mvc-features-path
  (f-parent emacs-widget-mvc-support-path))

(defvar emacs-widget-mvc-root-path
  (f-parent emacs-widget-mvc-features-path))

(add-to-list 'load-path emacs-widget-mvc-root-path)

(require 'emacs-widget-mvc)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
