(require 'f)

(defvar widget-mvc-support-path
  (f-dirname load-file-name))

(defvar widget-mvc-features-path
  (f-parent widget-mvc-support-path))

(defvar widget-mvc-root-path
  (f-parent widget-mvc-features-path))

(add-to-list 'load-path widget-mvc-root-path)

(require 'undercover)
(undercover "*.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
(require 'widget-mvc)
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
