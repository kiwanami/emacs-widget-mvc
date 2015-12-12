;;; test-helper --- Test helper for emacs-widget-mvc

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar emacs-widget-mvc-test-path
  (f-dirname (f-this-file)))

(defvar emacs-widget-mvc-root-path
  (f-parent emacs-widget-mvc-test-path))

(defvar emacs-widget-mvc-sandbox-path
  (f-expand "sandbox" emacs-widget-mvc-test-path))

(when (f-exists? emacs-widget-mvc-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" emacs-widget-mvc-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory emacs-widget-mvc-sandbox-path))
     (when (f-exists? emacs-widget-mvc-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir emacs-widget-mvc-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl))
(require 'widget-mvc)

(provide 'test-helper)
;;; test-helper.el ends here
