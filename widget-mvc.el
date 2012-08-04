;;; widget-mvc.el --- MVC framework for the emacs widgets

;; Copyright (C) 2012  SAKURAI Masashi

;; Author:  <m.sakurai at kiwanami.net>
;; Keywords: lisp, widget

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * Overview
;; This library provides following functions:
;;  - Widget Template
;;    - template helpers
;;  - Data Binding
;;  - Validation
;;  - Action Mapping
;;  - Session
;; 
;; * Widget Template
;; 
;; Sample:
;; 
;; Syntax:
;; 

;; Todo
;;   layout
;;   component
;;   radio
;;   user-action

;;; Code:

(eval-when-compile (require 'cl))
(require 'widget)


;;; Utilities

(defun wmvc:get-new-buffer (&optional buffer-name)
  "[internal] Create and return a buffer object.
This function kills the old buffer if it exists."
  (unless buffer-name (setq buffer-name wmvc:default-buffer-name))
  (let ((buf (get-buffer buffer-name)))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf)))
  (get-buffer-create buffer-name))

(defvar wmvc:default-buffer-name "*wmvc-buffer*" "[internal] Default buffer name.")

;;; MVC Context

(defstruct wmvc:context template model validations widget-map action-map attributes)

(defun wmvc:context-attr-set (context name value)
  (let ((attrs (wmvc:context-attributes context)))
    (cond
     ((assq name attrs)
      (let ((pair (assq name attrs)))
        (setf (cdr pair) value)))
     (t (setq attrs (cons (cons name value) attrs))))
    (setf (wmvc:context-attributes context) attrs)))

(defun wmvc:context-attr-get (context name)
  (let ((attrs (wmvc:context-attributes context)))
    (cdr (assq name attrs))))

(defun wmvc:context-widget-map-add (context name widget)
  (let ((widget-map (cons (cons name widget) 
                          (wmvc:context-widget-map context))))
    (setf (wmvc:context-widget-map context) widget-map)))


;;; Template

(defun wmvc:tmpl-build-buffer(buffer context)
  (let ((tmpl-src (wmvc:context-template context))
        (model (wmvc:context-model context)))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (loop for elm in tmpl-src
            do
            (cond
             ((consp elm)
              (wmvc:tmpl-make-widget elm context))
             ((stringp elm)
              (widget-insert elm))
             ((eq elm 'BR)
              (widget-insert "\n"))
             ((not elm) nil)
             (t (error "Found a wrong template element : %s" elm))))
      (wmvc:bind-from-model context)
      (use-local-map widget-keymap)
      (widget-setup)
      (set (make-local-variable 'wmvc:context) context)
      (goto-char (point-min))
      (widget-forward 1))
    (wmvc:context-attr-set context 'error nil)
    buffer))

(defun wmvc:tmpl-make-widget (elm context)
  "[internal] "
  (let* ((elm-type (car elm))
         (elm-plist (cdr elm)))
    (case elm-type
     ('input (wmvc:tmpl-make-widget-input elm-plist context))
     ('button (wmvc:tmpl-make-widget-button elm-plist context))
     ('message (wmvc:tmpl-make-widget-message elm-plist context)))))

(defun wmvc:tmpl-make-widget-input (elm-plist context)
  (let* ((type (plist-get elm-plist ':type)) 
         (name (plist-get elm-plist ':name))
         (widget
          (case type
            ('text 
             (wmvc:tmpl-make-widget-input-text elm-plist))
            ('password
             (wmvc:tmpl-make-widget-input-password elm-plist))
            ('checkbox
             (wmvc:tmpl-make-widget-input-checkbox elm-plist))
            ('select
             (wmvc:tmpl-make-widget-input-select elm-plist))
            (t (error "Unknown input type : %s" type)))))
    (wmvc:context-widget-map-add context name widget)))

(defun wmvc:tmpl-make-widget-input-text (elm-plist)
  (let ((size (plist-get elm-plist ':size))
        (format (plist-get elm-plist ':format)))
    (apply 'widget-create
           (append
            '(editable-field)
            (if size (list :size size))
            (if format (list :format format))))))

(defun wmvc:tmpl-make-widget-input-password (elm-plist)
  (let ((size (plist-get elm-plist ':size))
        (format (plist-get elm-plist ':format)))
    (apply 'widget-create
           (append
            '(editable-field :secret ?*)
            (if size (list :size size))
            (if format (list :format format))))))

(defun wmvc:tmpl-make-widget-input-checkbox (elm-plist)
  (widget-create 'checkbox))

(defun wmvc:tmpl-make-widget-input-select (elm-plist)
  (let ((options (plist-get elm-plist ':options))
        (title   (or (plist-get elm-plist ':title) "select"))
        (format (or (plist-get elm-plist ':format) "%[%t%] : %v"))
        (help-echo (or (plist-get elm-plist ':help-echo) "Click to choose")))
    (widget-create
     'menu-choice
     :format format :tag title :help-echo help-echo
     :args
     (cond
      ((consp (car options))
       (loop for i in options
             for (item-title . value) = i
             collect
             (list 'item ':tag item-title ':value value)))
      (t
       (loop for i in options
             collect
             (list 'item ':tag (format "%s" i) ':value i)))))))

(defun wmvc:tmpl-make-widget-button (elm-plist context)
  (lexical-let* ((action (plist-get elm-plist ':action))
                 (need-validation (plist-get elm-plist ':validation))
                 (widget
                  (widget-create
                   'push-button
                   :notify (lambda (&rest ignore) 
                             (wmvc:action-invoke action need-validation))
                   (plist-get elm-plist ':title))))
    (wmvc:context-widget-map-add context action widget)))

(defun wmvc:tmpl-make-widget-message (elm-plist context)
  (let* ((key (plist-get elm-plist ':key)) 
         (face (plist-get elm-plist ':face))
         (msg (wmvc:context-attr-get context key)))
    (when msg
      (widget-insert
       (if face (propertize msg 'face face) msg)))))

;;; binding / validation

(defun wmvc:bind-from-model (context)
  "[internal] "
  (loop with widget-map = (wmvc:context-widget-map context)
        with model = (wmvc:context-model context)
        for (name . widget) in widget-map
        for (fname . data) = (assq name model)
        if fname
        do 
        (widget-value-set widget data))
  (widget-setup))

(defun wmvc:bind-from-widgets (context)
  (loop with widget-map = (wmvc:context-widget-map context)
        with model = (wmvc:context-model context)
        for (name . widget) in widget-map
        for pair = (assq name model)
        if pair
        do
        (setf (cdr pair) (widget-value widget))
        finally return model))

(defun wmvc:validate-fields (model)
  (let* ((ctx wmvc:context)
         (validations (wmvc:context-validations ctx))
         (fails 
          (loop for (name . validation) in validations
                for value = (cdr (assq name model))
                for result = (funcall validation value)
                if result
                collect result)))
    (when fails
      (wmvc:context-attr-set ctx 'error (mapconcat 'identify fails "\n"))
      (wmvc:reload-buffer ctx)
      (throw 'fail nil))))


;;; action

(defun wmvc:action-invoke (action-name need-validation)
  "[internal] "
  (let* ((ctx wmvc:context)
         (action-map (wmvc:context-action-map ctx))
         (action (or (cdr (assq action-name action-map)) action-name))
         (xmodel (wmvc:bind-from-widgets ctx)))
    (catch 'fail
      (when need-validation
        (wmvc:validate-fields xmodel))
      (funcall action xmodel))))


;;; controller

(defun wmvc:reload-buffer (context)
  (let* ((win (get-buffer-window)) 
         (new-buf (wmvc:rebuild-buffer context)))
    (cond
     ((or (null win) (not (window-live-p win)))
      (pop-to-buffer new-buf))
     (t
      (set-window-buffer win new-buf)
      (set-buffer new-buf)))
    new-buf))

(defun wmvc:rebuild-buffer (context)
  (let ((buf-name (buffer-name))
        (pos (point))
        (buffer (wmvc:get-new-buffer buf-name)))
    (wmvc:tmpl-build-buffer buffer context)
    buffer))

(defun* wmvc:build-buffer(&key buffer tmpl model actions validations attributes)
  (let ((context
         (make-wmvc:context 
          :template tmpl :model model 
          :action-map actions :validations validations
          :attributes attributes)))
    (unless buffer
      (setq buffer (wmvc:get-new-buffer)))
    (wmvc:tmpl-build-buffer buffer context)
    buffer))


;;; debug

(defun wmvc:demo-template ()
  (interactive)
  (let ((src `(
               ,(propertize "Form Sample" 'face 'info-title-1) BR
               "This is a sample template.\nA normal text is inserted as is." BR
               "BR inserts a line break." BR BR
               (message :name error :face compilation-error) BR
               "  Input A  : "
               (input :name input-a :type text :size 30) BR
               "  Input B  : "
               (input :name input-b :type text :size 30) BR
               "  Password : "
               (input :name password :type password :size 20) BR
               "  Option   : "
               "Alpha" (input :name check-a :type checkbox) " "
               "Beta"  (input :name check-b :type checkbox) " "
               "Gamma" (input :name check-c :type checkbox) BR
               "  Select1  : "
               (input :name select1 :type select 
                      :options ("select1" "select2" "select3" "select4"))
               BR
               "  Select2  : "
               (input :name select2 :type select 
                      :options (("select1" . 1) ("select2" . 2) ("select3" . 3) ("select4" . 4)))
               BR BR
               "    " (button :title "OK" :action on-submit :validation t)
               "  " (button :title "Cancel" :action on-cancel)))
        (model 
         '((input-a . "")  (input-b . "initial value")
           (password . "") (check-a . t) (check-b . nil) (check-c . nil)
           (select1 . "select2") (select2 . 3)))
        (validations '())
        (action-mapping 
         '((on-submit . wmvc:demo-submit-action)
           (on-cancel . (lambda (model) 
                          (message "canceled")
                          (kill-this-buffer)))))
        (attributes '()))
    (pop-to-buffer
     (wmvc:build-buffer 
      :buffer (wmvc:get-new-buffer)
      :tmpl src :model model :actions action-mapping
      :validations validations :attributes attributes))))

(defun wmvc:demo-submit-action (model)
  (message "MODEL : %S" model)
  (kill-this-buffer))

;; (progn (eval-current-buffer) (wmvc:demo-template))

(provide 'widget-mvc)
;;; widget-mvc.el ends here
