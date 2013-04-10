# Widget MVC

This program is a GUI framework for emacs lisp.
This framework is designed for the programmers who is familiar with the conventional Web MVC frameworks.

The current status is under construction.

## Applications

Todo

## Sample Code

```lisp
(defun wmvc:demo ()
  (interactive)
  (let ((src `(
               ,(propertize "Form Sample" 'face 'info-title-1) BR
               "This is a sample template.\nA normal text is inserted as is." BR
               "BR inserts a line break." BR BR
               (message :name error :face compilation-error) BR
               "  Input A  : "
               (input :name input-a :type text :size 30) BR
               "  Input B  : "
               (input :name input-b :type text :size 30) "(decimal 0 - 12)" BR
               "  Password : "
               (input :name password :type password :size 20) BR
               "  Option   : "
               "Alpha" (input :name check-a :type checkbox) " "
               "Beta"  (input :name check-b :type checkbox) " "
               "Gamma" (input :name check-c :type checkbox) BR
               "  Radio Select : " 
               (input :name radio-a :type radio
                      :options (("select1" . 1) ("select2" . 2) ("select3" . 3) ("select4" . 4)))
               BR
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
         '((input-a . "")  (input-b . "6")
           (password . "") (check-a . t) (check-b . nil) (check-c . nil)
           (radio-a . 4) (select1 . "select2") (select2 . 3)))
        (validations
         '((input-a . wmvc:validation-integer)
           (input-b . (wmvc:validation-decimal :min 0 :max 12))
           (password . (wmvc:validation-length :min 5 :max 10))
           ))
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


;; M-x wmvc:demo
```

# Installation

## Manual installation

Place those programs and this one (widget-mvc.el) in your load path and add following code.

```lisp
(require 'widget-mvc)
```

# API Document

Todo

### API Overview

Todo

![Lifecycle Overview](https://cacoo.com/diagrams/O42ZDzbVzZx31o7e-EFE0F.png)

### Details

Todo

# License

EPC is licensed under GPL v3.

----
(C) 2013 SAKURAI Masashi. m.sakurai at kiwanami.net
