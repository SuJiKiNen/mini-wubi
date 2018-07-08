;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I switch to a clean buffer \"\\([^\"]+\\)\"$"
       (lambda (buffer-name)
         (if (get-buffer buffer-name)
             (kill-buffer buffer-name))
         (switch-to-buffer (get-buffer-create buffer-name))))


(Given "^I activate input method$"
       (lambda ()
         (toggle-input-method)
         (let ((msg "Expected current input method is '%s', but '%s'")
               (excepted-input-method mini-wubi-name))
           (cl-assert (equal excepted-input-method current-input-method)
                      nil msg excepted-input-method current-input-method))))
