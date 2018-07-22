(require 'f)

(defvar mini-wubi-support-path
  (f-dirname load-file-name))

(defvar mini-wubi-features-path
  (f-parent mini-wubi-support-path))

(defvar mini-wubi-root-path
  (f-parent mini-wubi-features-path))

(add-to-list 'load-path mini-wubi-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'mini-wubi)
  (require 'espuds)
  (require 'ert)
  (require 'mini-wubi-popup))

(Setup
 ;; Before anything has run
 (when (and (>= emacs-major-version 25))
   (require 'cl-preloaded)
   (setf (symbol-function 'cl--assertion-failed)
         (lambda (form &optional string sargs args)
           "This function has been modified by ecukes to fix problems with cl-assert in emacs 25.
           The modified version should only be used for running espuds tests."
           (if string
               (apply #'error string (append sargs args))
             (signal 'cl-assertion-failed `(,form ,@sargs))))))

 (setq default-input-method "mini-wubi")
 (setq mini-wubi-display-function 'mini-wubi-popup-show-selectlist))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
