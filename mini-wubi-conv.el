;;; mini-wubi-conv.el  -*- coding: utf-8 -*-
(defconst mini-wubi-rules-save-dir (file-name-directory (or load-file-name buffer-file-name)))
(defconst mini-wubi-rules-filename (expand-file-name "mini-wubi-rules.el" (file-name-as-directory mini-wubi-rules-save-dir)))

;;;###autoload
(defun mini-wubi-data-line-to-quail-rules-line (string)
  (let* ((input-line-data (split-string string " " t))
	 (key-seqs (car input-line-data))
	 (trans    (cdr input-line-data))
	 (output-line-data))
    (when (and key-seqs
	       trans)
      (setq output-line-data (cons key-seqs (vconcat trans))))))

;;;###autoload
(defun mini-wubi-convert-data-file-to-quail-rules-file ()
  (interactive)
  (let ((default-directory mini-wubi-rules-save-dir))
    (call-interactively
     (lambda (filename)
       (interactive "fFind wubi data file: ")
       (with-temp-buffer
         (message "Reading %s ..." filename)
         (insert-file-contents filename)
         (let ((input-datas (split-string (buffer-string) "\n" t)))
           (with-temp-file mini-wubi-rules-filename
	           (message "Converting to %s ..." mini-wubi-rules-filename)
	           (insert ";; -*-no-byte-compile: t; -*-\n")
	           (insert "(quail-define-rules\n")
	           (dolist (output-line-data (mapcar 'mini-wubi-data-line-to-quail-rules-line input-datas))
	             (insert (format "(%-6S %S)" (car output-line-data) (cdr output-line-data)))
	             (insert "\n"))
	           (insert ")\n")))
         (message "Finished"))))))

(provide 'mini-wubi-conv)

;;; mini-wubi-conv.el ends here
