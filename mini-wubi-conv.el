;;; mini-wubi-conv.el  -*- coding: utf-8 -*-

(defun mini-wubi-data-line-to-quail-rules-line (string)
  (let* ((input-line-data (split-string string " " t))
	 (key-seqs (car input-line-data))
	 (trans    (cdr input-line-data))
	 (output-line-data))
    (when (and key-seqs
	       trans)
      (setq output-line-data (cons key-seqs (vconcat trans))))))

(defun mini-wubi-convert-data-file-to-quail-rules-file (filename)
  (interactive "FFind wubi data file: ")
  (with-temp-buffer
    (message "Reading %s..." filename)
    (insert-file-contents filename)
    (let ((input-datas (split-string (buffer-string) "\n" t)))
      (with-temp-file "mini-wubi-rules.el"
	(message "Converting...")
	(insert "(quail-define-rules\n")
	(dolist (output-line-data (mapcar 'mini-wubi-data-line-to-quail-rules-line input-datas))
	  (insert (format "(%-6S %S)" (car output-line-data) (cdr output-line-data)))
	  (insert "\n"))
	(insert ")\n")))
    (message "Finished")))

(provide 'mini-wubi-conv)
;;; mini-wubi-conv.el ends here
