;;; mini-wubi-conv.el --- convert plan text file  to quail rules file -*- lexical-binding: t; -*-
;;; -*- coding: utf-8 -*-

;; Copyright (C) 2017 SuJiKiNen

;; Author: SuJiKiNen <SuJiKiNen@gmail.com>
;; Keywords:

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

;;

;;; Code:

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
