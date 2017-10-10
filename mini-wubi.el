;;; mini-wubi.el --- A simple Chinese wubi input method inside Emacs -*- lexical-binding: t; -*-
;;; -*- coding: utf-8 -*-

;; Copyright (C) 2017 SuJiKiNen

;; Author: SuJiKiNen <SuJiKiNen@gmail.com>
;; URL: https://github.com/SuJiKiNen/mini-wubi
;; Keywords:i18n
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (popup "0.5.3"))

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
;; mini-wubi is a simple Chinese wubi input method inside Emacs.
;;

;;; Code:

(require 'quail)
(require 'popup)

;; mini-wubi quail settings
(defconst mini-wubi-name "mini-wubi")
(defconst mini-wubi-lang "euc-cn")
(defconst mini-wubi-title "迷雾")
(defconst mini-wubi-doc-string "A simple Chinese wubi input method inside Emacs")
(defconst mini-wubi-version "0.1.0")
;;quail define package settings,I don't like using defcustom
(defvar mini-wubi-guidance t)
(defvar mini-wubi-trans-keys
  '((" " . quail-select-current)))
(defvar mini-wubi-forget-last-selection nil)
(defvar mini-wubi-deterministic nil)
(defvar mini-wubi-use-kbd-translate t)
(defvar mini-wubi-show-layout nil)
(defvar mini-wubi-create-decode-map nil)
(defvar mini-wubi-maximum-shortest nil)
(defvar mini-wubi-overylay-plist nil)
(defvar mini-wubi-upadte-translation-fn nil)
(defvar mini-wubi-conversion-keys nil)
(defvar mini-wubi-simple nil)

(defvar mini-wubi-rules-loaded-flag nil "flag that tell whether mini-wubi-rules loaded or not.")

(defconst mini-wubi-lang-states '("cn" "eng") "mini wubi input states,cn is short for Chinese,eng for English")
(defconst mini-wubi-width-states '("half" "full" "some characters width states,fullwidth or halfwidth"))

(defvar mini-wubi-current-lang-state (car mini-wubi-lang-states))
(defvar mini-wubi-current-width-state (car mini-wubi-width-states))

(defvar mini-wubi-lang-eng-state-indicator "$")
(defvar mini-wubi-lang-cn-state-indicator "¥")
(defvar mini-wubi-halfwidth-state-indicator "◑")
(defvar mini-wubi-fullwidth-state-indicator "●")

(defface mini-wubi-popup-selection-face
  '((default :foreground "black")
    (((class color) (min-colors 88) (background light))
     (:background "cornsilk")))
  "Face used for the selection in the popup.")

(defface mini-wubi-popup-face
  '((default :foreground "color-240")
    (((class color) (min-colors 88) (background light))
     (:background "light blue")))
  "Face used for the popup background.")

(defun mini-wubi-halfwidth-state ()
  (car mini-wubi-width-states))

(defun mini-wubi-fullwidth-state ()
  (cdr mini-wubi-width-states))

(defun mini-wubi-lang-cn-state ()
  (car mini-wubi-lang-states))

(defun mini-wubi-lang-eng-state ()
  (cdr mini-wubi-lang-states))

(defun mini-wubi-in-halfwidth ()
  (equal
   mini-wubi-current-width-state
   (car mini-wubi-width-states)))

(defun mini-wubi-in-fullwidth ()
  (equal
   mini-wubi-current-width-state
   (cdr mini-wubi-width-states)))

(defun mini-wubi-in-lang-eng-state()
  (equal mini-wubi-current-lang-state (mini-wubi-lang-eng-state)))

(defun mini-wubi-in-lang-cn-state()
  (equal mini-wubi-current-lang-state (mini-wubi-lang-cn-state)))

(defun mini-wubi-current-lang-indicator (lang)
  (if (equal lang (mini-wubi-lang-eng-state))
      mini-wubi-lang-eng-state-indicator
    mini-wubi-lang-cn-state-indicator))

(defun mini-wubi-current-width-indicator (char-width)
  (if (equal char-width (mini-wubi-halfwidth-state))
      mini-wubi-halfwidth-state-indicator
    mini-wubi-fullwidth-state-indicator))

(defun mini-wubi-mode-line-text ()
  (format "%s %s %s"
          (mini-wubi-current-lang-indicator mini-wubi-current-lang-state)
          (mini-wubi-current-width-indicator mini-wubi-current-width-state)
          mini-wubi-title))

(defvar mini-wubi-mode-line-indicator (mini-wubi-mode-line-text))

(defun mini-wubi-update-mode-line-indicator ()
  (setq mini-wubi-mode-line-indicator (mini-wubi-mode-line-text))
  (setq current-input-method-title mini-wubi-mode-line-indicator) ;; does this has side effect?
  (force-mode-line-update))

(defvar mini-wubi-eng-quail-map '(nil (nil nil)))
(defvar mini-wubi-cn-quail-map nil)

;; use M-x sort-lines to set the order of the list
(defvar mini-wubi-width-characters-alist
  '(
    ("!" "！")
    ("#" "＃")
    ("$" "￥")
    ("%" "％")
    ("&" "＆")
    ("(" "（")
    (")" "）")
    ("*" "×")
    ("+" "＋")
    ("," "，")
    ("-" "－")
    ("." "。")
    ("/" "、")
    (":" "：")
    (";" "；")
    ("<" "《")
    ("=" "＝")
    (">" "》")
    ("?" "？")
    ("@" "＠")
    ("[" "［")
    ("\\" "＼")
    ("]" "］")
    ("{" "｛")
    ("|" "｜")
    ("}" "｝")
    ("~" "～")
    ))

(defun mini-wubi-activated ()
  (and
   current-input-method
   (equal (quail-name) mini-wubi-name)))

(defun mini-wubi-switch-to-lang-state(lang-state)
  (if (equal lang-state (mini-wubi-lang-cn-state))
      (progn
        (setq mini-wubi-current-lang-state (mini-wubi-lang-cn-state))
        (quail-install-map mini-wubi-cn-quail-map))
    (progn
      (setq mini-wubi-current-lang-state (mini-wubi-lang-eng-state))
      (quail-install-map mini-wubi-eng-quail-map))))

(defun mini-wubi-switch-lang-state ()
  (interactive)
  (if (mini-wubi-in-lang-cn-state)
      (progn
        (mini-wubi-switch-to-lang-state (mini-wubi-lang-eng-state))
        (message "input method in english state now!"))
    (progn
      (mini-wubi-switch-to-lang-state (mini-wubi-lang-cn-state))
      (message "input method in chinese state now!")))
  (mini-wubi-update-mode-line-indicator))

(defun mini-wubi-switch-character-width ()
  (interactive)
  (when (mini-wubi-activated)
    (if (mini-wubi-in-halfwidth)
        (progn
          (mini-wubi-remap-character-width (mini-wubi-fullwidth-state))
          (setq mini-wubi-current-width-state (mini-wubi-fullwidth-state)))
      (progn
        (mini-wubi-remap-character-width (mini-wubi-halfwidth-state))
        (setq mini-wubi-current-width-state (mini-wubi-halfwidth-state))))
    (mini-wubi-update-mode-line-indicator)))

(defun mini-wubi-remap-character-width (char-width)
  (mapcar (lambda (convert-char-map)
            (let ((key (car convert-char-map))
                  (trans (if (equal char-width (mini-wubi-halfwidth-state))
                             (car convert-char-map)
                           (cdr convert-char-map)))
                  (map (quail-map)))
              (quail-defrule-internal key trans map)))
          mini-wubi-width-characters-alist))

(defun mini-wubi-setup ()
  "call this function to setup min-wubi,initialize or finalize settings
   be sure that quail current package is mini-wubi"
  (if (mini-wubi-activated)
      (progn
        (when (not mini-wubi-rules-loaded-flag)
          (message "Loading mini-wubi rules...")
          (load "mini-wubi-rules")
          (setq mini-wubi-rules-loaded-flag t))

        ;; make all variables below buffer local
        ;; the quail-current-package point to a quail-map need to make a copy
        ;; otherwise other buffer' quail-curent-package will point to same quail-map
        ;; modify one in a buffer will affect other's quail-map rule
        (setq-local quail-current-package (copy-tree (quail-package "mini-wubi") t))
        (make-local-variable 'mini-wubi-cn-quail-map)
        (make-local-variable 'mini-wubi-eng-quail-map)
        (make-local-variable 'mini-wubi-current-lang-state)
        (make-local-variable 'mini-wubi-current-width-state)

        (setq-local mini-wubi-cn-quail-map (quail-map))

        (mini-wubi-switch-to-lang-state mini-wubi-current-lang-state)
        (mini-wubi-remap-character-width mini-wubi-current-width-state))
    (progn
      (kill-local-variable 'mini-wubi-cn-quail-map)
      (kill-local-variable 'mini-wubi-eng-quail-map)
      (kill-local-variable 'mini-wubi-current-lang-state)
      (kill-local-variable 'mini-wubi-current-width-state))))

(defvar mini-wubi-popup nil)
(defvar mini-wubi-popup-width 10)
(defvar mini-wubi-popup-height 20)

(defun mini-wubi-create-selectlist (&rest args)
  (setq mini-wubi-popup (popup-create (point)
                                      mini-wubi-popup-width
                                      mini-wubi-popup-height
                                      :around t
                                      :scroll-bar t
                                      :margin-left 1
                                      :face 'mini-wubi-popup-face
                                      :selection-face 'mini-wubi-popup-selection-face
                                      )))

(defun mini-wubi-show-selectlist (&rest args)
  (popup-draw mini-wubi-popup))

(defun mini-wubi-selectlist-item-format (name index)
  name)

(defun mini-wubi-selectlist-select (&optional index)
  (when (and
         index
         (numberp index))
    (popup-select mini-wubi-popup index)))

(defun mini-wubi-selectlist-select-next ()
  (when quail-current-translations
      (let ((indices (car quail-current-translations)))
        (mini-wubi-selectlist-select (car indices)))))

(defun mini-wubi-selectlist-select-previous ()
  (when quail-current-translations
    (let ((indices (car quail-current-translations)))
      (mini-wubi-selectlist-select (car indices)))))

(defun mini-wubi-selectlist-last-selection ()
  (when quail-current-translations
    (let ((indices (car quail-current-translations)))
      (mini-wubi-selectlist-select (car indices)))))

(defun mini-wubi-update-selectlist (&rest args)
  (if (and quail-current-translations
	         (not (quail-deterministic)))
	    (let* ((indices (car quail-current-translations))
		         (cur (car indices))
		         (start (nth 1 indices))
		         (end (nth 2 indices))
		         (idx start)
             (mini-wubi-popup-list '()))
	      (while (< idx end)
	        (let ((trans (aref (cdr quail-current-translations) idx))
		            (current-idx (if (= (- idx start) 9) 0
				                       (1+ (- idx start)))))
		        (or (stringp trans)
		            (setq trans (string trans)))
            (setq mini-wubi-popup-list (append
                                        mini-wubi-popup-list
                                        (list (popup-make-item (mini-wubi-selectlist-item-format trans
                                                                                                 current-idx)
                                                               :summary (number-to-string current-idx))))))
		      (setq idx (1+ idx)))
        (popup-set-list mini-wubi-popup mini-wubi-popup-list)
        (mini-wubi-show-selectlist))
    (popup-set-list mini-wubi-popup '())
    (mini-wubi-show-selectlist)))

(defun mini-wubi-hide-selectlist (&rest args)
  (popup-delete mini-wubi-popup))

(quail-define-package
 mini-wubi-name
 mini-wubi-lang
 mini-wubi-title
 mini-wubi-guidance
 mini-wubi-doc-string
 mini-wubi-trans-keys
 mini-wubi-forget-last-selection
 mini-wubi-deterministic
 mini-wubi-use-kbd-translate
 mini-wubi-show-layout
 mini-wubi-create-decode-map
 mini-wubi-maximum-shortest
 mini-wubi-overylay-plist
 mini-wubi-upadte-translation-fn
 mini-wubi-conversion-keys
 mini-wubi-simple)

(advice-add 'toggle-input-method :after
            'mini-wubi-update-mode-line-indicator)

(advice-add 'quail-input-method :before
            'mini-wubi-create-selectlist)

(advice-add 'quail-update-translation :after
            'mini-wubi-update-selectlist)

(advice-add 'quail-input-method :after
            'mini-wubi-hide-selectlist)

(advice-add 'quail-update-guidance :after
            'mini-wubi-selectlist-last-selection)

(provide 'mini-wubi)

;;; mini-wubi.el ends here
