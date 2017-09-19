;;; mini-wubi.el --- A simple Chinese wubi input method inside Emacs -*- lexical-binding: t; -*-
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

(require 'quail)

(defvar mini-wubi-rules-loaded-flag nil "flag that tell whether mini-wubi-rules loaed or not.")

(defconst mini-wubi-lang-states '("cn" "eng"))
(defconst mini-wubi-width-states '("half" "full"))

(defvar mini-wubi-current-lang-state (car mini-wubi-lang-states))
(defvar mini-wubi-current-width-state (car mini-wubi-width-states))

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

(defun mini-wubi-switch-lang-state ()
  (interactive)
  (if (equal
       mini-wubi-current-lang-state
       (car mini-wubi-lang-states))
      (progn
        (setq mini-wubi-current-lang-state (cdr mini-wubi-lang-states))
        (quail-install-map mini-wubi-eng-quail-map)
        (message "input method in english state now!"))
    (progn
      (setq mini-wubi-current-lang-state (car mini-wubi-lang-states))
      (quail-install-map mini-wubi-cn-quail-map)
      (message "input method in chinese state now!"))))

(defun mini-wubi-activated ()
  (and
   current-input-method
   (equal (quail-name) mini-wubi-name)))

(defun mini-wubi-in-halfwidth ()
  (equal
   mini-wubi-current-width-state
   (car mini-wubi-width-states)))

(defun mini-wubi-in-fullwidth ()
  (equal
   mini-wubi-current-width-state
   (cdr mini-wubi-width-states)))

(defun mini-wubi-halfwidth-state ()
  (car mini-wubi-width-states))

(defun mini-wubi-fullwidth-state ()
  (cdr mini-wubi-width-states))

(defun mini-wubi-switch-character-width ()
  (interactive)
  (when (mini-wubi-activated)
    (if (mini-wubi-in-halfwidth)
        (progn
          (mini-wubi-remap-character-width (mini-wubi-fullwidth-state))
          (setq mini-wubi-current-width-state (mini-wubi-fullwidth-state)))
      (progn
        (mini-wubi-remap-character-width (mini-wubi-halfwidth-state))
        (setq mini-wubi-current-width-state (mini-wubi-halfwidth-state))))))

(defun mini-wubi-remap-character-width (char-width)
  (mapcar (lambda (convert-char-map)
            (let ((key (car convert-char-map))
                  (trans (if (equal char-width (mini-wubi-halfwidth-state))
                             (car convert-char-map)
                           (cdr convert-char-map)))
                  (map (quail-map)))
              (quail-defrule-internal key trans map)))
          mini-wubi-width-characters-alist))

(defun mini-wubi-init ()
  "call this function to init min-wubi,
   be sure that quail current package is mini-wubi"
  (if (and
       (equal (quail-name) mini-wubi-name)
       (not mini-wubi-rules-loaded-flag))
      (progn
	      (message "Loading mini-wubi rules...")
	      (load "mini-wubi-rules")
	      (setq mini-wubi-rules-loaded-flag t)
        (setq mini-wubi-cn-quail-map (quail-map))
        (if (mini-wubi-in-fullwidth)
            (mini-wubi-remap-character-width (mini-wubi-fullwidth-state))
          (mini-wubi-remap-character-width (mini-wubi-halfwidth-state))))))

(defconst mini-wubi-name "mini-wubi")
(defconst mini-wubi-lang "euc-cn")
(defconst mini-wubi-title "迷雾")
(defconst mini-wubi-doc-string "")
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

(provide 'mini-wubi)

;;; mini-wubi.el ends here
