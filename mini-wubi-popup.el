;;; mini-wubi-popup.el --- popup display for mini-wubi input method -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 SuJiKiNen

;; Author: SuJiKiNen <SuJiKiNen@gmail.com>
;; URL: https://github.com/SuJiKiNen/mini-wubi
;; Keywords: i18n
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
;; This is a popup UI hint for mini-wubi
;;

;;; Code:

(require 'mini-wubi)
(require 'popup)


(defface mini-wubi-popup-selection-face
  '((t (:inherit popup-menu-selection-face)))
  "Face used for the selection in the popup.")

(defface mini-wubi-popup-face
  '((t (:inherit popup-menu-face)))
  "Face used for the popup background.")

(defface mini-wubi-popup-selection-index-face
  '((t (:inherit popup-summary-face)))
  "Face used for selection index")

(defvar mini-wubi-popup nil)
(defvar mini-wubi-popup-width 10)
(defvar mini-wubi-popup-height 20)

(defun mini-wubi-popup-selectlist-item-format (trans n)
  "You can custom popup item view format by overide this function.
TRANS is conversion of your input, N is ordered number of TRANS."
  trans)

(defun mini-wubi-popup-create-selectlist (&rest args)
  "Create popup selectlist, ARGS are passed from hook."
  (setq mini-wubi-popup (popup-create (point)
                                      mini-wubi-popup-width
                                      mini-wubi-popup-height
                                      :around t
                                      :scroll-bar t
                                      :margin-left 1
                                      :face 'mini-wubi-popup-face
                                      :selection-face 'mini-wubi-popup-selection-face
                                      :summary-face 'mini-wubi-popup-selection-index-face)))

(defun mini-wubi-popup-selectlist-select (n)
  "Highlight current translation N candidates selection.
Count from One."
  (when (and n mini-wubi-popup)
    (popup-select mini-wubi-popup n)
    (popup-draw mini-wubi-popup)))


(defun mini-wubi-popup-show-selectlist (selectlist)
  "Disply given SELECTLIST."
  (if selectlist
      (let ((mini-wubi-popup-list '()))
        (dolist (item (mini-wubi-selectlist-items selectlist))
          (push
           (popup-make-item (mini-wubi-popup-selectlist-item-format
                             (mini-wubi-item-trans item)
                             (mini-wubi-item-index item))
                            :summary (number-to-string (mini-wubi-item-index item)))
           mini-wubi-popup-list))
        (popup-set-list mini-wubi-popup mini-wubi-popup-list)
        (mini-wubi-popup-selectlist-select (mini-wubi-selectlist-selected selectlist)))
    (popup-set-list mini-wubi-popup '()))
  (popup-draw mini-wubi-popup))


(defun mini-wubi-popup-hide (&rest args)
  "Hide the mini-wubi-popup, ARGS are passed from hook."
  (when mini-wubi-popup
    (popup-delete mini-wubi-popup)))

(defun mini-wubi-popup--setup-hooks ()
  "Hooks that tirgeer relative popup action."

  (advice-add 'mini-wubi-create-selectlist :before
              'mini-wubi-popup-create-selectlist)

  (advice-add 'mini-wubi-hide-selectlist :before
              'mini-wubi-popup-hide))

(eval-after-load 'mini-wubi
  '(mini-wubi-popup--setup-hooks))

(provide 'mini-wubi-popup)

;;; mini-wubi-popup.el ends here
