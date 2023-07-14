;;; bef-mode.el --- A Befunge-98 editing mode  -*- lexical-binding: t; -*-

;;; Copyright (c) 2019-2023 kaki

;; Author: kaki
;; Package-Requires: ((cl-lib "1.0"))
;; Created: 26 Oct 2019
;; Version: 0.1.0
;; Keywords: languages, Befunge-98

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'bef-mode)

(defvar bef98-mode-command-table
  (let ((tbl (make-char-table 'bef-mode-command-table)))
    (set-char-table-parent tbl bef-mode-command-table)
    (cl-loop for (ch . spec)
             in '((?% ((x y) . (x%y)) "Remainder; If y=0, push 0.")
                  (?' (() . (c)) "Fetch Character")
                  (?\( ((en..e1 n) . (f 1)) "Load Semantics")
                  (?\) ((en..e1 n) . ()) "Unload Semantics")
                  (?/ ((x y) . (x/y)) "Divide; If y=0, push 0.")
                  (?\; (() . ()) "Jump over")
                  (?= ((0gnirts) . (r)) "Execute")
                  (?\[ (() . ()) "Turn Left")
                  (?\] (() . ()) "Turn Right")
                  (?h (() . ()) "Go High/3D; Reflects in Unefunge and Befunge.")
                  (?i ((x y f 0gnirts) . ()) "Input File")
                  (?j ((s) . ()) "Jump Forward")
                  (?k ((n) . ()) "Iterate; Execute next instruction 0 or n+1 times.")
                  (?l (() . ()) "Go Low/3D;  Reflects in Unefunge and Befunge.")
                  (?m ((n) . ()) "High-Low If/3D; Act as 'l' if n=0, otherwise 'h'")
                  (?n ((en..e1) . ()) "Clear Stack")
                  (?o ((x y x\' y\' f 0gnirts) . ()) "Output File")
                  (?q ((r) . ()) "Exit")
                  (?r (() . ()) "Reflect")
                  (?s ((c) . ()) "Store Character")
                  (?t (() . ()) "Split")
                  (?u ((n) . ((en..e1))) "Stack Under Stack")
                  (?w ((a b) . ()) "Compare; Act as ']' if a>b, '[' if a<b, otherwize 'z'")
                  (?x ((x y) . ()) "Absolute Delta")
                  (?y ((c) . (en(..e1))) "Get SysInfo")
                  (?z (() . ()) "No Operation")
                  (?{ ((en..e1 n) . ((en..e1))) "Begin Block; Push offset as a vector, push a new stack onto the stack stack, transfer n elements, and set storage offset.")
                  (?} ((en..e1 n) . ((en..e1))) "End Block; Pop off the stack stack, pop a vector and restore storage offset with it, and transfer m elements."))
             do (aset tbl ch spec))
    (cl-loop for ch from ?a to ?f
             do (let ((n (+ (- ch ?a) 10)))
                  (aset tbl ch `((() . (,n)) ,(format "Push %d" n)))))
    tbl))

(defun bef98-mode-info-at-point ()
  (bef-mode-info-at-point-by-command-table bef98-mode-command-table))

(defvar bef98-mode-font-lock-keywords-1
  '(("[\"']"                . font-lock-string-face)
    ("[#;]"                 . font-lock-comment-face)
    ("[]v^<>hl_|mw?rjxkt[]" . font-lock-keyword-face)
    ("[@q]"                 . 'bef-mode-stop-face)

    ("[\C-@-\C-i\C-k-\C-_\C-?]" . 'bef-mode-control-char-face)
    ("[\241-\376]"              . 'bef-mode-non-ascii-face)
    ("[\200-\240\377]"          . 'bef-mode-non-ascii-control-char-face)
    ("[^\C-@-\377]"             . 'bef-mode-multibyte-char-face)))

(defvar bef98-mode-font-lock-keywords-2
  (append
   bef98-mode-font-lock-keywords-1
   '(("[0-9a-f]"      . font-lock-constant-face)
     ("[-+*/%!`:$\\]" . font-lock-variable-name-face)
     ("[pgs~&,.]"     . font-lock-function-name-face)
     ("[io=y(){}nu]"  . font-lock-builtin-face))))

(define-derived-mode bef98-mode bef-mode "Bef98"
  "Major mode for editing Befunge-98 code."
  (setq font-lock-defaults
        '((bef98-mode-font-lock-keywords-1
           bef98-mode-font-lock-keywords-2)
          t nil nil backward-char
          (font-lock-mark-block-function . bef-mode-mark-line)))

  (setq-local eldoc-documentation-function
              #'bef98-mode-info-at-point)
  (eldoc-mode))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bef98\\'" . bef98-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.b98\\'" . bef98-mode))

(provide 'bef98-mode)

;;; bef-mode.el ends here
