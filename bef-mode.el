;;; bef-mode.el --- A Befunge editing mode    -*- lexical-binding: t; -*-

;;; Copyright (c) 2011-2023, kaki

;; Author: kaki
;; Package-Requires: ((cl-lib "1.0"))
;; Created: 5 Nov 2011
;; Version: 0.1.0
;; Keywords: languages, Befunge

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

(defconst bef-mode-version "0.1.0")

(defgroup bef-mode nil
  "Major mode for editing Befunge code."
  :group 'languages)

(defcustom bef-mode-use-bef-mode-whitespace t
  "If Non-nil, use bef-mode-whitespace."
  :group 'bef-mode
  :type 'boolean)

(defface bef-mode-stop-face
  '((((class color) (background dark))
     :foreground "magenta")
    (((class color) (background light))
     :foreground "DeepPink")
    (t :bold t))
  "Bef mode face used to highlight @."
  :group 'bef-mode)

(defface bef-mode-control-char-face
  '((((class color) (background dark))
     :foreground "DarkCyan")
    (((class color) (background light))
     :foreground "orchid")
    (t :inverse-video t))
  "Bef mode face used to highlight control characters."
  :group 'bef-mode)

(defface bef-mode-non-ascii-face
  '((((class color) (background dark))
     :foreground "yellow4")
    (((class color) (background light))
     :foreground "yellow4")
    (t :inverse-video t))
  "Bef mode face used to highlight non ascii characters."
  :group 'bef-mode)

(defface bef-mode-non-ascii-control-char-face
  '((((class color) (background dark))
     :foreground "DarkSeaGreen4")
    (((class color) (background light))
     :foreground "LightSalmon2")
    (t :inverse-video t))
  "Yet another bef mode face used to highlight non ascii characters.")

(defface bef-mode-multibyte-char-face
  '((t :inherit font-lock-comment-face))
  "Bef mode face used to highlight multibyte characters."
  :group 'bef-mode)

(defvar bef-mode-monospace-display-table
  (let ((table (make-display-table))
        (ctl-to-ascii (lambda (c)
                        (vector (logxor c #x40))))
        (non-ascii-mask #x3fff00))
    (cl-loop for c from ?\C-@ to ?\C-_
             unless (= c ?\C-j)
             do (aset table c (funcall ctl-to-ascii c)))
    (aset table ?\C-? (funcall ctl-to-ascii ?\C-?))

    (cl-loop for c from (logior non-ascii-mask ?\200) to (logior non-ascii-mask ?\377)
             for glyph = (logand #x7f c)
             do (aset table c
                      (if (and (< 32 glyph)
                               (< glyph 127))
                          (vector glyph)
                        (funcall ctl-to-ascii glyph))))
    table))

(defvar bef-mode-syntax-table
   (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "_" table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\) "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?\{ "." table)
    (modify-syntax-entry ?\} "." table)
    (modify-syntax-entry ?\t "." table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\; "." table)
    table))

(defvar bef-mode-command-table
  (let ((tbl (make-char-table 'bef-mode-command-table)))
    (cl-loop for (ch . spec)
             in '((?! ((x) . (!x)) "Logical Not")
                  (?\" (() . ()) "Toggle Stringmode")
                  (?# (() . ()) "Trampoline")
                  (?$ ((x) . ()) "Pop")
                  (?% ((x y) . (x%y)) "Remainder; If y=0, exit.")
                  (?& (() . (n)) "Input Integer")
                  (?* ((x y) . (x*y)) "Multiply")
                  (?+ ((x y) . (x+y)) "Add")
                  (?, ((c) . ()) "Output Character")
                  (?- ((x y) . (x-y)) "Subtract")
                  (?. ((n) . ()) "Output Integer")
                  (?/ ((x y) . (x/y)) "Divide; If y=0, ask the user what result they want.")
                  (?: ((x) . (x x)) "Duplicate")
                  (?< (() . ()) "Go West")
                  (?> (() . ()) "Go East")
                  (?? (() . ()) "Go Away")
                  (?@ (() . ()) "Stop")
                  (?\\ ((x y) . (y x)) "Swap")
                  (?^ (() . ()) "Go North")
                  (?_ ((n) . ()) "East-West If; Act as '>' if n=0, otherwise '<'.")
                  (?` ((x y) . (x>y)) "Greater")
                  (?g ((x y) . (n)) "Get")
                  (?p ((n x y) . ()) "Put")
                  (?v (() . ()) "Go South")
                  (?| ((n) . ()) "North-South If; Act as 'v' if n=0, otherwise '^'.")
                  (?~ (() . (c)) "Input Character"))
             do (aset tbl ch spec))
    (cl-loop for ch from ?0 to ?9
             do (aset tbl ch `((() . (,(- ch ?0))) ,(format "Push %c" ch))))
    tbl))

(defun bef-mode-info-at-point-by-command-table (command-table)
  (let ((ch (following-char))
        (argf (lambda (args)
                (or args "()"))))
    (pcase (aref command-table ch)
      (`((,in . ,out) ,doc)
       (format "%c :: %s -> %s  : %s"
               ch (funcall argf in) (funcall argf out) doc)))))

(defun bef-mode-info-at-point ()
  (bef-mode-info-at-point-by-command-table bef-mode-command-table))

(defvar bef-mode-font-lock-keywords-1
  '(("\""        . font-lock-string-face)
    ("#"         . font-lock-comment-face)
    ("[v^<>_|?]" . font-lock-keyword-face)
    ("@"         . 'bef-mode-stop-face)

    ("[\C-@-\C-i\C-k-\C-_\C-?]" . 'bef-mode-control-char-face)
    ("[\241-\376]"              . 'bef-mode-non-ascii-face)
    ("[\200-\240\377]"          . 'bef-mode-non-ascii-control-char-face)
    ("[^\C-@-\377]"             . 'bef-mode-multibyte-char-face)))

(defvar bef-mode-font-lock-keywords-2
  (append
   bef-mode-font-lock-keywords-1
   '(("[0-9]"                    . font-lock-constant-face)
     ("[-+*/%!`:$\\]"            . font-lock-variable-name-face)
     ("[pg~&,.]"                 . font-lock-function-name-face))))


(defun bef-mode-insert-byte (byte count)
  (interactive "nInsert byte: \np")
  (insert-byte byte count))

(defun bef-mode-reverse-chars-buffer-substring (beg end)
  (interactive "r")
  (let* ((str (buffer-substring beg end))
         (reversed-str (apply 'string
                              (reverse
                               (string-to-list str)))))
    (delete-region beg end)
    (insert
     (replace-regexp-in-string
      "<\\|>"
      (lambda (s)
        (assoc-default s '(("<" . ">")
                           (">" . "<"))))
      reversed-str))))

(defun bef-mode-reverse-chars ()
  "Reverse the order of characters in a line."
  (interactive)
  (let* ((pos (point))
         (beg (line-beginning-position))
         (end (line-end-position)))
    (bef-mode-reverse-chars-buffer-substring beg end)
    (goto-char (+ beg (- end pos)))))

(defun bef-mode-rotate-horizontally (cnt)
  (let* ((pos (point)))
    (cond ((> cnt 0)
           (beginning-of-line)
           (skip-chars-forward " ")
           (let* ((beg (point))
                  (end (min (+ beg cnt)
                            (save-excursion
                              (end-of-line)
                              (skip-chars-backward " ")
                              (point))))
                  (str (buffer-substring beg end)))
             (delete-region beg end)
             (end-of-line)
             (skip-chars-backward " ")
             (insert str)
             (goto-char
              (cond
               ((and (<= beg pos) (< pos end))
                ;; wrap
                (- (point) cnt (- beg pos)))
               ((and (<= beg pos) (< pos (point)))
                ;; inside
                (- pos cnt))
               (t
                ;; outside
                pos)))))
          ((< cnt 0)
           (end-of-line)
           (skip-chars-backward " ")
           (let* ((end (point))
                  (beg (max (+ end cnt)
                            (save-excursion
                              (beginning-of-line)
                              (skip-chars-forward " ")
                              (point))))
                  (str (buffer-substring beg end)))
             (delete-region beg end)
             (beginning-of-line)
             (skip-chars-forward " ")
             (insert str)
             (goto-char
              (cond
               ((and (<= beg pos) (< pos end))
                ;; wrap
                (- (point) (- cnt) (- beg pos)))
               ((and (<= (+ (point) cnt) pos) (< pos end))
                ;; inside
                (- pos cnt))
               (t
                ;; outside
                pos))))))))

(defun bef-mode-rotate-horizontally-number (arg)
  (if arg
      (prefix-numeric-value arg)
    (if mark-active
        (abs (- (mark) (point)))
      1)))

(defun bef-mode-rotate-backward (arg)
  (interactive "P")
  (bef-mode-rotate-horizontally
   (bef-mode-rotate-horizontally-number arg)))

(defun bef-mode-rotate-forward (arg)
  (interactive "P")
  (bef-mode-rotate-horizontally
   (- (bef-mode-rotate-horizontally-number arg))))

(defvar bef-mode-rotate-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") #'bef-mode-rotate-forward)
    (define-key map (kbd "C-b") #'bef-mode-rotate-backward)
    map))

(defun bef-mode-rotate ()
  (interactive)
  (set-transient-map bef-mode-rotate-map t)
  (message "bef-mode-rotate"))

(defun bef-mode-current-column ()
  (- (point) (line-beginning-position)))

(defvar bef-mode-temporary-goal-column 0)

(defvar bef-mode-line-move-functions
  '(bef-mode-next-line bef-mode-previous-line))

(defun bef-mode-line-move-1 (arg &optional try-vscroll)
  (if (> arg 0)
      (next-line arg try-vscroll)
    (previous-line (- arg) try-vscroll)))

(defun bef-mode-line-move (arg &optional try-vscroll)
  (if line-move-visual
      (bef-mode-line-move-1 arg try-vscroll)
    (let ((col (if (memq last-command bef-mode-line-move-functions)
                    bef-mode-temporary-goal-column
                  (setq bef-mode-temporary-goal-column
                        (bef-mode-current-column)))))
      (setq temporary-goal-column
            (save-excursion
              (forward-line arg)
              (skip-chars-forward "^\n" (+ (point)
                                           bef-mode-temporary-goal-column))
              (current-column)))
      (let ((last-command #'next-line)
            (this-command #'next-line))
        (bef-mode-line-move-1 arg try-vscroll)))))

(defun bef-mode-next-line (&optional arg try-vscroll)
  (interactive "p\np")
  (bef-mode-line-move arg try-vscroll))

(defun bef-mode-previous-line (&optional arg try-vscroll)
  (interactive "p\np")
  (bef-mode-line-move (- arg) try-vscroll))

(defvar bef-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'bef-mode-next-line)
    (define-key map (kbd "C-p") #'bef-mode-previous-line)
    (define-key map (kbd "C-c C-o") #'overwrite-mode)
    (define-key map (kbd "C-c C-v") #'bef-mode-reverse-chars)
    (define-key map (kbd "C-c C-r") #'bef-mode-rotate)
    map))

(defun bef-mode-restore-buffer-multibyte ()
  (set-buffer-multibyte t)
  (remove-hook 'change-major-mode-hook
               #'bef-mode-restore-buffer-multibyte))

(defun bef-mode-enable-buffer-multibyte ()
  (interactive)
  (unless enable-multibyte-characters
    (bef-mode-restore-buffer-multibyte)
    (font-lock-flush)))

(defun bef-mode-disable-buffer-multibyte ()
  (interactive)
  (when enable-multibyte-characters
    (add-hook 'change-major-mode-hook
              #'bef-mode-restore-buffer-multibyte
              nil t)
    (set-buffer-multibyte nil)
    (font-lock-flush)))

(autoload 'bef-mode-whitespace-turn-on "bef-mode-whitespace")

(defun bef-mode-mark-line ()
  (push-mark (line-end-position))
  (beginning-of-line))

;;;###autoload
(define-derived-mode bef-mode nil "Bef"
  "Major mode for editing Befunge code."
  (setq tab-width 1)
  (setq buffer-display-table bef-mode-monospace-display-table)

  (setq font-lock-defaults
        '((bef-mode-font-lock-keywords-1
           bef-mode-font-lock-keywords-2)
          t nil nil backward-char
          (font-lock-mark-block-function . bef-mode-mark-line)))

  (bef-mode-disable-buffer-multibyte)

  (when (and bef-mode-use-bef-mode-whitespace
             (require 'whitespace nil t))
    (bef-mode-whitespace-turn-on))

  (setq-local eldoc-documentation-function
              #'bef-mode-info-at-point)
  (eldoc-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bef\\'" . bef-mode))

(provide 'bef-mode)

;;; bef-mode.el ends here
