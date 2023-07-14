;;; bef-mode-whitespace.el ---     -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2023, kaki

;; Author: kaki
;; Package-Requires: ((whitespace "13.2.2") (dash "2.8.0"))
;; Created: 18 Sep 2014
;; Keywords: Befunge

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

(require 'whitespace)
(require 'dash)

(defvar bef-mode-whitespace-accepts-kinds
  '(face lines lines-tail trailing newline newline-mark empty))

(defvar bef-mode-whitespace-trailing-regexp "\\( +\\)$")
(defvar bef-mode-whitespace-empty-at-bob-regexp regexp-unmatchable)

(defun bef-mode-whitespace-turn-on ()
  (setq-local whitespace-style
              (-intersection whitespace-style
                             bef-mode-whitespace-accepts-kinds))
  (setq-local whitespace-trailing-regexp
              bef-mode-whitespace-trailing-regexp)
  (setq-local whitespace-empty-at-bob-regexp
              bef-mode-whitespace-empty-at-bob-regexp))

(defun bef-mode-whitespace-turn-off ()
  (kill-local-variable 'whitespace-style)
  (kill-local-variable 'whitespace-trailing-regexp)
  (kill-local-variable 'whitespace-empty-at-bob-regexp))

(provide 'bef-mode-whitespace)

;;; bef-mode-whitespace.el ends here
