;;; pict.el --- PICT and org-babel support -*- lexical-binding: t; -*-

;; Copyright (C) 2024  USAMI Kenta

;; Author: Kenta Usami <tadsan@zonu.me>
;; Created: 07 Dec 2024
;; Version: 0.1.0
;; Keywords: tools, org, ob-pict
;; Homepage: https://github.com/zonuexe/pict.el
;; Package-Requires: ((emacs "29.1") (org "9"))
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This feature contains major modes for editing PICT models and functions for building `pict` commands.
;; You can also change the symbols for your model's syntax using custom variables.

;;; Code:
(require 'conf-mode)
(require 'cl-lib)

(defgroup pict nil
  "PICT interface."
  :group 'tools)

(defcustom pict-executable (executable-find "pict")
  "Path to PICT executable file."
  :type 'string)

(defcustom pict-separator-values nil
  "Separator for values."
  :type '(choise null string)
  :safe (lambda (v) (or (null v) (stringp v))))

(defcustom pict-separator-aliases nil
  "Separator for aliases."
  :type '(choise null string)
  :safe (lambda (v) (or (null v) (stringp v))))

(defcustom pict-negative-value-prefix nil
  "Negative value prefix."
  :type '(choise null string)
  :safe (lambda (v) (or (null v) (stringp v))))

(eval-and-compile
  (defconst pict-syntax-keywords
    '("IF" "THEN" "ELSE" "NOT" "AND" "OR" "LIKE" "IN")))

(defvar pict-conf-font-lock-keywords
  `(,@conf-colon-font-lock-keywords
    (,(eval-when-compile (rx "[" (group (+? any)) "]"))
     1 font-lock-variable-name-face)
    (,(eval-when-compile (rx-to-string `(: symbol-start (or ,@pict-syntax-keywords) symbol-end) t))
     0 font-lock-builtin-face)))

;;;###autoload
(defun pict ()
  "Execute PICT command in this buffer file."
  (interactive)
  (compile (pict-build-command-line buffer-file-name)))

;;;###autoload
(define-derived-mode pict-mode conf-mode
  "PICT"
  (conf-mode-initialize "#" 'pict-conf-font-lock-keywords))

(cl-defun pict-build-command-line (file &key show-statistics)
  "Build PICT command line string from FILE.

When SHOW-STATISTICS is non-NIL, statistics are printed instead of generating
test cases."
  (let ((args (append (list pict-executable file)
                      (when show-statistics
                        (list "/s"))
                      (when pict-separator-values
                        (list (concat "/d:" pict-separator-values)))
                      (when pict-separator-aliases
                        (list (concat "/a:" pict-separator-aliases)))
                      (when pict-negative-value-prefix
                        (list (concat "/n:" pict-negative-value-prefix))))))
    (mapconcat #'shell-quote-argument args " ")))

(provide 'pict)
;;; pict.el ends here
