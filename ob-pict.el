;;; ob-pict.el --- Babel functions for PICT models   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  USAMI Kenta

;; Author: Kenta Usami <tadsan@zonu.me>
;; Keywords: tools, org, pict

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

;; Org Babel support for evaluating PICT model.

;;; Code:
(require 'org)
(require 'ob)
(require 'pict)
(require 'org-table)

(defgroup ob-pict nil
  "Org mode blocks for PICT."
  :group 'org
  :group 'pict)

(defcustom ob-pict-default-result 'table
  "Specify the default result format for `ob-pict'."
  :type '(choise (const :tag "Display results as an Org table." table)
                 (const :tag "Return raw output in TSV format." tsv))
  :safe (lambda (v) (memq v '(table tsv))))

(eval-when-compile
  (defvar org-babel-confirm-evaluate-answer-no))

(defun ob-pict--parse-tsv-result (tsv)
  "Convert a TSV (tab-separated values) string into a list.
The resulting list has the first row (headers), followed by \\='line,
and then the subsequent rows."
  (let ((result (mapcar (lambda (row) (string-split row "\t"))
                        (string-split tsv
                                      (eval-when-compile (rx (? "\r") "\n"))
                                      t))))
    (append (list (car result) 'hline) (cdr result))))

(defun ob-pict--parse-statistics (statistics)
  "Convert the STATISTICS string into a list."
  (mapcar (lambda (row)
            (let ((parts (string-split row ":" t)))
              (list (car parts) (mapconcat #'identity (cdr parts) ":"))))
          (string-split statistics (eval-when-compile (rx (? "\r") "\n")) t)))

(defun ob-pict--concat-as-raw (statistics testcase)
  "Concat PICT STATISTICS and TESTCASE as raw text."
  (if (and statistics testcase)
      (concat statistics "\n" testcase)
    (or statistics testcase)))

(defun ob-pict--concat-as-list (statistics testcase)
  "Concat PICT STATISTICS and TESTCASE as list."
  (let ((list-statistics (when statistics
                           (ob-pict--parse-statistics statistics)))
        (list-testcase (when testcase
                         (ob-pict--parse-tsv-result testcase))))
    (if (and list-statistics list-testcase)
        (append list-statistics (list 'hline) list-testcase)
      (or list-statistics list-testcase))))

;;;###autoload
(defun org-babel-execute:pict (body params)
  "Generate a test case from PICT code block in BODY using PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((tmp-file (org-babel-temp-file "pict-" ".txt"))
         (result (alist-get :result params))
         (vars (org-babel--get-vars params))
         (show-statistics (or (alist-get 'stats vars)
                              (member "stats" result) (member "statistics" result)))
         (should-not-convert
          (or (member "verbatim" result) (member "file" result) (member "code" result)))
         (should-convert
          (or (member "table" result) (member "vector" result) (member "list" result)
              (eq ob-pict-default-result 'table)))
         raw-testcase raw-stats)
    (message "Params %S" (org-babel-process-params params))
    (message "show-statistics: %S, %S" show-statistics (equal show-statistics "only"))
    (with-temp-file tmp-file
      (insert (org-babel-expand-body:generic body params)))
    (when show-statistics
      (setq raw-stats (org-babel-eval (pict-build-command-line tmp-file :show-statistics t) "")))
    (unless (equal show-statistics "only")
      (setq raw-testcase (org-babel-eval (pict-build-command-line tmp-file) "")))
    (cond
     (should-not-convert (ob-pict--concat-as-raw raw-stats raw-testcase))
     (should-convert (ob-pict--concat-as-list raw-stats raw-testcase))
     ((ob-pict--concat-as-raw raw-stats raw-testcase)))))

(provide 'ob-pict)
;;; ob-pict.el ends here
