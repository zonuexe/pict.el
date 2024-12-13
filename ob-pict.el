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

(defcustom ob-pict-binary-cell-symbols '("T" "")
  "Symbols used for binary cell representation in tables.
The first value represents a \"true\" state, and the second represents a
\"false\" state.  You can customize this variable to use any symbols that
suit your preferences."
  :type '(list (string :tag "True symbol") (string :tag "False symbol")))

(defvar ob-pict-table-style-converter-alist
  '(("transpose" . ob-pict--convert-as-transposed-list)
    ("matrix" . ob-pict--convert-as-matrix-list)))

(eval-when-compile
  (defvar org-babel-confirm-evaluate-answer-no))

(defvar ob-pict--model-text)

(defconst ob-pict--re-model-search-param
  (eval-when-compile
    (rx line-start (* space) (group (+ (not ":"))) (* space) ":" (* space))))


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

(defun ob-pict--convert-as-transposed-list (testcase)
  "Concat PICT STATISTICS and TESTCASE as list."
  (let ((list-testcase (ob-pict--parse-tsv-result testcase)))
    (append
     (list (cons "Rule" (number-sequence 1 (- (length list-testcase) 2)))
           'hline)
     (apply #'cl-mapcar #'list (cons (car list-testcase) (cddr list-testcase))))))


(defun ob-pict--fetch-params-alist ()
  "Parse model parameters from the current buffer and return them as an alist.
Each parameter key is associated with a list of values."
  (goto-char (point-min))
  (cl-loop while (re-search-forward ob-pict--re-model-search-param nil t)
           for key = (match-string-no-properties 1)
           for value = (mapcar #'string-trim
                               (split-string (buffer-substring-no-properties (point) (pos-eol)) ","))
           collect (cons key value)))

(defun ob-pict--convert-as-matrix-list (testcase)
  "Convert TESTCASE data into a matrix-style list for visualization.
This function generates a matrix that compares test case results against model
parameters."
  (let* ((true-sym (nth 0 ob-pict-binary-cell-symbols))
         (false-sym (nth 1 ob-pict-binary-cell-symbols))
         (flatten-params (cl-loop for (key . value) in (with-temp-buffer
                                                         (insert ob-pict--model-text)
                                                         (ob-pict--fetch-params-alist))
                                  append (mapcar (lambda (val) (cons key val)) value)))
         (list-testcase (ob-pict--parse-tsv-result testcase))
         (header (car list-testcase))
         (alist-testcase (mapcar (lambda (row) (cl-mapcar #'cons header row))
                                 (cddr list-testcase))))
    (append
     (list (cons "Rule" (number-sequence 1 (- (length list-testcase) 2)))
           'hline)
     (cl-loop for (key . value) in flatten-params
              for row = (cons key value)
              collect (append (list (concat key ": " value))
                              (mapcar (lambda (pair) (if (equal (assoc key pair) row) true-sym false-sym))
                                      alist-testcase))))))

;;;###autoload
(defun org-babel-execute:pict (body params)
  "Generate a test case from PICT code block in BODY using PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((temp-file (org-babel-temp-file "pict-" ".txt"))
         (result (alist-get :result params))
         (vars (org-babel--get-vars params))
         (style (alist-get (alist-get 'style vars) ob-pict-table-style-converter-alist nil nil #'equal))
         (show-statistics (or (alist-get 'stats vars)
                              (member "stats" result) (member "statistics" result)))
         (should-not-convert
          (or (member "verbatim" result) (member "file" result) (member "code" result)))
         (should-convert
          (or (member "table" result) (member "vector" result) (member "list" result)
              (eq ob-pict-default-result 'table)))
         (ob-pict--model-text (org-babel-expand-body:generic body params))
         raw-testcase raw-stats)
    (with-temp-file temp-file
      (insert ob-pict--model-text))
    (when (and show-statistics (null style))
      (setq raw-stats (org-babel-eval (pict-build-command-line temp-file :show-statistics t) "")))
    (unless (equal show-statistics "only")
      (setq raw-testcase (org-babel-eval (pict-build-command-line temp-file) "")))
    (cond
     (should-not-convert (ob-pict--concat-as-raw raw-stats raw-testcase))
     (should-convert (if style
                         (funcall style raw-testcase)
                       (ob-pict--concat-as-list raw-stats raw-testcase)))
     ((ob-pict--concat-as-raw raw-stats raw-testcase)))))

(provide 'ob-pict)
;;; ob-pict.el ends here
