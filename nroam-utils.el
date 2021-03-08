;;; nroam-utils.el --- Util functions for nroam      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nicolas Petton

;; Author: Nicolas Petton <nico@petton.fr>

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

;; This library provides utility functions used by other files in nroam.

;;; Code:
(require 'seq)
(require 'org-roam)

(defun nroam--pluralize (n thing)
  "Pluralize the string THING if N>1."
  (format "%s%s" thing (if (> n 1) "s" "")))

(defun nroam--ensure-empty-line ()
  "Insert a newline character if the buffer does contain one before point."
  (let ((inhibit-read-only t))
    (unless (eq ?\n (char-before (1- (point)))) (insert "\n"))))

(defun nroam--do-separated-by-newlines (function sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Insert a single newline between each call to FUNCTION."
  (seq-do-indexed (lambda (item index)
                    (unless (= index 0)
                      (delete-blank-lines)
                      (nroam--ensure-empty-line))
                    (funcall function item))
                  sequence))

(defun nroam--fix-links (content origin)
  "Correct all relative links in CONTENT from ORIGIN.
Temporary fix until `org-roam' v2 is out."
  (org-roam-buffer-expand-links content origin))

(provide 'nroam-utils)
;;; nroam-utils.el ends here
