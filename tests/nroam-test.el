;;; nroam-test.el --- Tests for nroam                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

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

;; Tests for nroam.

;;; Code:

(require 'buttercup)
(require 'nroam)

(defmacro nroam-test-with-temp-file-buffer (&rest body)
  "Execute BODY within a temporary buffer backed by a temporary file.
Execute BODY and cleanup the file and buffer after that."
  (declare (debug t))
  (let ((filename-var (make-symbol "filename")))
    `(let ((,filename-var (make-temp-file "nroam-test-")))
       (unwind-protect
           (with-current-buffer (find-file ,filename-var)
             ,@body)
         (delete-file ,filename-var)))))

(nroam-test-with-temp-file-buffer (insert "foo"))

(describe "nroam"
  (describe "nroam-mode"
    (it "creates nroam sections immediately"
      (nroam-test-with-temp-file-buffer
       (insert "#+title: my notes\n")
       (nroam-mode)
       (setf (point) (point-min))
       (let ((section-position (re-search-forward "^* Backlinks" nil t)))
         (expect section-position :not :to-be nil)
         (expect section-position :to-be-greater-than 10))))))

(provide 'nroam-test)
;;; nroam-test.el ends here
