;;; nroam-unlinked.el --- Unlinked references section for nroam.el  -*- lexical-binding: t; -*-

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

;; This library provides an unlinked references nroam section for org-roam
;; buffers.

;;; Code:
(require 'org-roam)
(require 'nroam-utils)
(declare-function nroam-register-section "nroam.el")
(declare-function nroam-update "nroam.el")

(defvar-local nroam-unlinked-show-references nil
  "When non-nil, show unlinked references for the current buffer.")

(defun nroam-unlinked-register-section ()
  "Register `nroam-unlinked-section' as a section in nroam."
  (nroam-register-section #'nroam-unlinked-section))

(defun nroam-unlinked-section ()
  "Insert `org-roam' unlinked references for the current buffer."
  (nroam-unlinked--insert-heading)
  (if nroam-unlinked-show-references
      (nroam-unlinked--insert-references)
    (nroam-unlinked--insert-toggle-button)))

(defun nroam-unlinked--insert-heading ()
  "Insert the heading for unlinked references."
  (insert "* Unlinked references\n"))

(defun nroam-unlinked--insert-references ()
  "Insert unlinked references for the current buffer."
  (let (content)
    (save-window-excursion
      (org-roam-unlinked-references)
      (let ((buf (current-buffer)))
        (goto-char (point-min))
        (search-forward "* Unlinked References\n" nil t)
        (setq content (buffer-substring (point) (point-max)))
        (kill-buffer buf)))
    (insert content)))

(defun nroam-unlinked--insert-toggle-button ()
  "Insert a button to show unlinked references."
  (let ((beg (point)))
    (insert "[Show unlinked references]")
    (make-text-button beg (point) 'action #'nroam-unlinked--show-references)
    (insert "\n")))

(defun nroam-unlinked--show-references (&rest _)
  "Search for and show unlinked references."
  (let ((inhibit-read-only t)
        (pos (point-at-bol)))
    (setq nroam-unlinked-show-references t)
    (nroam-update)
    (goto-char pos)
    (org-back-to-heading)))

(provide 'nroam-unlinked)
;;; nroam-unlinked.el ends here
