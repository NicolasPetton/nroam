;;; nroam-backlinks.el --- Backlink section for nroam.el  -*- lexical-binding: t; -*-

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

;; This library provides a backlinks nroam section for org-roam buffers.

;;; Code:
(require 'org-element)
(require 'org-roam)
(require 'seq)
(require 'subr-x)
(require 'org-element)
(require 'nroam-utils)
(declare-function nroam-register-section "nroam.el")

(defvar nroam-backlinks--work-buffer-name " *nroam-work*")

(defun nroam-backlinks-register-section ()
  "Register `nroam-backlinks-section'."
  (nroam-register-section #'nroam-backlinks-section))

(defun nroam-backlinks-section ()
  "Insert org-roam backlinks for the current buffer."
  (let* ((backlinks (nroam-backlinks--get-backlinks))
         (groups (seq-reverse (nroam-backlinks--group backlinks))))
    (nroam-backlinks--insert-heading (seq-length backlinks))
    (nroam--do-separated-by-newlines #'nroam-backlinks--insert-group groups)
    (nroam-backlinks--hide-drawers)))

(defun nroam-backlinks--get-backlinks ()
  "Return a list of backlinks for the current buffer."
  (if-let* ((file-path (buffer-file-name (current-buffer)))
            (titles (org-roam--extract-titles)))
      (org-roam--get-backlinks (cons file-path titles))))

(defun nroam-backlinks--group (backlinks)
  "Return BACKLINKS grouped by source file."
  (seq-group-by #'car backlinks))

(defun nroam-backlinks--insert-heading (count)
  "Insert the heading for the backlinks section with a COUNT."
  (insert (format "* %s %s\n"
                  (if (= count 0) "No" count)
                  (nroam--pluralize count "linked reference"))))

(defun nroam-backlinks--insert-group (group)
  "Insert all backlinks in GROUP."
  (let ((file (car group))
        (backlinks (cdr group)))
    (insert (format "** %s\n"
                    (org-roam-format-link
                     file
                     (org-roam-db--get-title file)
                     "file")))
    (nroam--do-separated-by-newlines #'nroam-backlinks--insert-backlink backlinks)))

(defun nroam-backlinks--insert-backlink (backlink)
  "Insert the source element where BACKLINK is defined."
  (seq-let (file _ props) backlink
    (when-let* ((point (plist-get props :point))
                (elt (nroam-backlinks--crawl-source file point))
                (type (plist-get elt :type))
                (content (nroam--fix-links (string-trim (plist-get elt :string)) file))
                (beg (point)))
      (let ((outline (plist-get elt :outline))
            (full-outline (plist-get elt :full-outline)))
        (if (eq type 'headline)
            (nroam-backlinks--insert-backlink-subtree content outline)
          (nroam-backlinks--insert-backlink-content content full-outline)))
      (set-text-properties beg (point)
                           `(nroam-link t file ,file point ,point))
      (insert "\n"))))

(defun nroam-backlinks--insert-backlink-subtree (content outline)
  "Insert CONTENT as a heading with its subtree.
When OUTLINE is non-nil, insert it as a heading."
  (nroam-backlinks--insert-backlink-breadcrumbs outline)
  (nroam-backlinks--insert-subtree content (if outline 4 3)))

(defun nroam-backlinks--insert-backlink-content (content outline)
  "Insert CONTENT with OUTLINE as a heading if non-nil."
  (nroam-backlinks--insert-backlink-breadcrumbs outline)
  (insert content))

(defun nroam-backlinks--insert-backlink-breadcrumbs (outline)
  "Insert OUTLINE if non-nil as a breadcrumbs heading."
  (when outline
    (let ((str-outline (concat "* " (string-join outline " › "))))
      (nroam-backlinks--insert-subtree str-outline))))

(defun nroam-backlinks--insert-subtree (subtree &optional level)
  "Insert SUBTREE as a LEVEL headline.
When nil, LEVEL defaults to 3."
  (org-paste-subtree (or level 3) subtree)
  (goto-char (point-max)))

(defun nroam-backlinks--crawl-source (file point)
  "Return the source element in FILE at POINT."
  (with-current-buffer (nroam-backlinks--work-buffer)
    (insert-file-contents file nil nil nil 'replace)
    (goto-char point)
    (let* ((elt (org-element-at-point))
           (begin (org-element-property :begin elt))
           (end (org-element-property :end elt))
           (type (org-element-type elt))
           (outline (org-get-outline-path))
           (full-outline (unless (org-before-first-heading-p)
                           (org-get-outline-path 'with-self))))
      `(:type ,type
        :string ,(buffer-substring begin end)
        :outline ,outline
        :full-outline ,full-outline))))

(defun nroam-backlinks--hide-drawers ()
  "Fold all drawers starting at POINT in the current buffer."
  ;; Taken from `org-hide-drawer-all'.
  (save-excursion
    (while (re-search-forward org-drawer-regexp nil t)
      (let* ((pair (get-char-property-and-overlay (line-beginning-position)
						  'invisible))
	     (o (cdr-safe pair)))
	(if (overlayp o) (goto-char (overlay-end o)) ;invisible drawer
	  (pcase (get-char-property-and-overlay (point) 'invisible)
	    (`(outline . ,o) (goto-char (overlay-end o))) ;already folded
	    (_
	     (let* ((drawer (org-element-at-point))
		    (type (org-element-type drawer)))
	       (when (memq type '(drawer property-drawer))
		 (org-hide-drawer-toggle t nil drawer)
		 ;; Make sure to skip drawer entirely or we might flag it
		 ;; another time when matching its ending line with
		 ;; `org-drawer-regexp'.
		 (goto-char (org-element-property :end drawer)))))))))))

(defun nroam-backlinks--work-buffer ()
  "Return the hidden buffer used for crawling operations."
  (if-let ((buf (get-buffer nroam-backlinks--work-buffer-name)))
      buf
    (nroam-backlinks--init-work-buffer)))

(defun nroam-backlinks--init-work-buffer ()
  "Initiate nroam hidden buffer."
  (let ((buf (get-buffer-create nroam-backlinks--work-buffer-name)))
    (with-current-buffer buf
      (delay-mode-hooks (org-mode)))
    buf))

(provide 'nroam-backlinks)
;;; nroam-backlinks.el ends here
