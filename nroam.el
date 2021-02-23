;;; nroam.el --- Org-roam backlinks within org-mode buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; URL: https://github.com/NicolasPetton/nroam
;; Keywords: outlines, convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org-roam "1.2.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; nroam is a supplementary package for org-roam that replaces the backlink side
;; buffer of Org-roam.  Instead, it displays org-roam backlinks at the end of
;; org-roam buffers.
;;
;; To setup nroam for all org-mode buffer, evaluate the following:
;; (add-hook 'org-mode-hook #'nroam-mode)

;;; Code:

(require 'org-roam)
(require 'org-roam-buffer)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'bookmark)

(defvar-local nroam-start-marker nil)
(defvar-local nroam-end-marker nil)

(defvar nroam-work-buffer " *nroam-work*")

(defmacro with-nroam-markers (&rest body)
  "Evaluate BODY.
Make the region inserted by BODY read-only, and marked with
`nroam-start-marker' and `nroam-end-marker'."
  (declare (indent 0) (debug t))
  `(let ((beg (point)))
     (set-marker nroam-start-marker (point))
     ,@body
     (put-text-property beg (1+ beg) 'front-sticky '(read-only))
     (put-text-property beg (point) 'read-only t)
     (set-marker nroam-end-marker (point))))

(defvar nroam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nroam-ctrl-c-ctrl-c)
    (define-key map (kbd "RET") #'nroam-return)
    map))

(define-minor-mode nroam-mode
  "Show backlinks at the end of org-roam buffers."
  :lighter "nroam"
  :keymap nroam-mode-map
  (if nroam-mode
      (progn
        (nroam--init-work-buffer)
        (add-hook 'before-save-hook #'nroam--prune-backlinks nil t)
        (add-hook 'after-save-hook #'nroam--update-backlinks-maybe nil t)
        (nroam-update-backlinks))
    (remove-hook 'before-save-hook #'nroam--prune-backlinks t)
    (remove-hook 'after-save-hook #'nroam--update-backlinks-maybe t)
    (nroam--prune-backlinks)))

(defun nroam--init-work-buffer ()
  "Initiate nroam hidden buffer."
  (get-buffer-create nroam-work-buffer)
  (with-current-buffer nroam-work-buffer
    (delay-mode-hooks
      (org-mode))))

(defun nroam-ctrl-c-ctrl-c ()
  "Update the backlinks for the current buffer, or fallback to `org-ctrl-c-ctrl-c'."
  (interactive)
  (if (nroam--point-at-backlinks-p)
      (nroam-update-backlinks)
    (call-interactively (if org-capture-mode
                            #'org-capture-finalize
                          #'org-ctrl-c-ctrl-c))))

(defun nroam-return ()
  "Open backlink at point, or fallback to `org-return'."
  (interactive)
  (if (nroam--point-at-backlinks-p)
      (nroam--follow-link)
    (call-interactively #'org-return)))

(defun nroam--point-at-backlinks-p ()
  "Return non-hil if point if on the backlinks section."
  (when-let* ((beg (marker-position nroam-start-marker))
              (end (marker-position nroam-end-marker)))
    (<= beg (point) end)))

(defun nroam-update-backlinks ()
  "Update org-roam backlinks for the current buffer."
  (interactive)
  (nroam--setup-markers)
  (nroam--prune-backlinks)
  (nroam--insert-backlinks))

(defun nroam--update-backlinks-maybe ()
  "Update backlinks when in nroam-mode."
  (when nroam-mode
    (nroam-update-backlinks)))

(defun nroam--setup-markers ()
  "Setup the current buffer with markers for nroam."
  (unless (nroam--backlinks-inserted-p)
    (progn
      (setq nroam-start-marker (make-marker))
      (setq nroam-end-marker (make-marker)))))

(defun nroam--backlinks-inserted-p ()
  "Return non-nil if the current buffer has backlinks inserted."
  (and (markerp nroam-start-marker)
       (marker-position nroam-start-marker)))

(defun nroam--prune-backlinks ()
  "Remove the backlinks section from the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (with-buffer-modified-unmodified
       (when (nroam--backlinks-inserted-p)
         (delete-region nroam-start-marker nroam-end-marker))))))

(defun nroam--insert-backlinks ()
  "Insert org-roam backlinks for the current buffer."
  (if-let* ((backlinks (nroam--get-backlinks))
            (groups (seq-reverse (nroam--group-backlinks backlinks))))
      (with-buffer-modified-unmodified
       (save-excursion
         (goto-char (point-max))
         (with-nroam-markers
           (nroam--ensure-empty-line)
           (nroam--insert-backlinks-heading (seq-length backlinks))
           (seq-do #'nroam--insert-backlink-group groups)))
       (nroam--hide-drawers))))

(defun nroam--get-backlinks ()
  "Return a list of backlinks for the current buffer."
  (if-let* ((file-path (buffer-file-name (current-buffer)))
            (titles (org-roam--extract-titles)))
      (org-roam--get-backlinks (cons file-path titles))))

(defun nroam--group-backlinks (backlinks)
  "Return BACKLINKS grouped by source file."
  (seq-group-by #'car backlinks))

(defun nroam--insert-backlinks-heading (count)
  "Insert the heading for the backlinks section with a COUNT."
  (insert (format "* %s %s\n"
                  count
                  (nroam--pluralize count "linked reference"))))

(defun nroam--insert-backlink-group (group)
  "Insert all backlinks in GROUP."
  (let ((file (car group))
        (backlinks (cdr group)))
    (insert (format "\n** %s\n"
                    (org-roam-format-link
                     file
                     (org-roam-db--get-title file)
                     "file")))
    (seq-do-indexed (lambda (backlink index)
                      (nroam--insert-backlink backlink)
                      (when (< index (1- (seq-length backlinks)))
                        (insert "\n")))
                    backlinks)))

(defun nroam--insert-backlink (backlink)
  "Insert a link to the org-roam BACKLINK."
  (nroam--insert-source-content backlink))

(defun nroam--insert-source-content (backlink)
  "Insert the source element where BACKLINK is defined."
  (seq-let (from _ props) backlink
    (when-let* ((backlink-point (plist-get props :point))
                (elt (nroam--crawl-source from backlink-point))
                (type (car elt))
                (content (string-trim (cdr elt)))
                (beg (point)))
      (progn
        (pcase type
          ('headline (progn
                       (org-paste-subtree 3 (nroam--fix-links content from))
                       (goto-char (point-max))))
          (_ (insert (nroam--fix-links content from))))
        (set-text-properties beg (point)
                             `(nroam-link t file ,from point ,backlink-point))
        (insert "\n")))))

(defun nroam--crawl-source (file point)
  "Return the source element in FILE at POINT."
  (with-current-buffer nroam-work-buffer
    (insert-file-contents file nil nil nil 'replace)
    (goto-char point)
    (let ((elt (org-element-at-point)))
      (let ((begin (org-element-property :begin elt))
            (end (org-element-property :end elt))
            (type (org-element-type elt)))
        `(,type . ,(buffer-substring begin end))))))

(defun nroam--fix-links (content origin)
  "Correct all relative links in CONTENT from ORIGIN.
Temporary fix until `org-roam' v2 is out."
  (org-roam-buffer-expand-links content origin))

(defun nroam--follow-link ()
  "Follow backlink at point."
  (when (get-text-property (point) 'nroam-link)
    (let ((file (get-text-property (point) 'file))
          (point (get-text-property (point) 'point)))
      (org-open-file file t)
      (goto-char point))))

(defun nroam--hide-drawers ()
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

(defun nroam--pluralize (n thing)
  "Pluralize the string THING if N>1."
  (format "%s%s"
          thing
          (if (> n 1) "s" "")))

(defun nroam--ensure-empty-line ()
  "Insert a newline character if the buffer does not end with a newline."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (eq ?\n (char-before (1- (point)))) (insert "\n"))))

(provide 'nroam)
;;; nroam.el ends here
