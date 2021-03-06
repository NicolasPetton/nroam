;;; nroam.el --- Org-roam backlinks within org-mode buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; URL: https://github.com/NicolasPetton/nroam
;; Keywords: outlines, convenience
;; Version: 0.9.0
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
;; To setup nroam for all org-roam buffers, evaluate the following:
;; (add-hook 'org-mode-hook #'nroam-setup-maybe)

;;; Code:

(require 'org-element)
(require 'org-capture)
(require 'seq)
(require 'subr-x)
(require 'bookmark)

(require 'nroam-utils)
(require 'nroam-backlinks)
(require 'nroam-unlinked)

(defcustom nroam-sections nil
 "List of functions to be called to insert sections in nroam buffers."
 :group 'nroam
 :type '(repeat function))

(defun nroam-register-section (function)
  "Add FUNCTION as a section in nroam."
  (add-to-list 'nroam-sections function t))

(defvar-local nroam-start-marker nil)
(defvar-local nroam-end-marker nil)

(defun nroam--handle-org-capture (&rest _)
  "Setup the `org-capture' buffer.

Nroam sections need to be pruned as they are in read-only,
otherwise `org-capture' will fail to insert the capture
template."
  (when-let ((buf (org-capture-get :buffer)))
    (with-current-buffer buf
      (nroam--prune))))

(advice-add 'org-capture-place-template :before #'nroam--handle-org-capture)

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

;;;###autoload
(defun nroam-setup-maybe ()
  "Setup nroam for the current buffer iff an org-roam buffer."
  (when (nroam--org-roam-file-p)
    (nroam-mode)))

;;;###autoload
(define-minor-mode nroam-mode
  "Show nroam sections at the end of org-roam buffers."
  :lighter "nroam"
  :keymap nroam-mode-map
  (if nroam-mode
      (progn
        (add-hook 'before-save-hook #'nroam--prune nil t)
        (add-hook 'after-save-hook #'nroam--update-maybe nil t)
        (nroam--maybe-insert-immediately))
    (remove-hook 'before-save-hook #'nroam--prune t)
    (remove-hook 'after-save-hook #'nroam--update-maybe t)
    (nroam--prune)))

(defun nroam--maybe-insert-immediately ()
  "Insert nroam sections iff the buffer file exists."
  (when (file-exists-p buffer-file-name)
    (nroam-update)))

;;;###autoload
(defun nroam-ctrl-c-ctrl-c ()
  "Update the sections for the current buffer, or fallback to `org-ctrl-c-ctrl-c'."
  (interactive)
  (if (nroam--point-at-section-p)
      (nroam-update)
    (call-interactively (if org-capture-mode
                            #'org-capture-finalize
                          #'org-ctrl-c-ctrl-c))))

;;;###autoload
(defun nroam-return ()
  "Open nroam link at point, or fallback to `org-return'."
  (interactive)
  (if (nroam--point-at-section-p)
      (nroam--follow-link)
    (call-interactively #'org-return)))

;;;###autoload
(defun nroam-update ()
  "Update org-roam sections for the current buffer."
  (interactive)
  (nroam--setup-markers)
  (nroam--prune)
  (nroam--insert))

(defun nroam--org-roam-file-p ()
  "Return non-nil if the current buffer is an org-roam buffer."
  (org-roam--org-roam-file-p))

(defun nroam--point-at-section-p ()
  "Return non-hil if point if on the backlinks section."
  (when (nroam--sections-inserted-p)
    (when-let* ((beg (marker-position nroam-start-marker))
                (end (marker-position nroam-end-marker)))
      (<= beg (point) end))))

(defun nroam--update-maybe ()
  "Update backlinks when in nroam-mode."
  (when nroam-mode
    (nroam-update)))

(defun nroam--setup-markers ()
  "Setup the current buffer with markers for nroam."
  (unless (nroam--sections-inserted-p)
    (progn
      (setq nroam-start-marker (make-marker))
      (setq nroam-end-marker (make-marker)))))

(defun nroam--sections-inserted-p ()
  "Return non-nil if the current buffer has nroam sections inserted."
  (and (markerp nroam-start-marker)
       (marker-position nroam-start-marker)))

(defun nroam--prune ()
  "Remove nroam sections from the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (with-buffer-modified-unmodified
       (when (nroam--sections-inserted-p)
         (delete-region nroam-start-marker nroam-end-marker))))))

(defun nroam--insert ()
  "Insert nroam sections in the current buffer."
  (with-buffer-modified-unmodified
   (save-excursion
     (goto-char (point-max))
     (unless (bobp)
       (nroam--ensure-empty-line))
     (with-nroam-markers
       (nroam--do-separated-by-newlines #'funcall nroam-sections))
     (nroam--set-sections-visibility))))

(defun nroam--set-sections-visibility ()
  "Set nroam section visibility according to `org-set-startup-visibility'."
  (when (nroam--sections-inserted-p)
    (save-restriction
      (narrow-to-region nroam-start-marker nroam-end-marker)
      (org-set-startup-visibility))))

(defun nroam--follow-link ()
  "Follow backlink at point."
  (when (get-text-property (point) 'nroam-link)
    (let ((file (get-text-property (point) 'file))
          (point (get-text-property (point) 'point)))
      (org-open-file file t)
      (goto-char point))))

(nroam-backlinks-register-section)
(nroam-unlinked-register-section)

(provide 'nroam)
;;; nroam.el ends here
