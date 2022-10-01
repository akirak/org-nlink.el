;;; consult-org-nlink.el --- Consult interface for org-nlink -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org-nlink "0.1") (consult "0.19"))
;; Keywords: convenience outlines
;; URL: https://github.com/akirak/org-nlink.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `consult-org-nlink-insert' inserts a link to a heading or a target in the
;; file.

;;; Code:

(require 'consult)
(require 'org-nlink)

(defgroup consult-org-nlink nil
  "Consult interface for org-nlink."
  :group 'consult
  :group 'org-nlink)

(defun consult-org-nlink--sources ()
  "Return consult sources."
  (let ((plist (org-nlink-build-cache)))
    (list (list :name "Targets"
                :narrow ?t
                :category 'org-nlink-target
                :annotate #'org-nlink-annotate-target
                :items (plist-get plist :targets))
          (list :name "Headings"
                :narrow ?h
                :category 'org-nlink-heading
                :items (plist-get plist :headings)))))

;;;###autoload
(defun consult-org-nlink-insert (&optional arg)
  "Insert a link to an in-buffer target."
  (interactive "P")
  (let* ((data (org-nlink-thing arg))
         (ent (consult--multi (consult-org-nlink--sources)
                              :prompt "Insert a link: "
                              :initial (or (cadr data)
                                           (when data
                                             (buffer-substring-no-properties
                                              (caar data) (cdar data))))
                              :sort nil)))
    (atomic-change-group
      (when data
        (delete-region (caar data) (cdar data)))
      (if (plist-get (cdr ent) :match)
          (cl-case (plist-get (cdr ent) :category)
            (org-nlink-target
             (consult-org-nlink--insert-target-link (car ent)))
            (org-nlink-heading
             (consult-org-nlink--insert-heading-link (car ent))))
        (org-nlink-insert-new-link (car ent) (cddr data))))))

(defun consult-org-nlink--insert-target-link (target)
  "Insert a link to TARGET."
  (if-let (plist (gethash target org-nlink-target-cache))
      (insert (if (plist-get plist :radio)
                  target
                (format "[[%s]]" target)))
    (error "Not found %s" target)))

(defun consult-org-nlink--insert-heading-link (olp-text)
  "Insert a link to a heading."
  (if-let (plist (gethash olp-text org-nlink-heading-cache))
      (progn
        (org-with-point-at (plist-get plist :marker)
          (let ((inhibit-message t))
            (org-store-link nil 'interactive)))
        (let ((link (pop org-stored-links)))
          (insert (org-link-make-string (car link)
                                        (read-string "Description: "
                                                     (nth 1 link))))))
    (error "Not found %s" olp-text)))

(provide 'consult-org-nlink)
;;; consult-org-nlink.el ends here
