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

(defcustom consult-org-nlink-insert-super-link t
  "Whether to insert a super link to the selected heading.

If this option is t, `consult-org-nlink-insert' command insert a
super link when the user selects a heading. You need
`org-super-links' library to use this feature."
  :type 'boolean)

(defun consult-org-nlink--sources ()
  "Return consult sources."
  (let ((plist (org-nlink-build-cache :files (org-nlink--default-files))))
    (list (list :name "Targets"
                :narrow ?t
                :category 'org-nlink-target
                :annotate #'org-nlink-annotate-target
                :items (plist-get plist :targets))
          (list :name "Headings"
                :narrow ?h
                :category 'org-nlink-heading
                :annotate #'org-nlink-annotate-heading
                :items (plist-get plist :headings)))))

;;;###autoload
(defun consult-org-nlink-insert (&optional arg)
  "Insert a link to an in-buffer target."
  (interactive "P")
  (pcase-let*
      ((`((,begin . ,end) . (,link . ,text)) (org-nlink-thing arg))
       (initial (or link
                    (when (and begin end)
                      (org-nlink--sanitize-target
                       (buffer-substring-no-properties begin end)))))
       (desc (or text
                 link
                 (when (and begin end)
                   (buffer-substring-no-properties begin end))))
       (`(,sel . ,plist) (let ((completion-ignore-case t))
                           (consult--multi (consult-org-nlink--sources)
                                           :prompt "Insert a link to a target or heading: "
                                           :initial initial
                                           :sort nil))))
    (atomic-change-group
      (when begin
        (delete-region begin end))
      (if (plist-get plist :match)
          (cl-case (plist-get plist :category)
            (org-nlink-target
             (consult-org-nlink--insert-target-link sel desc))
            (org-nlink-heading
             (consult-org-nlink--insert-heading-link sel desc)))
        (org-nlink-insert-new-link sel desc)))))

(defun consult-org-nlink--insert-target-link (target &optional text)
  "Insert a link to TARGET."
  (if-let (plist (gethash target org-nlink-target-cache))
      (insert (if (plist-get plist :radio)
                  target
                (if (and text
                         (string-equal-ignore-case target text))
                    (org-link-make-string text)
                  (let ((description (or text
                                         (read-from-minibuffer "Description: " target))))
                    (org-link-make-string target
                                          (unless (equal description target)
                                            description))))))
    (error "Not found %s" target)))

(defun consult-org-nlink--insert-heading-link (olp-text &optional text)
  "Insert a link to a heading."
  (if-let (plist (gethash olp-text org-nlink-heading-cache))
      (if (and consult-org-nlink-insert-super-link
               (or (require 'org-super-links nil t)
                   (progn
                     (message "org-super-links is not available")
                     nil)))
          (progn
            (org-with-point-at (plist-get plist :marker)
              (let ((inhibit-message t))
                (org-super-links-store-link)))
            (save-excursion
              (org-super-links-insert-link))
            (when-let (link (and (thing-at-point org-link-bracket-re)
                                 (match-string-no-properties 1)))
              (delete-region (match-beginning 0) (match-end 0))
              (insert (org-link-make-string
                       link
                       (or text
                           (read-from-minibuffer "Description: " (match-string-no-properties 1)))))))
        (org-with-point-at (plist-get plist :marker)
          (let ((inhibit-message t))
            (org-store-link nil 'interactive)))
        (let ((link (pop org-stored-links)))
          (insert (org-link-make-string
                   (car link)
                   (or text
                       (read-from-minibuffer "Description: " (nth 1 link)))))))
    (error "Not found %s" olp-text)))

(provide 'consult-org-nlink)
;;; consult-org-nlink.el ends here
