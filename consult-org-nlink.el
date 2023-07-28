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

(declare-function org-super-links-insert-link "ext:org-super-links")
(declare-function org-super-links-store-link "ext:org-super-links")

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
(cl-defun consult-org-nlink-insert (begin end &key link text)
  "Insert a link to an in-buffer target."
  (interactive (pcase-exhaustive (org-nlink-thing current-prefix-arg)
                 (`((,begin . ,end) . ,rest)
                  (list begin end :link (car rest) :text (cdr rest)))
                 (`nil
                  (list nil nil))))
  (let* ((initial (or link
                      (when (and begin end)
                        (org-nlink--sanitize-target
                         (buffer-substring-no-properties begin end)))))
         (desc (or text
                   link
                   (when (and begin end)
                     (buffer-substring-no-properties begin end))))
         (completion-ignore-case t))
    (pcase (consult--multi (consult-org-nlink--sources)
                           :prompt "Insert a link to a target or heading: "
                           :initial initial
                           :sort nil)
      (`(,sel . ,plist)
       (atomic-change-group
         (when begin
           (delete-region begin end))
         (if (plist-get plist :match)
             (cl-case (plist-get plist :category)
               (org-nlink-target
                (consult-org-nlink--insert-target-link sel desc))
               (org-nlink-heading
                (consult-org-nlink--insert-heading-link sel desc)))
           (org-nlink-insert-new-link sel desc)))))))

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
            (when-let (link (and (thing-at-point-looking-at org-link-bracket-re)
                                 (match-string-no-properties 1)))
              (let ((description (match-string-no-properties 2)))
                (delete-region (match-beginning 0) (match-end 0))
                (insert (org-link-make-string
                         link
                         (or text
                             (consult-org-nlink--ask-description description)))))))
        (org-with-point-at (plist-get plist :marker)
          (let ((inhibit-message t))
            (org-store-link nil 'interactive)))
        (let ((link (pop org-stored-links)))
          (insert (org-link-make-string
                   (car link)
                   (or text
                       (consult-org-nlink--ask-description
                        (nth 1 link)))))))
    (error "Not found %s" olp-text)))

(defun consult-org-nlink--ask-description (default)
  (let ((string (read-from-minibuffer
                 (format-prompt "Description" default)
                 nil nil nil nil default
                 'inherit)))
    (unless (or (string-empty-p string)
                (equal string default))
      string)))

;;;###autoload
(defun consult-org-nlink-isearch ()
  "Convert the highlighted text in isearch to an Org link.

You can bind this command in `isearch-mode-map'."
  (interactive)
  (pcase-exhaustive isearch-match-data
    (`(,begin ,end . ,_)
     ;; Normalize the matched string into one that doesn't contain newlines.
     (pcase-let*
         ((`(,begin . ,end) (save-excursion
                              (goto-char begin)
                              (when (thing-at-point-looking-at (regexp-quote isearch-string))
                                (cons (match-beginning 0) (match-end 0)))))
          (`(,begin . ,end) (if (use-region-p)
                                (cons (min begin (region-beginning))
                                      (max end (region-end)))
                              (cons begin end)))
          (string (buffer-substring-no-properties begin end))
          (link (org-nlink--sanitize-target string))
          (text (org-nlink--sanitize-target-1 string)))
       ;; `isearch-mode' affect the active maps, so exit from it.
       (isearch-done)
       (consult-org-nlink-insert begin end :link link :text text)))))

(provide 'consult-org-nlink)
;;; consult-org-nlink.el ends here
