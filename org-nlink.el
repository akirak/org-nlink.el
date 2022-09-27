;;; org-nlink.el --- Utilities for in-file links -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.2"))
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

;; `org-nlink-occur' command lets you choose a target and displays an occur
;; buffer of the target.

;;; Code:

(require 'org)

(defgroup org-nlink nil
  "Utilities for Org in-file links."
  :group 'org)

(defvar org-nlink-target-cache nil)
(defvar org-nlink-heading-cache nil)

(defun org-nlink-read-target (prompt)
  "Returns a cons cell of a chosen target and its plist."
  (let ((target (completing-read prompt (org-nlink-target-completion))))
    (when target (cons target (gethash target org-nlink-target-cache)))))

;;;###autoload
(defun org-nlink-occur (target &optional radio)
  "Find links to a target in the buffer."
  (interactive (let ((result (org-nlink-read-target "Target: ")))
                 (list (car result) (plist-get (cdr result) :radio))))
  (org-occur (if radio
                 (regexp-quote target)
               (rx-to-string `(and "[[" ,target
                                   "]"
                                   (?  "[" (+? anything) "]")
                                   "]")))))

(defun org-nlink-target-completion ()
  "Return a completion table for targets."
  (let ((items (plist-get (org-nlink-build-cache :skip-headings t) :targets)))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . org-nlink-target)
                         (annotation-function . org-nlink-annotate-target)))
         (complete-with-action action ',items string pred)))))

(cl-defun org-nlink-build-cache (&key skip-headings)
  "Update cache variables for targets and headings."
  (let ((entries (org-nlink--scan-1))
        (width (frame-width))
        headings
        targets)

    (if org-nlink-target-cache
        (clrhash org-nlink-target-cache)
      (setq org-nlink-target-cache
            (make-hash-table :test #'equal)))

    (unless skip-headings
      (if org-nlink-heading-cache
          (clrhash org-nlink-heading-cache)
        (setq org-nlink-heading-cache
              (make-hash-table :test #'equal :size (length entries)))))

    (dolist (entry-plist entries)
      (unless skip-headings
        (let* ((olp (plist-get entry-plist :olp))
               (heading (plist-get entry-plist :heading))
               (olp-text (org-format-outline-path
                          (append (butlast olp)
                                  (list heading))
                          width)))
          (remove-text-properties 0 (- (length olp-text) (length heading))
                                  '(face) olp-text)
          (puthash olp-text entry-plist org-nlink-heading-cache)
          (push olp-text headings)))

      (pcase-dolist (`(,target . ,plist) (plist-get entry-plist :targets))
        (puthash target
                 (append (list :olp (plist-get entry-plist :olp))
                         plist)
                 org-nlink-target-cache)
        (push target targets)))

    (list :targets targets
          :headings headings)))

(defun org-nlink-annotate-target (target)
  "Completion annotation function for `org-nlink-target'."
  (when-let (plist (gethash target org-nlink-target-cache))
    (concat (propertize (thread-last
                          (plist-get plist :olp)
                          (org-format-outline-path)
                          (format " (%s)"))
                        'face 'font-lock-doc-face))))

(defun org-nlink--scan-1 (&optional buffer)
  "Return a list of plists containing entry information."
  (with-current-buffer (org-base-buffer (or buffer (current-buffer)))
    (unless (derived-mode-p 'org-mode)
      (user-error "This function must be run in org-mode"))
    (org-with-wide-buffer
     (let ((use-cache nil))
       (org-map-entries
        (lambda ()
          (prog1 (list :olp (mapcar #'org-link-display-format
                                    (org-get-outline-path 'self use-cache))
                       :marker (point-marker)
                       :targets (org-nlink--scan-targets (org-entry-end-position))
                       :heading (org-link-display-format (org-get-heading)))
            (unless use-cache (setq use-cache t)))))))))

(defun org-nlink--scan-targets (&optional bound)
  "Return a list of link targets."
  (let (result)
    (save-match-data
      (while (re-search-forward org-target-regexp bound t)
        (push (list (match-string-no-properties 1)
                    :point (point)
                    :radio (looking-at ">"))
              result)))
    result))

(provide 'org-nlink)
;;; org-nlink.el ends here
