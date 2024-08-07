;;; org-nlink.el --- Utilities for in-file links -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (compat "29"))
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
(require 'compat-29)

;; Silence byte compiler
(defvar org-capture-entry)
(defvar org-refile-targets)
(defvar org-refile-target-verify-function)
(declare-function thing-at-point-looking-at "thingatpt")

(defgroup org-nlink nil
  "Utilities for Org in-file links."
  :group 'org)

(defconst org-nlink-word-regexp
  (rx (*? (or (syntax string-quote)
              (syntax character-quote)
              (syntax open-parenthesis)))
      word-start
      (group (+ (not (any space))))
      word-end
      (*? (or (syntax string-quote)
              (syntax character-quote)
              (syntax close-parenthesis)
              (syntax punctuation)))))

(defcustom org-nlink-extra-files #'ignore
  "A function to return extra Org files.

The function should return a list of Org files. `org-nlink'
package will look for link targets in the extra files as well."
  :type 'function)

(defcustom org-nlink-target-verify-function nil
  "Function to verify locations in `org-nlink-insert-new-link'.

See the documentation of `org-refile-target-verify-function' for
allowed values."
  :type '(choice (const nil) function))

(defvar org-nlink-target-cache nil)
(defvar org-nlink-heading-cache nil)

(defun org-nlink--extra-files ()
  "Return a list of extra Org files.

See `org-nlink-extra-files'."
  (funcall org-nlink-extra-files))

(defun org-nlink--default-files ()
  (seq-uniq (cons (thread-last
                    (org-base-buffer (current-buffer))
                    (buffer-file-name)
                    (abbreviate-file-name))
                  (org-nlink--extra-files))))

(defmacro org-nlink-with-cache-disabled (&rest body)
  "Evaluate BODY while org-element cache is disabled."
  `(if (boundp 'org-element-use-cache)
       (let ((orig-use-cache org-element-use-cache))
         (setq org-element-use-cache nil)
         (unwind-protect
             (progn ,@body)
           (setq org-element-use-cache orig-use-cache)))
     ,@body))

;;;###autoload
(defun org-nlink-open-link (link)
  "Search the target from the current files and extra files.

You can add this function to `org-open-link-functions'. This
function takes a single argument, TARGET, which is a url or fuzzy
link. Only fuzzy links are handled."
  (unless (string-match-p org-link-types-re link)
    (let ((regexp (regexp-quote (format "<<%s>>" link)))
          (orig-buffer (current-buffer)))
      (if-let (point (catch 'found-target
                       (dolist (file (org-nlink--default-files))
                         (set-buffer (or (org-find-base-buffer-visiting file)
                                         (find-file-noselect file)))
                         (org-with-wide-buffer
                          (goto-char (point-min))
                          (when (re-search-forward regexp nil t)
                            (throw 'found-target (match-beginning 0)))))))
          (progn
            (widen)
            (goto-char point))
        (set-buffer orig-buffer)
        nil))))

(defun org-nlink-read-target (prompt)
  "Returns a cons cell of a chosen target and its plist."
  (let ((target (completing-read prompt (org-nlink-target-completion
                                         (org-nlink--default-files)))))
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

(defun org-nlink-target-completion (&optional files)
  "Return a completion table for targets."
  (let (items)
    (cl-flet
        ((get-targets (&rest args)
           (plist-get (apply #'org-nlink-build-cache :skip-headings t args)
                      :targets)))
      (if files
          (let ((files (copy-sequence files)))
            (when-let (file (pop files))
              (setq items (get-targets :file file)))
            (dolist (file files)
              (setq items (append items (get-targets :no-clear-cache t :file file)))))
        (setq items (get-targets))))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . org-nlink-target)
                         ,@(when files
                             '((group-function . org-nlink-group-target-by-file)))
                         (annotation-function . org-nlink-annotate-target)))
         (complete-with-action action ',items string pred)))))

(cl-defun org-nlink-build-cache (&key skip-headings no-clear-cache files
                                      ;; Deprecated; Use files instead
                                      file)
  "Update cache variables for targets and headings."
  (let ((width (frame-width))
        headings
        targets)

    (if org-nlink-target-cache
        (unless no-clear-cache
          (clrhash org-nlink-target-cache))
      (setq org-nlink-target-cache
            (make-hash-table :test #'equal)))

    (unless skip-headings
      (if org-nlink-heading-cache
          (unless no-clear-cache
            (clrhash org-nlink-heading-cache))
        (setq org-nlink-heading-cache
              ;; Make the size customizable
              (make-hash-table :test #'equal :size 500))))

    (dolist (file (or files
                      (when file
                        (ensure-list file))
                      (list (buffer-file-name))))
      (let ((entries (org-nlink--scan-1 (or (find-buffer-visiting file)
                                            (find-file-noselect file)))))
        (dolist (entry-plist entries)
          (unless skip-headings
            (let* ((olp (plist-get entry-plist :olp))
                   (heading (org-no-properties (plist-get entry-plist :heading)))
                   (olp-text (org-format-outline-path
                              (append (list (file-name-nondirectory file))
                                      (butlast olp)
                                      (list heading))
                              width)))
              (remove-text-properties 0 (max 0 (- (length olp-text) (length heading)))
                                      '(face) olp-text)
              (puthash heading entry-plist org-nlink-heading-cache)
              (push heading headings)))

          (pcase-dolist (`(,target . ,plist) (plist-get entry-plist :targets))
            (puthash target
                     (append (list :olp (plist-get entry-plist :olp))
                             plist)
                     org-nlink-target-cache)
            (push target targets)))))

    (list :targets targets
          :headings headings)))

(defun org-nlink-annotate-target (target)
  "Completion annotation function for `org-nlink-target'."
  (when-let (plist (gethash target org-nlink-target-cache))
    (concat (propertize (thread-last
                          (plist-get plist :olp)
                          (cons (thread-last
                                  (plist-get plist :marker)
                                  (marker-buffer)
                                  (buffer-file-name)
                                  (file-name-nondirectory)))
                          (org-format-outline-path)
                          (format " (%s)"))
                        'face 'font-lock-doc-face))))

(defun org-nlink-group-target-by-file (target transform)
  (if transform
      target
    (when-let* ((plist (gethash target org-nlink-target-cache))
                (marker (plist-get plist :marker)))
      (thread-last
        marker
        (marker-buffer)
        (buffer-file-name)
        (abbreviate-file-name)))))

(defun org-nlink-annotate-heading (heading)
  "Completion annotation function for `org-nlink-target'."
  (when-let (plist (gethash heading org-nlink-heading-cache))
    (concat (propertize (thread-last
                          (butlast (plist-get plist :olp))
                          (org-format-outline-path)
                          (format " (%s)"))
                        'face 'font-lock-doc-face))))

(defun org-nlink--scan-1 (&optional buffer)
  "Return a list of plists containing entry information."
  (with-current-buffer (org-base-buffer (or buffer (current-buffer)))
    (unless (derived-mode-p 'org-mode)
      (user-error "This function must be run in org-mode"))
    (org-with-wide-buffer
     (let ((use-cache nil)
           (archive-regexp (regexp-quote (format ":%s:" org-archive-tag)))
           result)
       (org-nlink-with-cache-disabled
        (goto-char (point-min))
        (setq org-outline-path-cache nil)
        (while (re-search-forward org-complex-heading-regexp nil t)
          (if (and (match-string 5)
                   (string-match-p archive-regexp (match-string 5)))
              (org-end-of-subtree)
            (push (list :olp (mapcar #'org-link-display-format
                                     (org-get-outline-path 'self use-cache))
                        :marker (point-marker)
                        :targets (org-nlink--scan-targets (org-entry-end-position))
                        :heading (org-link-display-format (org-get-heading)))
                  result))
          (unless use-cache (setq use-cache t)))
        result)))))

(defun org-nlink--scan-targets (&optional bound)
  "Return a list of link targets."
  (let (result)
    (save-match-data
      (while (re-search-forward org-target-regexp bound t)
        (push (list (match-string-no-properties 1)
                    :marker (point-marker)
                    :radio (looking-at ">"))
              result)))
    result))

(defun org-nlink-thing (&optional n)
  "Return (bounds . (target . text)) for a thing at point.

If N is a number or '-, select words around the point. If the
number is positive, it selects words after the point. If
negative, it selects words before the point."
  (cond
   (n
    (cond
     ((not (or (numberp n) (eq n '-)))
      (error "N must be either a number or '-"))
     ((or (eq n '-) (< n 0))
      (when (eq n '-)
        (setq n -1))
      (if (thing-at-point-looking-at org-nlink-word-regexp)
          (save-excursion
            (let ((end (match-end 1)))
              (forward-word n)
              (cons (cons (point) end) nil)))
        (error "Cannot find a word start")))
     ((> n 0)
      (if (thing-at-point-looking-at org-nlink-word-regexp)
          (save-excursion
            (let ((start (match-beginning 1)))
              (forward-word n)
              (cons (cons start (point)) nil)))
        (error "Cannot find a word start")))
     (t
      (error "Unallowed value for N: %d" n))))
   ((use-region-p)
    (cons (car (region-bounds))
          nil))
   (t
    (save-match-data
      (if (and (get-char-property (point) 'org-emphasis)
               (when-let (face (get-text-property (point) 'face))
                 (and (listp face)
                      (cl-intersection face
                                       '(verbatim
                                         italic
                                         bold
                                         org-verbatim
                                         org-code)))))
          (cond
           ((thing-at-point-looking-at org-emph-re)
            (cons (org-nlink--matching-bound-1)
                  (cons (org-nlink--sanitize-target (match-string-no-properties 4))
                        nil)))
           ((thing-at-point-looking-at org-verbatim-re)
            (cons (org-nlink--matching-bound-1)
                  (cons (org-nlink--sanitize-target-1 (match-string-no-properties 4))
                        nil)))
           (t
            ;; Fallback
            (org-nlink--thing-fallback-1)))
        (org-nlink--thing-fallback-1))))))

(defun org-nlink--thing-fallback-1 ()
  "Fallback cases of `org-nlink-thing'."
  (cond
   ((thing-at-point-looking-at org-link-bracket-re)
    (cons (cons (match-beginning 0)
                (match-end 0))
          (cons (org-nlink--sanitize-target (match-string-no-properties 1))
                (match-string-no-properties 2))))
   ((thing-at-point-looking-at org-nlink-word-regexp)
    (cons (cons (match-beginning 1)
                (match-end 1))
          nil))))

(defun org-nlink--matching-bound-1 ()
  "Return a sane bound of inline matching."
  (cons (save-match-data
          (save-excursion
            (goto-char (match-beginning 0))
            ;; Apparently, org-emph-re can match text starting with
            ;; space. Skip space.
            (if (looking-at (rx (+ (or space (syntax open-parenthesis)))))
                (match-end 0)
              (point))))
        (save-match-data
          (save-excursion
            (goto-char (match-end 0))
            (if (looking-back (rx (+ (or space punct
                                         (syntax close-parenthesis))))
                              (match-beginning 0))
                (match-beginning 0)
              (point))))))

(defun org-nlink--sanitize-target (string)
  (let ((string (org-nlink--sanitize-target-1 string))
        (case-fold-search nil))
    (if (or (org-nlink--proper-noun-p string)
            (org-nlink--acronym-p string)
            (org-nlink--symbol-p string))
        string
      (downcase string))))

(defun org-nlink--sanitize-target-1 (string)
  (replace-regexp-in-string (rx (+ (any "\r\n\t "))) " " string))

(defun org-nlink--acronym-p (string)
  (string-match-p (rx bol anything (*? anything) upper) string))

(defun org-nlink--symbol-p (string)
  (not (string-match-p (rx bos (+ (any alnum space)) eos) string)))

(defun org-nlink--proper-noun-p (string)
  (seq-find #'org-nlink--capital-word-p (cdr (split-string string " "))))

(defun org-nlink--capital-word-p (string)
  (char-uppercase-p (string-to-char string)))

(defun org-nlink-insert-new-link (target &optional description)
  "Insert a link to a new target.

TARGET is the name of a link target. An optional DESCRIPTION is a
link description, which is added if the created link points to a
non-radio target."
  (pcase (read-char-choice (format "Create [r]adio or [n]on-radio target named %s.\
Optionally change the [d]escription (default %s)"
                                   target
                                   (when description
                                     (format "\"%s\"" description)))
                           (string-to-list "rnd"))
    (?r
     (insert target)
     (org-nlink--insert-target (format "<<<%s>>>" target)))
    (?n
     (insert (org-link-make-string target description))
     (org-nlink--insert-target (format "<<%s>>" target)))
    (?d
     (insert (org-link-make-string target (read-string "Description: " description)))
     (org-nlink--insert-target (format "<<%s>>" target)))))

(defun org-nlink--insert-target (string)
  "Insert a link target into a file.

STRING is a string of the link target. It is usually wrapped in a
pair of two or three angles."
  (let ((org-refile-targets (thread-last
                              (org-nlink--default-files)
                              (mapcar (lambda (file)
                                        `(,file :maxlevel . 99)))))
        (org-refile-target-verify-function org-nlink-target-verify-function)
        (org-capture-entry `("" ""
                             item
                             (function
                              (lambda ()
                                (pcase (org-refile-get-location
                                        ,(format "Location of %s: " string)
                                        nil t)
                                  (`(,_ ,file ,_ ,point)
                                   (switch-to-buffer (org-find-base-buffer-visiting file))
                                   (widen)
                                   (goto-char point)))))
                             ,(concat string "%?")
                             ;; Avoid annoying warnings
                             :after-finalize
                             ,(if (string-match-p org-radio-target-regexp string)
                                  #'org-nlink--update-radio
                                #'org-nlink--cache-reset))))
    (org-nlink-with-cache-disabled
     (org-capture))))

(defun org-nlink--update-radio ()
  (let ((parent-buffer (org-base-buffer (current-buffer))))
    (with-current-buffer parent-buffer
      (org-update-radio-target-regexp))
    (org-nlink-update-radio-target-regexps parent-buffer)
    (org-nlink--cache-reset)))

(defun org-nlink-update-radio-target-regexps (parent-buffer)
  (let ((new-regexp (buffer-local-value 'org-target-link-regexp parent-buffer)))
    (dolist (buffer (buffer-list))
      (when (and (not (equal buffer parent-buffer))
                 (equal (buffer-base-buffer buffer)
                        parent-buffer))
        (with-current-buffer parent-buffer
          (when (and (derived-mode-p 'org-mode)
                     (not (equal org-target-link-regexp new-regexp)))
            (setq-local org-target-link-regexp new-regexp)
            (save-excursion
              (goto-char (point-min))
              (org-activate-target-links nil))))))))

(defun org-nlink--cache-reset ()
  (when (fboundp 'org-element-cache-reset)
    (org-element-cache-reset)))

(defun org-nlink--refresh-cache ()
  (when (fboundp 'org-element-cache-refresh)
    (org-element-cache-refresh (point))))

;;;###autoload
(defun org-nlink-insert-plain (files)
  "Insert a target from FILES."
  (insert (completing-read "Insert: " (org-nlink-target-completion files))))

(provide 'org-nlink)
;;; org-nlink.el ends here
