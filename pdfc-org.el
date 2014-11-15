;;; pdfc-org.el --- Org mode backend for pdfc.

;; Copyright (C) 2014 Alexander Baier

;; Author: Alexander Baier <alexander.baier@mailbox.org>
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((s "1.9.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundatiofn, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:


(require 'pdfc)

(defvar pdfc-org-page-note-separators '((headline . ": ")
                                        (item . " :: "))
  "The strings used to separate the page number and the note.

Each element in this alist looks like \(TYPE . SEPARATOR).
TYPE is one of headline or item symbolizing a note as a
headline or an item in a list.
SEPARATOR is the string used to separate the page number and the
following note.")

(defvar pdfc-org-doc-property "PDFC_FILE")

(defun pdfc-org-extract-page ()
  (save-excursion
    (let ((eol (progn (end-of-line) (point)))
          (page-rx (rx-to-string
                    `(and (or (regexp ,(org-item-re))
                              (regexp ,org-heading-regexp))
                          (group-n 1 (1+ digit))))))
      (beginning-of-line)
      (when (re-search-forward page-rx eol t)
        (match-string 1)))))

(defun pdfc-org-get-separator ()
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at org-heading-regexp)
           (cdr (assoc 'headline pdfc-org-page-note-separators)))
          ((looking-at (org-item-re))
           (cdr (assoc 'item pdfc-org-page-note-separators)))
          (t ""))))

(defun pdfc-org-insert-heading (&optional arg invisible-ok)
  (interactive "P")
  (org-insert-heading arg invisible-ok)
  (when (looking-at " :: ")
    (delete-char 4))
  nil)

(defun pdfc-org-guess-file-paths ()
  (let ((files (org-entry-get (point) pdfc-org-doc-property 'selective)))
    (if (and files (string-match "\\S-" files))
        (car (read-from-string (concat "(" files ")")))
      '(""))))

(pdfc-define-backend "org-mode"
  :format-page #'int-to-string
  :extract-page #'pdfc-org-extract-page
  :separator #'pdfc-org-get-separator
  :new-note #'pdfc-org-insert-heading
  :file-name #'pdfc-org-guess-file-paths
  :mode '(org-mode))

(provide 'pdfc-org)

;;; pdfc.el ends here
