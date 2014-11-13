;;; pdfc-doc-view.el --- doc-view viewer for pdfc.

;; Copyright (C) 2014 Alexander Baier

;; Author: Alexander Baier <alexander.baier@mailbox.org>
;; Keywords: convenience
;; Version: 0.0.3
;; Require-Package: ((pdfc "0.0.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; 
;;; Commentary:
;;
;; Supported viewer functionality:
;; - :display
;;
;;; Code:

(require 'pdfc)

(defvar pdfc-doc-view--buffer-hash)

(add-hook 'pdfc-viewer-hook
          (defun pdfc-doc-view--setup ()
            (set (make-local-variable 'pdfc-doc-view--buffer-hash)
                 (make-hash-table :test #'equal :size 10))))

(defun pdfc-doc-view-display-cmd (file page)
  (let ((pdf-buf (gethash file pdfc-doc-view--buffer-hash)))
    ;; If file is not yet in our hash table see if it is open in emacs
    ;; and if it is, put it into the table.
    (and (not pdf-buf)
         (setq pdf-buf (get-file-buffer file))
         (puthash file pdf-buf pdfc-doc-view--buffer-hash))
    (if pdf-buf
        (with-selected-window
            (or (get-buffer-window pdf-buf)
                ;; Display buffer if it is currently not displayed.
                (display-buffer pdf-buf t))
          (doc-view-goto-page page))
      ;; file is not yet open in emacs.
      (let ((sw (selected-window)))
        (find-file-other-window file)
        (puthash file (current-buffer) pdfc-doc-view--buffer-hash)
        (select-window sw)))))

(pdfc-define-viewer "doc-view-mode"
  :display #'pdfc-doc-view-display-cmd)

(provide 'pdfc-doc-view)

;;; pdfc-doc-view.el ends here
