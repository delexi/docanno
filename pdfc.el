;;; pdfc.el --- Control pdf programs from within emacs.

;; Copyright (C) 2014 Alexander Baier

;; Author: Alexander Baier <alexander.baier@mailbox.org>
;; Keywords: convenience
;; Version: 0.0.3
;; Package-Requires: ((s "1.9.0"))

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

;;; Commentary:
;;
;; Control a PDF reader from inside emacs.
;; - goto previous/next page
;; - goto a certain page
;; - insert org headline/list item/table entry with page number
;;
;;; Code:

(require 's)
(require 'cl)

(defvar pdfc--current-backend nil
  "The backend currently active.")

(defvar pdfc--current-viewer nil
  "The viewer currently active.")

(defvar pdfc--backends nil
  "A plist representing the defined backends.")

(defvar pdfc--viewers nil
  "A plist representing the defined viewers.")

(defvar pdfc--current-page-num 1
  "The page of the document currently displayed.")

(defvar pdfc--current-file-name nil
  "The file name of the document currently displayed.")

(defvar pdfc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-n") 'pdfc-next-page)
    (define-key map (kbd "C-c M-p") 'pdfc-previous-page)
    (define-key map (kbd "C-c M-d") 'pdfc-display-pdf)
    (define-key map (kbd "C-c M-v") 'pdfc-set-viewer)
    (define-key map (kbd "C-c M-f") 'pdfc-set-file-name)
    (define-key map (kbd "C-c i") 'pdfc-insert-page)
    (define-key map (kbd "C-c M-i") 'pdfc-insert-note-and-page)
    map))

(defvar pdfc-command-hook nil
  "Runs after every invocation of the pdf command.")

(defvar pdfc-auto-update-file-path t)

(defvar pdfc--page-num-hash nil)

(define-minor-mode pdfc-mode "Control pdf programs from within emacs."
  nil "PDFc" pdfc-mode-map
  (if pdfc-mode
      (progn
        (make-local-variable 'pdfc--current-backend)
        (make-local-variable 'pdfc--current-viewer)
        (make-local-variable 'pdfc--current-page-num)
        (make-local-variable 'pdfc--current-file-name)
        (set (make-local-variable 'pdfc--page-num-hash)
             (make-hash-table :test #'equal :size 10))
        (cl-loop for backend in pdfc--backends by #'cddr
                 when (eql (pdfc-backend-get :mode backend) major-mode)
                 return (pdfc-set-backend backend)))))

(defun pdfc-define-backend (name &rest keywords)
  "Define a new backend accessible by NAME."
  (declare (indent defun))
  (setq pdfc--backends 
        (plist-put pdfc--backends name (plist-put keywords :name name))))

(defun pdfc-define-viewer (name &rest keywords)
  "Define the configuration for a new viewer accessible by NAME."
  (declare (indent defun))
  (setq pdfc--viewers
        (plist-put pdfc--viewers name (plist-put keywords :name name))))

(defun pdfc--generic-get (default store error-label key &optional opt)
  (when (null store)
    (user-error "No %s is activated. Activate one via pdfc-set-%s" error-label))
  (lax-plist-get (or (lax-plist-get store (or opt default))
                     (error "There is no %s defined by the name \"%s\""
                            error-label (or opt default))) key))

(defun pdfc-backend-get (key &optional backend)
  "Return the value of KEY in current backend.

If BACKEND is a string it is used instead of the current backend.
If no backend has been activated yet, throw a user-error."
  (pdfc--generic-get pdfc--current-backend pdfc--backends "backend" key backend))

(defun pdfc-viewer-get (key &optional viewer)
  "Return the value of KEY in current viewer.

If VIEWER is a string it is used instead of the current viewer.
If no viewer has been activated yet, throw a user-error."
  (pdfc--generic-get pdfc--current-viewer pdfc--viewers "viewer" key viewer))

(defvar pdfc-backend-hook nil
  "This hook is run after changing the current backend.")
(defvar pdfc-viewer-hook nil
  "This hook is run after changing the current viewer.")

(defun pdfc-set-viewer (viewer)
  "Make VIEWER the current viewer.

When called interatively, completion over the currently activated
viewers is available."
  (interactive (list (completing-read
                      "Select a viewer: "
                      (cl-loop for v in pdfc--viewers by #'cddr collect v))))
  (setq pdfc--current-viewer viewer)
  (run-hooks 'pdfc-viewer-hook))

(defun pdfc-set-backend (backend)
  "Make BACKEND the current backend.

When called interatively, completion over the currently activated
backends is available."
  (interactive (list (completing-read
                      "Select a backend: "
                      (cl-loop for b in pdfc--backends by #'cddr collect b))))
  (setq pdfc--current-backend backend)
  (run-hooks 'pdfc-backend-hook))

(defun pdfc-set-file-name (file &optional no-guess)
  "Make FILE the current file.

When called interactively, completion over files returned by the
backend property :file-name is available.
With universal argument there is no such completion."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (let (insert-default-directory)
               (read-file-name
                "Find PDF file to control: " nil nil t nil
                (lambda (f) (or (file-directory-p f)
                           (string-suffix-p ".pdf" f t)))))
           (let ((guesses (funcall (pdfc-backend-get :file-name))))
             (completing-read 
              "Select PDF to control: "
              guesses nil 'confirm nil nil guesses)))
         current-prefix-arg))
  (when (file-exists-p file)
    ;; Save the page of the current file.
    (puthash pdfc--current-file-name pdfc--current-page-num pdfc--page-num-hash)
    ;; Restore the page of the new file.
    (setq pdfc--current-page-num (or (gethash file pdfc--page-num-hash) 1))
    (setq pdfc--current-file-name (expand-file-name file))))

(defun pdfc--maybe-change-file-name ()
  "Use the backend property :file-name to set `pdfc--current-file-name'.

Calls `pdfc-set-file-name' with the first file name returned by
backend property :file-name if it is non-nil and is not the
current file name.
Does nothing if `pdfc-auto-update-file-path' is nil."
  (when pdfc-auto-update-file-path
    (let ((file (car (funcall (pdfc-backend-get :file-name)))))
      (when (or (and file (not (string= file pdfc--current-file-name)))
                (not pdfc--current-file-name))
        (pdfc-set-file-name file)))))

(defun pdfc--check-file-name ()
  (cond ((null pdfc--current-file-name)
         (user-error "Document has not been set"))
        ((not (file-exists-p pdfc--current-file-name))
         (user-error "\"%s\" does not exist" pdfc--current-file-name))))

(defun pdfc-display-pdf (&optional page)
  "Display the current document at PAGE."
  (interactive "p")
  (pdfc--check-file-name)
  (when page (setq pdfc--current-page-num page))
  (let ((display-cmd (pdfc-viewer-get :display)))
    (cond
     ((functionp display-cmd)
      (funcall display-cmd pdfc--current-file-name pdfc--current-page-num))
     ((stringp display-cmd)
      (start-process-shell-command
       display-cmd (get-buffer-create " *pdfc-display-process*")
       (pdfc--replace-placeholder display-cmd pdfc--current-page-num)))
     (t (user-error "The value of `pdfc-command-alist' is not correct"))))
  (run-hooks 'pdfc-command-hook))

(defun pdfc--replace-placeholder (command page)
  (s-replace-all `(("%f" . ,(replace-quote
                             (shell-quote-argument
                              (expand-file-name pdfc--current-file-name))))
                   ("%p" . ,(int-to-string page)))
                 command))

(defun pdfc-previous-page (&optional count)
  "Display previous page of current document.

If COUNT is given, go back COUNT pages."
  (interactive "p")
  (pdfc--maybe-change-file-name)
  (pdfc-display-pdf (- pdfc--current-page-num (or count 1))))

(defun pdfc-next-page (&optional count)
  "Display next page of current document.

If COUNT is given, go forward COUNT pages."
  (interactive "p")
  (pdfc--maybe-change-file-name)
  (pdfc-display-pdf (+ pdfc--current-page-num (or count 1))))

(defun pdfc-insert-page ()
  "Insert the current page at point."
  (interactive)
  (insert (funcall (pdfc-backend-get :format-page) pdfc--current-page-num)))

(defun pdfc-insert-note ()
  "Insert a new note at point."
  (interactive)
  (let ((new-note (pdfc-backend-get :new-note)))
    (cond ((stringp new-note)
           (insert new-note))
          ((functionp new-note)
           (let ((ret (funcall new-note)))
             (when (stringp ret)
               (insert ret))))
          (t (error ":new-note must either be a string or a function")))))

(defun pdfc-insert-note-and-page ()
  "Insert a new note and the current page at point."
  (interactive)
  (pdfc-insert-note)
  (pdfc-insert-page)
  (let ((sep (pdfc-backend-get :separator)))
    (cond ((stringp sep) (insert sep))
          ((functionp sep) (insert (or (funcall sep) ""))))))

(provide 'pdfc)

;;; pdfc.el ends here
