;;; docanno.el --- Control and annotate documents from emacs.

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

(defvar docanno--current-backend nil
  "The backend currently active.")

(defvar docanno--current-viewer nil
  "The viewer currently active.")

(defvar docanno--backends nil
  "A plist representing the defined backends.")

(defvar docanno--viewers nil
  "A plist representing the defined viewers.")

(defvar docanno--current-page-num 1
  "The page of the document currently displayed.")

(defvar docanno--current-file-name nil
  "The file name of the document currently displayed.")

(defvar docanno-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-n") 'docanno-next-page)
    (define-key map (kbd "C-c M-p") 'docanno-previous-page)
    (define-key map (kbd "C-c M-d") 'docanno-display-doc)
    (define-key map (kbd "C-c M-v") 'docanno-set-viewer)
    (define-key map (kbd "C-c M-f") 'docanno-set-file-name)
    (define-key map (kbd "C-c i") 'docanno-insert-page)
    (define-key map (kbd "C-c M-i") 'docanno-insert-note-and-page)
    map))

(defvar docanno-viewer-hook nil
  "Runs after every invocation of the pdf command.")

(defvar docanno-auto-update-file-path t)

(defvar docanno-file-suffixes '(".pdf" ".ps")
  "File suffixes shown in `docanno-set-file-name' completion.")

(defvar docanno--page-num-hash nil)

;;;###autoload
(define-minor-mode docanno-mode "Control and annotate documents from emacs."
  nil "Docanno" docanno-mode-map
  (if docanno-mode
      (progn
        (make-local-variable 'docanno--current-backend)
        (make-local-variable 'docanno--current-viewer)
        (make-local-variable 'docanno--current-page-num)
        (make-local-variable 'docanno--current-file-name)
        (set (make-local-variable 'docanno--page-num-hash)
             (make-hash-table :test #'equal :size 10))
        (cl-loop for backend in docanno--backends by #'cddr
                 when (member major-mode (docanno-backend-get :mode backend))
                 return (docanno-set-backend backend)))))

(defun docanno-define-backend (name &rest keywords)
  "Define a new backend accessible by NAME."
  (declare (indent defun))
  (setq docanno--backends 
        (plist-put docanno--backends name (plist-put keywords :name name))))

(defun docanno-define-viewer (name &rest keywords)
  "Define the configuration for a new viewer accessible by NAME."
  (declare (indent defun))
  (setq docanno--viewers
        (plist-put docanno--viewers name (plist-put keywords :name name))))

(defun docanno--generic-get (default store error-label key &optional opt)
  (when (null store)
    (user-error "No %s is activated. Activate one via docanno-set-%s" error-label))
  (lax-plist-get (or (lax-plist-get store (or opt default))
                     (error "There is no %s defined by the name \"%s\""
                            error-label (or opt default))) key))

(defun docanno-backend-get (key &optional backend)
  "Return the value of KEY in current backend.

If BACKEND is a string it is used instead of the current backend.
If no backend has been activated yet, throw a user-error."
  (docanno--generic-get docanno--current-backend docanno--backends "backend" key backend))

(defun docanno-viewer-get (key &optional viewer)
  "Return the value of KEY in current viewer.

If VIEWER is a string it is used instead of the current viewer.
If no viewer has been activated yet, throw a user-error."
  (docanno--generic-get docanno--current-viewer docanno--viewers "viewer" key viewer))

(defvar docanno-backend-change-hook nil
  "This hook is run after changing the current backend.")

(defvar docanno-viewer-change-hook nil
  "This hook is run after changing the current viewer.")

;;;###autoload
(defun docanno-set-viewer (viewer)
  "Make VIEWER the current viewer.

When called interatively, completion over the currently activated
viewers is available."
  (interactive (list (completing-read
                      "Select a viewer: "
                      (cl-loop for v in docanno--viewers by #'cddr collect v))))
  (setq docanno--current-viewer viewer)
  (run-hooks 'docanno-viewer-change-hook))

;;;###autoload
(defun docanno-set-backend (backend)
  "Make BACKEND the current backend.

When called interatively, completion over the currently activated
backends is available."
  (interactive (list (completing-read
                      "Select a backend: "
                      (cl-loop for b in docanno--backends by #'cddr collect b))))
  (setq docanno--current-backend backend)
  (run-hooks 'docanno-backend-change-hook))

;;;###autoload
(defun docanno-set-file-name (file &optional no-guess)
  "Make FILE the current file.

When called interactively, completion over files returned by the
backend property :file-name is available.
With universal argument there is no such completion."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (let (insert-default-directory)
               (read-file-name
                "Find document file to control: " nil nil t nil
                (lambda (f) 
                  (or (file-directory-p f)
                      (cl-some (lambda (suffix)
                                 (string-suffix-p suffix f 'ignore-case))
                               docanno-file-suffixes)))))
           (let ((guesses (funcall (docanno-backend-get :file-name))))
             (completing-read 
              "Select document to control: "
              guesses nil 'confirm nil nil guesses)))
         current-prefix-arg))
  (when (file-exists-p file)
    ;; Save the page of the current file.
    (puthash docanno--current-file-name docanno--current-page-num docanno--page-num-hash)
    ;; Restore the page of the new file.
    (setq docanno--current-page-num (or (gethash file docanno--page-num-hash) 1))
    (setq docanno--current-file-name (expand-file-name file))))

(defun docanno--maybe-change-file-name ()
  "Use the backend property :file-name to set `docanno--current-file-name'.

Calls `docanno-set-file-name' with the first file name returned by
backend property :file-name if it is non-nil and is not the
current file name.
Does nothing if `docanno-auto-update-file-path' is nil."
  (when docanno-auto-update-file-path
    (let ((file (car (funcall (docanno-backend-get :file-name)))))
      (when (or (and file (not (string= file docanno--current-file-name)))
                (not docanno--current-file-name))
        (docanno-set-file-name file)))))

(defun docanno--check-file-name ()
  (cond ((null docanno--current-file-name)
         (user-error "Document has not been set"))
        ((not (file-exists-p docanno--current-file-name))
         (user-error "\"%s\" does not exist" docanno--current-file-name))))

;;;###autoload
(defun docanno-display-doc (&optional page)
  "Display the current document at PAGE."
  (interactive "p")
  (docanno--check-file-name)
  (when page (setq docanno--current-page-num page))
  (let ((display-cmd (docanno-viewer-get :display)))
    (cond
     ((functionp display-cmd)
      (funcall display-cmd docanno--current-file-name docanno--current-page-num))
     ((stringp display-cmd)
      (start-process-shell-command
       display-cmd (get-buffer-create " *docanno-display-process*")
       (docanno--replace-placeholder display-cmd docanno--current-page-num)))
     (t (user-error "The value of `docanno-command-alist' is not correct"))))
  (run-hooks 'docanno-command-hook))

(defun docanno--replace-placeholder (command page)
  (s-replace-all `(("%f" . ,(replace-quote
                             (shell-quote-argument
                              (expand-file-name docanno--current-file-name))))
                   ("%p" . ,(int-to-string page)))
                 command))

;;;###autoload
(defun docanno-previous-page (&optional count)
  "Display previous page of current document.

If COUNT is given, go back COUNT pages."
  (interactive "p")
  (docanno--maybe-change-file-name)
  (docanno-display-doc (- docanno--current-page-num (or count 1))))

;;;###autoload
(defun docanno-next-page (&optional count)
  "Display next page of current document.

If COUNT is given, go forward COUNT pages."
  (interactive "p")
  (docanno--maybe-change-file-name)
  (docanno-display-doc (+ docanno--current-page-num (or count 1))))

;;;###autoload
(defun docanno-insert-page ()
  "Insert the current page at point."
  (interactive)
  (insert (funcall (docanno-backend-get :format-page) docanno--current-page-num)))

;;;###autoload
(defun docanno-insert-note ()
  "Insert a new note at point."
  (interactive)
  (let ((new-note (docanno-backend-get :new-note)))
    (cond ((stringp new-note)
           (insert new-note))
          ((functionp new-note)
           (let ((ret (funcall new-note)))
             (when (stringp ret)
               (insert ret))))
          (t (error ":new-note must either be a string or a function")))))

;;;###autoload
(defun docanno-insert-note-and-page ()
  "Insert a new note and the current page at point."
  (interactive)
  (docanno-insert-note)
  (docanno-insert-page)
  (let ((sep (docanno-backend-get :separator)))
    (cond ((stringp sep) (insert sep))
          ((functionp sep) (insert (or (funcall sep) ""))))))

(provide 'docanno)

;;; docanno.el ends here
