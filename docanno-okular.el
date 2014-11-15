;;; docanno-okular.el --- docanno backend for okular.

;; Copyright (C) 2014 Alexander Baier

;; Author: Alexander Baier <alexander.baier@mailbox.org>
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: (("docanno" 0.0.3))

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

;;; Code:

(require 'docanno)

(docanno-define-viewer "okular"
  :display "/usr/bin/okular --noraise --unique -p %p %f")

(provide 'docanno-okular)

;;; docanno-okular ends here.
