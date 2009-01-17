;; Copyright (C) 2008 Gabriel Saldana

;; Author: Gabriel Saldana <gsaldana@gmail.com>
;; Created: Aug 20
;; Version: 0.1
;; Keywords: identica web
;; URL:

;; Identica Mode is a major mode to check friends timeline, and update your
;; status on Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth floor,
;; Boston, MA 02110-1301, USA.

;; Installation

;; Add the following to your .emacs or your prefered customizations file

;; (require 'laconica-mode)
;; (setq laconica-username "yourusername")
;; (setq laconica-password "yourpassword")

;; If you want to connect to a custom laconica server add this and change
;; identi.ca with your server's doman name.

;; (setq laconica-server "identi.ca")

;; Start using with M-x identica-mode

(require 'xml)
(require 'url)
(require 'url-http)

(eval-when-compile
  (require 'cl))

(defconst laconica-mode-version "0.1")

;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;
(defvar laconica-username nil)

(defvar laconica-password nil)

; Uses identi.ca by default
(defvar laconica-server-url "http://identi.ca/api/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Twitter-compatible API actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get friends timeline
;; url: /statuses/public_timeline.xml
;; params: none
(defun laconica_get_friends_timeline ()
  "Get friends timeline"
  (send_request (concat laconica-server-url "statuses/public_timeline.xml") "GET" ))

;; Update status
;; url: /statuses/update.xml
;; params: status The text of the status update
(defun laconica_update_status ()
  "Send a message to update your status"
  (interactive "P"))

;; Add as favorite
;; url: /favorites/create/id.xml
;; params: id of the status to fave


(defun laconica_send_request (serverurl method action &optional params)
  "Sends the request to the specified server"
  (let ((url-request-method 'method)
	(if params (url-request-data
		    (mapconcat (lambda (arg)
                          (concat (url-hexify-string (car arg))
                                  "="
                                  (url-hexify-string (cdr arg))))
                        args
                        "&")))
	)
    (url-retrieve serverurl 'laconica-update-buffer)))

(defun laconica-update-buffer ()
  "Update laconica buffer")
