;;; identica-friends.el ---
;;
;; Filename: identica-friends.el
;; Description: A library that provides some functions to look who are your friends in your identi.ca account.
;; Author: Christian Giménez
;; Maintainer:
;; Created: dom sep 25 17:58:40 2011 (-0300)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Use M-x identica first, if you are not connected, this library
;; will not work.
;; You can check who are your friends on Identi.ca using the function
;; M-x identica-show-friends
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'xml)

(defvar identica-friends-buffer nil
  "Friend's Buffer"
  )

(defvar identica-friends-buffer-name "*identica-friends*"
  )

(defun identica-friends-buffer ()
  "Show a new buffer with all the friends. "
  (setq identica-friends-buffer (get-buffer-create identica-friends-buffer-name))
  (switch-to-buffer identica-friends-buffer)
  )

(defun identica-show-friends ()
  (interactive)
;  (setq identica-method-class "statuses")
;  (setq identica-method "friends")
;  (identica-http-get identica-method-class identica-method identica-show-friend-sentinel)
  (identica-http-get "statuses" "friends" nil 'identica-show-friend-sentinel)
  )

(defun identica-show-friend-sentinel (&rest rest)
  "Sentinel executed after recieving all the information from identi.ca."
  ;; cnngimenez: This I used for debug HTTP
  ;;  (identica-copiar-http-buffer)
  ;; Search for the begining of the xml...
  (goto-char 0)
  (search-forward "<?xml")
  (setq start-xml (match-beginning 0))
  ;; Parse xml into a list
  (setq lst-xml (xml-parse-region start-xml (point-max)))
  ;; Change buffer...
  (identica-friends-buffer)
  ;; Find elements on that list and write it
  (identica-write-friends lst-xml)
  )

(defun identica-write-friends (lst)
  "Parse the list and, after that, write every friends into the current buffer."
  ;; Get first element
  (setq lst (car lst))
  ;; ignore the word 'users' and the next element... remove enter.
  (setq lst (nthcdr 3 lst))
  (dolist (usr lst)
    (unless (stringp usr)
      (identica-write-user usr)
      )
    )
  )

(defun identica-get-user-data (usr-lst)
  "Parse the list and make a list more easy to read. The list has the following form:
  (id . name . screen_name . location . description ).
"

  (setq lst '())

  ;; Put the desription
  (push
   (nth 2 (nth 11 usr-lst))
   lst
   )

  ;; Put the location
  (push
   (nth 2 (nth 9 usr-lst))
   lst
   )

  ;; Put the screen name
  (push
   (nth 2 (nth 7 usr-lst))
   lst
   )


  ;; Put the name
  (push
   (nth 2 (nth 5 usr-lst))
   lst
   )


  ;; Put the id
  (push
   (nth 2 (nth 3 usr-lst))
   lst
   )



  )

(defun identica-write-user (usr)
  "Write an user taking the info from a list."
  (setq usr-data (identica-get-user-data usr))
  (insert "\nId: " (nth 0 usr-data))
  (insert "\nNick: " (nth 2 usr-data))
  (insert "\nName: " (nth 1 usr-data))
  (insert "\nDescription: " (nth 4 usr-data))
  (insert "\nLocation: " (nth 3 usr-data))
  (insert "\n--------------------\n")
  )

(provide 'identica-friends)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; friends.el ends here
