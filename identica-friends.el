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
;; If you want to check who are following you, type:
;; M-x identica-show-followers
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


(defface identica-friends-mode-id
  '(
                                        ; If there's dark background...
    (((class color) (background dark))
     :foreground "yellow"
     )
                                        ; If there's light background...
    (((class color) (background light))
     :foreground "red"
     )

    (t :background "white"
       :foreground "blue")
    )
  ""
  )

(defface identica-friends-mode-bar
  '(
                                        ; If there's dark background...
    (((class color) (background dark))
     :bold t
     )
                                        ; If there's light background...
    (((class color) (background light))
     :bold t
     )

    (t :background "white"
       :foreground "blue"
       :bold t)
    )
  ""
  )

(defvar identica-friends-mode-font-lock
  '(
    ;; font-lock-keywords
    (
     ("^Id: .*$" . 'identica-friends-mode-id)
     ("^Nick: .*$" . 'identica-username-face)
     ("^--------------------$" . 'identica-friends-mode-bar)
     )

    ;; Otros...
    )
  ;;
  "Font lock for `identica-friends--mode'"
  )


;; KEYMAPS
;; _______

(defvar identica-friends-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'bury-buffer)
    map)
  "Keymap for `identica-friends-mode'."
  )

(defun identica-friends-buffer ()
  "Show a new buffer with all the friends. "
  (setq identica-friends-buffer (get-buffer-create identica-friends-buffer-name))
  (switch-to-buffer identica-friends-buffer)
  (identica-friends-mode)
  )

(define-derived-mode identica-friends-mode nil "Identica-friends-mode"
  "Major mode for identica-friends buffer.
Use `identica-show-friends' to call this buffer."
  ;; font lock para ej-mode
  (set (make-local-variable 'font-lock-defaults)
       identica-friends-mode-font-lock)
  (set (make-local-variable 'buffer-read-only) t)
  (make-local-variable 'inhibit-read-only)
  )


                                        ; ----------
                                        ; Followers
                                        ;

(defun identica-show-followers()
  (interactive)
  (identica-http-get "statuses" "followers" nil 'identica-show-user-sentinel '("follower"))
  )


(defun identica-get-follower-data (usr-lst)
  "Parse the list and make a more easy-to-read list. The final list will have the following form suitable
for writing in a buffer with the function `identica-write-user'.
  (id . name . screen_name . location . description )."

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

  ;; Put the screen-name
  (push
   (nth 2 (nth 7 usr-lst))
   lst)

  ;; Put the name
  (push
   (nth 2 (nth 5 usr-lst))
   lst)

  ;; Put the id
  (push
   (nth 2 (nth 3 usr-lst))
   lst)


  ;; Replace nils into strings...
  (replace-nils lst "")
  )


                                        ;----------
                                        ; Friends
                                        ;


(defun identica-show-friends ()
  (interactive)
;  (setq identica-method-class "statuses")
;  (setq identica-method "friends")
;  (identica-http-get identica-method-class identica-method identica-show-friend-sentinel)
  (identica-http-get "statuses" "friends" nil 'identica-show-user-sentinel '("friend"))
  )

(defun identica-get-friend-data (usr-lst)
  "Parse the list and make a list more easy to read. The list has the following form:
  (id . name . screen_name . location . description ).

This form is suitable for the function `identica-write-user'.
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

  ;; Replace nils into strings...
  (replace-nils lst "")
  )


                                        ; ----------
                                        ; Common
                                        ;


;; Are there any function to replace anything from a list?
(defun replace-nils (lst elt)
  "Replace nils with an element elt."
  (unless (null lst)
    (if (null (car lst))
        (cons elt (replace-nils (cdr lst) elt))
      (cons (car lst) (replace-nils (cdr lst) elt))
      )
    )
  )



(defun identica-show-user-sentinel
  (&optional status method-class method parameters success-message type-of-user)
  "Sentinel executed after recieving all the information from identi.ca.
This sentinel needs to know if the type-of-user(or type of list) is one of these:
- \"friend\"
- \"follower\".

First, its parse the XML file recieved by identi.ca. While parsing, it show the user data into a buffer.

"
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
  (identica-parse-xml-user-lists type-of-user lst-xml)
  )



(defun identica-parse-xml-user-lists (type-of-user xml-lst)
  "Parse the xml-lst list and, after that, write every user into the current buffer.
The way it is parsed depends on the type-of-user we are talking about:
- \"friend\"
- \"follower\"
"
  ;; Get first element
  (setq xml-lst (car xml-lst))
  ;; ignore the word 'users' and the next element... remove enter.
  (setq xml-lst (nthcdr 3 xml-lst))
  ;; Erase friends-buffer
  (with-current-buffer identica-friends-buffer-name
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      )
    )
  ;; for each user in the xml list, parse it, and write it...
  (dolist (usr xml-lst)
    (unless (stringp usr)
      (identica-write-user
       (if (string= type-of-user "friends")
           (identica-get-friend-data usr) ;; Is a friend, parse xml as a friends.xml
         (identica-get-follower-data usr) ;; is a follower, parse as followers.xml
         )
       )
      )
    )
  )


(defun identica-write-user (usr-data)
  "Write an user taking the info from a list.
The list must be of the form given by the functions `identica-get-friend-data'
or `identica-get-follower-data':

 (id . name . screen_name . location . description )

"
  (let ((inhibit-read-only t))
    (insert "\nNick: " (nth 2 usr-data))
    (insert "\nName: " (nth 1 usr-data))
    (insert "\nDescription: " (nth 4 usr-data))
    (insert "\nLocation: " (nth 3 usr-data))
    (insert "\n--------------------\n")
    )
  )

                                        ; ----------
                                        ; For debugging purpose
                                        ;

(defvar identica-http-debug "*identica-http*"
  "Buffer to the http requests")

(defun identica-copiar-http-buffer ()
  "Copia el buffer http a otro nuevo para ver el resultado de la comunicación."
  (with-current-buffer identica-http-buffer
    (setq str (buffer-string))
    )
  (with-current-buffer (get-buffer-create identica-http-debug)
    (delete-region (point-min) (point-max))
    (insert str)
    )
  )


(provide 'identica-friends)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; friends.el ends here
