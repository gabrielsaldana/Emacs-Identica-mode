;;; bbdb-identica.el ---
;;
;; Filename: bbdb-identica.el
;; Description:
;; Author: Christian
;; Maintainer:
;; Created: dom oct  2 22:15:13 2011 (-0300)
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
;;   BBDB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This should work in BBDB V.3.x for now...
;; It is in heavy, really heavy, development.
;;
;; As far I tried, I couldn't make it work in the way proposed by BBDB. 
;; I couldn't find any documentation of how to use the MUA API.
;; For now, I will use every possible command despite it is not desirable
;; for BBDB developers(I think :-S ).
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

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)
(require 'identica-mode)
(require 'identica-friends)

                                        ; Identica-friends-buffer
;; Identica friends buffer must have a way to introduce people into BBDB.
;; There's need of creating a new field into a record. This field will be called "identica".

;; We'll define a ':' key for introducing a new record into BBDB or updating a record.

(defcustom bbdb/identica-update-records-p
  (lambda ()
    (let ((bbdb-update-records-p 'query ))
      (bbdb-select-message)))
  "How `bbdb-mua-update-records' processes mail addresses in Identica and Identica-friends.
Allowed values are:
 nil          Do nothing.
 search       Search for existing records.
 query        Update existing records or query for creating new ones.
 create or t  Update existing records or create new ones.
A function which returns one of the above values."
  :group 'bbdb-mua-identica
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records"
                        (lambda () (let ((bbdb-update-records-p 'search))
                                     (bbdb-select-message))))
                 (const :tag "query annotation of all messages"
                        (lambda () (let ((bbdb-update-records-p 'query))
                                     (bbdb-select-message))))
                 ;; (const :tag "annotate (query) only new messages"
                 ;;        (lambda ()
                 ;;          (let ((bbdb-update-records-p
                 ;;                 (if (bbdb/rmail-new-flag) 'query 'search)))
                 ;;            (bbdb-select-message))))
                 (const :tag "annotate all messages"
                        (lambda () (let ((bbdb-update-records-p 'create))
                                     (bbdb-select-message))))
                 (const :tag "accept messages" bbdb-accept-message)
                 (const :tag "ignore messages" bbdb-ignore-message)
                 (const :tag "select messages" bbdb-select-message)
                 (sexp  :tag "user defined function")))

;; (defun bbdb/identica-header (header)
;;   ""
;; )
                                        ; --------------------
                                        ; Insinuation
                                        ; --------------------

;; It doesn't work :-( Is still under development.
;; 
;; ;;;###autoload
;; (defun bbdb-insinuate-identica ()
;;   "Add every keymap and hooks necesary for using BBDB into `identica-friends-mode'.
;; You shouldn't call this in your init file, instead use `bbdb-initialize'"
;;   (define-key identica-friends-mode-map ":" 'bbdb-mua-display-sender)
;;   (define-key identica-friends-mode-map ";" 'bbdb-mua-edit-notes-sender)
;; )



;; ;; We have to make bbdb-mua recognice identica-friends-mode, if not it will fall-back with error.
;; (defadvice bbdb-mua (before identica-bbdb-mua ())
;;   "This advice add into `bbdb-mua' the necessary for BBDB to recognice identica-friends-mode, and identica-mode."
;;   (if (member major-mode '(identica-mode identica-friends-mode))
;;       'identica)
;;   )

;; Activate identica-bbdb-mua advice
;; (ad-activate 'bbdb-mua)
;; (ad-deactivate 'bbdb-mua)


					; ____________________

(defun bbdb-identica-next-usr ()
  "This function is supposed to be used as a hook to the function `identica-friends-next-user'.
Check if the actual user is in BBDB. If not, add it *without query the user*. 

Remember: 
Adding into the BBDB means: 
1) to create a new BBDB record with the same name of the identica user name(NOT NICK!)
2) Add the nick into a new field called \"identica\"."
  (setq usr (identica-friends-get-current-user))
  ;; Our idea is to show the user if founded...
  ;; Search for the *first mach* in the BBDB:
  (setq record 
	(car (bbdb-search (bbdb-records) (nth 2 usr)))
	)
  
  ;; check if exist, if not add it(or query to add it).
  (if record 
      (if (bbdb-identica-check-record record usr)
	  ;; It has to be updated!
	  (bbdb-identica-query-update-record record usr)
	)
    ;; No record available... query to add it..
    (bbdb-identica-query-add-record record usr)
    )
  )

(defun bbdb-identica-query-update-record (record usr)
  "Query the user if she/he wants to update the BBDB record.
If she/he answer \"yes\", update it.
If she/he answer \"no\", do nothing."
  ;; TODO
  ;; Don't know if the record parameter is necesary...
)

(defun bbdb-identica-update-record (record usr)
  "Update the record usr with new values:
1) Update the \"identica\" field.
2) No need to update anything else..."
  ;; TODO
  ;; Don't know if the record parameter is necesary
  )

(defun bbdb-identica-query-add-record (record usr)
  "Query the user if she/he wants to add this identica user into BBDB.
If she/he answer \"yes\", add it.
If she/he answer \"no\", don't add it of course."
  ;; TODO
  ;; Don't know if the record parameter is necesary
  )

(defun bbdb-identica-add-record (record usr)
  "Add friend/follower into BBDB."
  ;; TODO
  ;; Don't know if the record parameter is necesary
  )

(defun bbdb-identica-check-record (record usr)
  "Check if the record has the same value in the \"identica\" field and the name field.
If it is the same return t.
If it is different return nil.
If the \"identica\" field does not exists return nil(it means it has different value).
"
  ;; *TODO*
  )




(provide 'bbdb-identica)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-identica.el ends here
