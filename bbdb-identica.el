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


                                        ; --------------------
                                        ; Insinuation
                                        ; --------------------

;;;###autoload
(defun bbdb-insinuate-identica-friends ()
  "Add every keymap and hooks necesary for using BBDB into `identica-friends-mode'.
You shouldn't call this in your init file, instead use `bbdb-initialize'"
  (define-key identica-friends-mode-map ":" 'bbdb-mua-display-sender)
  (define-key identica-friends-mode-map ";" 'bbdb-mua-edit-notes-sender)
)

(provide 'bbdb-identica)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-identica.el ends here
