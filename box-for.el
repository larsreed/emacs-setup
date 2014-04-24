;;; box-for.el --- put a box around text

;; Author: Terrence Brannon (brannon@lnc.usc.edu)
;; Created: Tue Feb  3 15:32:54 PST 1998
;; Keywords: extensions, text manipulation

;; box-for.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; box-for.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This puts a box around text: I would do a whole CAD application in Emacs
;; but I just dont have the time right now :-)

;;; Usage:
;;  Place this file in your load-path
;;   Type C-h v load-path if you dont know that.
;;  Add: (require 'box-for) to your .emacs
;;  Type: M-x box-for whenever you must attach files
;;  That's it.

;;; Intent: to draw things in a line like this.

;------  -----------------   ------  -------------------------------------
;| HH |  | passive cable |   | HH |  | passive cable (measure EPSP here) |
;------	 -----------------   ------  -------------------------------------

(require 'picture)

(defun box-for (text)
  (interactive "sBox for what text: ")

  (picture-move-up 1)
  (let ((upper-left (point)))
    (insert-char ?- (+ 4 (length text)))	
    (goto-char upper-left)
    (picture-move-down 1)
    (insert "| ")
    (insert text)
    (insert " |")
    (goto-char upper-left)
    (picture-move-down 2)
    (insert-char ?- (+ 4 (length text)))))

(provide 'box-for)
