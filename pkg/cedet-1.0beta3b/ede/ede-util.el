;;; ede-util.el --- EDE utilities

;;;  Copyright (C) 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-util.el,v 1.2 2000/11/18 14:10:32 zappo Exp $

;; This software is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Utilities that may not require project specific help, and oporate
;; on generic EDE structures.  Provide user level commands for activities
;; not directly related to source code organization or makefile generation.

(require 'ede)

;;; Code:

;;; Updating the version of a project.
;;;###autoload
(defun ede-update-version (newversion)
  "Update the current projects main version number.
Argument NEWVERSION is the version number to use in the current project."
  (interactive (list (let* ((o (ede-toplevel))
			    (v (oref o version)))
		       (read-string (format "Update Version (was %s): " v)
				  v nil v))))
  (let ((ede-object (ede-toplevel)))
    (oset ede-object :version newversion)
    (project-update-version ede-object)
    (ede-update-version-in-source ede-object newversion)))

(defmethod project-update-version ((ot ede-project))
  "The :version of the project OT has been updated.
Handle saving, or other detail."
  (error "project-update-version not supported by %s" (object-name ot)))

(defmethod ede-update-version-in-source ((this ede-project) version)
  "Change occurrences of a version string in sources.
In project THIS, cycle over all targets to give them a chance to set
their sources to VERSION."
  (ede-map-targets this (lambda (targ)
			  (ede-update-version-in-source targ version))))

(defmethod ede-update-version-in-source ((this ede-target) version)
  "In sources for THIS, change version numbers to VERSION."
  (if (and (slot-boundp this 'versionsource)
	   (oref this versionsource))
      (let ((vs (oref this versionsource)))
	(while vs
	  (save-excursion
	    (set-buffer (find-file-noselect
			 (ede-expand-filename this (car vs))))
	    (goto-char (point-min))
	    (let ((case-fold-search t))
	      (if (re-search-forward "version:\\s-*\\([^ \t\n]+\\)" nil t)
		  (progn
		    (save-match-data
		      (ede-make-buffer-writable))
		    (delete-region (match-beginning 1)
				   (match-end 1))
		    (goto-char (match-beginning 1))
		    (insert version)))))
	  (setq vs (cdr vs))))))

;;; Writable files
;;
;; Utils for EDE when it needs to write a file that could be covered by a
;; version control system.
(defun ede-make-buffer-writable (&optional buffer)
  "Make sure that BUFFER is writable.
If BUFFER isn't specified, use the current buffer."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if buffer-read-only
	(if (and vc-mode
		 (y-or-n-p (format "Check out %s? " (buffer-file-name))))
	    (vc-toggle-read-only)
	  (if (not vc-mode)
	      (toggle-read-only -1))))))
  
(provide 'ede-util)

;;; ede-util.el ends here
