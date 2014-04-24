;; @(#) csdiff.el -- Compare files/revisions with CSDiff
;; @(#) $Id: csdiff.el,v 1.1 2000/03/13 16:03:57 david_ponce Exp $

;; This file is not part of Emacs

;; Copyright (C) 2000 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      March 13 2000

;; LCD Archive Entry:
;; csdiff|David Ponce|david.ponce@wanadoo.fr|
;; Compare files/revisions with CSDiff|
;; $Date: 2000/03/13 16:03:57 $|$Revision: 1.1 $|~/misc/csdiff.el|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:
;;
;;  csdiff.el allows use of the ComponentSoftware Diff (CSDiff) tool from Emacs.
;;  ComponentSoftware Diff (CSDiff) is a free advanced file difference analysis
;;  tool for Windows 95/98/NT (see <http://www.componentsoftware.com/csdiff/>).

;;  This library defines the following commands:
;;  
;;      `csdiff-files'    - to compare 2 files. 
;;      `csdiff-revision' - to compare 2 revisions of a file under version control. 

;;; Installation:
;;
;;  csdiff.el requires vc.el to compare revisions.
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'csdiff)

;;; Usage:
;;
;;  M-x `csdiff-files' to compare 2 files.
;;  M-x `csdiff-revision' to compare 2 revisions.
;;
;;  If possible csdiff add a "Compare with CSDiff" sub-menu in the "Tools" menu.

;;; Customization:
;;
;;  M-x `csdiff-customize' to customize csdiff various options.
;;
;;  The following variables could be set:
;;
;;  o `csdiff-load-hook'
;;        hook run when package has been loaded.
;;
;;  o `jdok-describe-method'
;;        Template used to describe a method.
;;
;;  o `csdiff-program'
;;        Path to csdiff.exe program.
;;

;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This library was developed with NTEmacs 20.6.1 under MS Windows
;;  NT 4 WKS SP5.

;;; Code:
(if (not (eq window-system 'w32))
    (error "csdiff.el requires running %s under w32 window-system" (invocation-name)))

(require 'vc)

(defgroup csdiff nil
  "csdiff customization"
  :group 'tools)

(defcustom csdiff-program "CSDiff.exe"
  "*CSDiff program path."
  :group 'csdiff
  :type '(file :must-match))

(defcustom csdiff-load-hook nil
  "*Hook run when package has been loaded."
  :group 'csdiff
  :type 'hook)

;;;###autoload
(defun csdiff-customize ()
  "Customization of csdiff."
  (interactive)
  (customize-group "csdiff"))


;;;###autoload
(defun csdiff-files (file1 file2)
  "Run CSDiff to compare FILE1 with FILE2."
  (interactive "fCompare file: \nfwith file: ")
  (message "csdiff '%s' '%s'" file1 file2)
  (csdiff-execute-nowait file1 file2))

;;;###autoload
(defun csdiff-revision (&optional file)
  "Run CSDiff to compare versions of a file.
The file is an optional FILE argument or the file visited by the current
buffer."
  ;; if buffer is non-nil, use that buffer instead of the current buffer
  (interactive "P")
  (if (stringp file) (find-file file))
  (let (rev1 rev2)
    (setq rev1
          (read-string
           (format "Version 1 to compare (default: %s's latest version): "
                   (if (stringp file)
                       (file-name-nondirectory file) "current buffer")))
          rev2
          (read-string
           (format "Version 2 to compare (default: %s): "
                   (if (stringp file)
                       (file-name-nondirectory file) "current buffer"))))
    (csdiff-vc-internal rev1 rev2)
    ))

(defun csdiff-vc-internal (rev1 rev2)
  "Run CSDiff on versions of the current buffer.
If REV2 is \"\" then compare current buffer with REV1.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (let ((file1 (csdiff-version rev1))
        (file2 (if (string= rev2 "")    ; use current buffer
                   (buffer-file-name)
                 (csdiff-version rev2))))
;;    (csdiff-files file1 file2)
    (progn
      (csdiff-execute-wait file1 file2)
      (csdiff-delete-file file1)
      (or (string= rev2 "") (csdiff-delete-file file2)))
    ))

(defun csdiff-version (rev)
  "Retrieve the version REV file of the current buffer.
If the current buffer is named `F', the version file is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (vc-ensure-vc-buffer)
  (let* ((version (if (string-equal rev "")
                      (vc-latest-version buffer-file-name)
                    rev))
         (filename (concat buffer-file-name ".~" version "~")))
    (or (file-exists-p filename)
        (vc-backend-checkout buffer-file-name nil version filename))
    filename))

(defun csdiff-delete-file (f)
  "Delete file named F and display a status message."
  (progn
    (delete-file f)
    (message "File '%s' %s" f (if (file-exists-p f) "not deleted" "deleted"))))
        
(defun csdiff-execute-wait (file1 file2 &rest options)
  "Run CSDiff by comparing FILE1 with FILE2. OPTIONS could specifies additional
CSDiff options."
  (message "CSDiff running...(%s waiting)" (invocation-name))
  (apply 'call-process csdiff-program nil nil nil
         (append options
                 (list (csdiff-w32-file-name file1)
                       (csdiff-w32-file-name file2))))
  (message "CSDiff ended."))

;; I WAS NOT ABLE TO GET csdiff-sentinel INVOKED WHEN CSDiff EXITED.
(defun csdiff-execute-nowait (file1 file2 &rest options)
  "Run CSDiff by comparing FILE1 with FILE2. OPTIONS could specifies additional
CSDiff options."
  (message "CSDiff started...")
  (let ((process (apply 'start-process "csdiff" nil csdiff-program
                        (append options
                                (list (csdiff-w32-file-name file1)
                                      (csdiff-w32-file-name file2))))))
    (set-process-sentinel process 'csdiff-sentinel)))

(defun csdiff-sentinel (process event)
  "Handle a change to the process communicating with CSDiff."
  (or (eq (process-exit-status process) 0)
      (message "Process '%S' ended." process)))

(defun csdiff-w32-file-name (f)
  "Return a valid win32 filename from the given Emacs filename F." 
  (csdiff-slash-to-backslash (expand-file-name f)))

(defun csdiff-slash-to-backslash (s)
  "Replace occurence of '/' by '\\' in the string S. Always returns a new copy of S."
  (let ((l (length s))
        (i 0)
        (sw (copy-sequence s)))
    (while (< i l)
      (and (eq (aref sw i) ?/) (aset sw i ?\\))
      (setq i (1+ i)))
    sw))

(defun csdiff-check-if-vc-buffer ()
  "Return t if the current buffer visits a version-controlled file."
  (condition-case nil
      (progn
        (vc-ensure-vc-buffer)
        t)
    (error nil)))

(defvar csdiff-menu
  (list "Compare with CSDiff"
        ["Two Files..."          csdiff-files    t]
        ["File with Revision..." csdiff-revision (csdiff-check-if-vc-buffer)]
        )
  "The csdiff menu.")

;; Add the csdiff menu before the Tools/Compare menu
(if (and (require 'easymenu "easymenu" t) (fboundp 'easy-menu-add-item))
    (easy-menu-add-item nil '("tools") csdiff-menu "compare"))

(provide 'csdiff)
(run-hooks 'csdiff-load-hook)

;;; Change History:

;;
;; $Log: csdiff.el,v $
;; Revision 1.1  2000/03/13 16:03:57  david_ponce
;; Initial revision.
;;
;;

;;; csdiff.el ends here.
