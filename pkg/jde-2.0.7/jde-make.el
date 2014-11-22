;;; jde-make.el -- Integrated Development Environment for Java.
;; $Revision: 1.3 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defcustom jde-make-program "make"
  "*Specifies name of make program."
 :group 'jde-project
 :type 'string)

(defcustom jde-make-args ""
  "*Specifies arguments to be passed to make program."
  :group 'jde-project
  :type 'string)

;;;###autoload
(defun jde-make (args)
  "Run the JDE make program."
  (interactive
   (list (if (string= jde-make-args "")
	     (read-from-minibuffer (concat jde-make-program " ")
				   (nth 0 minibuffer-history))
	   jde-make-args)))
  (jde-make-internal (jde-run-parse-args args)))


(defun jde-make-internal(args)
  (let ((make-buf-name (concat "*" jde-project-name "*")))
    (if (not (comint-check-proc make-buf-name))
	(let* ((make-buffer (get-buffer-create make-buf-name))
	       (win32-p (eq system-type 'windows-nt))
	       (prog jde-make-program)
	       (prog-args args)
	       (command-string (concat prog " " 
				       (jde-run-make-arg-string
					prog-args)
				       "\n\n")))
	  (save-excursion
	    (set-buffer make-buffer)
	    (erase-buffer)
	    (insert (concat "cd " default-directory "\n"))
	    (insert command-string)
	    (jde-make-mode))
	  (comint-exec make-buffer "JDE make" prog nil prog-args)
	  (pop-to-buffer make-buffer))
      (message "A make process for %s is running." jde-project-name)
      (pop-to-buffer make-buf-name))))

;; Dummy function to facilitate autoloading.
;;;###autoload
(defun jde-make-mode ()
  "Major mode for building Java applications."
  )

(define-derived-mode
  jde-make-mode comint-mode "JDE Make"
  "Major mode for building Java applications.
  \\(jde-make-mode-map)"
)


(provide 'jde-make)

;; $Log: jde-make.el $
;; Revision 1.3  1998/05/29 01:46:39  paulk
;; Added dummy function for jde-make-mode to facilitate autoloading.
;;
;; Revision 1.2  1998/05/27 06:04:52  paulk
;; Added autoload comments.
;;
;; Revision 1.1  1998/03/27 04:44:36  kinnucan
;; Initial revision
;;

;; End of jde-make.el