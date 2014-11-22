;;; jde-util.el -- JDE utilities.
;; $Revision: 1.7 $ $Date: 2002/08/20 03:09:05 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 2001, 2002 Paul Kinnucan.

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

;;; Commentary:

;; This package provides useful macros, functions, and classes
;; required by multiple JDE packages. You should not put any macros,
;; functions, or classes in this package that require other
;; JDE packages.
;; 
;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://jde.sunsite.dk>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@mediaone.net.

;;; Code:

(require 'efc)

(defmacro jde-assert-source-buffer ()
  "Asserts that the current buffer is a
Java source or a debug buffer."
  '(assert  (eq major-mode 'jde-mode) nil 
    "This command works only in a Java source or debug buffer."))

(defun jde-get-line-at-point (&optional pos)
  "Get the number of the line at point."
  (let* ((point (or pos (point)))
	 (ln (if (= point 1)
		 1
	       (count-lines (point-min) point))))
    (save-excursion
      (goto-char point)
      (if (eq (char-before) ?\n)
	  (1+ ln)
	ln))))

(defmacro jde-with-file-contents (file &rest body)
  "If FILE exists and is readable creates a temporary buffer with the contents
of FILE, points to beginning of buffer, evaluates BODY and return the value of
the last form of BODY. If FILE does not exist or is not readable nil is
returned.
Note: No major/minor-mode is activated and no local variables are evaluated
for FILE, but proper EOL-conversion and charcater interpretation is done!"
  (let ((exp-filename (make-symbol "exp-filename")))
    `(let ((,exp-filename (expand-file-name ,file)))
       (if (and (file-exists-p ,exp-filename)
                (file-readable-p ,exp-filename))
           (with-temp-buffer
             (insert-file-contents ,exp-filename)
             (beginning-of-buffer)
             ,@body)
         nil))))

(defmacro jde-normalize-paths (pathlist &optional symbol)
  "Normalize all paths of the list PATHLIST and returns a list with the
expanded paths."
  (` (mapcar (lambda (path)
               (jde-normalize-path path (, symbol)))
             (, pathlist))))

(defun jde-search-src-dirs (class)
  "Return the directory containing the source file for a class.
CLASS is the fully qualified name of the class."
  (let ((file (concat
	       (jde-parse-get-unqualified-name class)
	       ".java"))
	(package (jde-parse-get-package-from-name class)))
    (catch 'found
      (loop for dir in jde-sourcepath do
	    (progn
	      (setq 
	       dir
	       (jde-normalize-path dir 'jde-sourcepath))
	      (if (file-exists-p (expand-file-name file dir))
		  (throw 'found dir)
		(let* ((pkg-path (subst-char-in-string ?. ?/ package))
		       (pkg-dir (expand-file-name pkg-path dir))
		       (file-path (expand-file-name file pkg-dir)))
		  (if (file-exists-p file-path)
		      (throw 'found pkg-dir)))))))))	  


(defun jde-find-class-source-file (class)
  "*Find the source file for a specified class.
CLASS is the fully qualified name of the class. This
function searchs the source file paths specified by 
`jde-sourcepath' for the source file 
corresponding to CLASS. If it finds the source file,
it returns the file's path. Otherwise, it returns nil."
  (let* ((class (jde-remove-inner-class class))
         (source-dir (jde-search-src-dirs class))
         (file-name 
	 (concat (jde-parse-get-unqualified-name class)
		 ".java")))
     (if source-dir
         (expand-file-name file-name source-dir)
       (message "JDE error: Could not find source for \"%s\". See `jde-sourcepath' for more information." class)
       nil)))

(defun jde-remove-inner-class (class) 
  "Removes the inner class name for the class"
  (car (split-string class "[$]")))
    
(defun jde-find-class-source (class)
   "*Find the source file for a specified class.
Calls `jde-find-class-source-file' to do the search.
If it finds the source file, it opens the file in a buffer."
   (interactive "sClass: ")
   (let ((source (jde-find-class-source-file class)))
     (when source
       (find-file source))))

(provide 'jde-util)

;; Change History 

;;
;; $Log: jde-util.el,v $
;; Revision 1.7  2002/08/20 03:09:05  jslopez
;; Fixes bug in jde-find-class-source-file that was not handling
;; inner class properly.
;;
;; Revision 1.6  2002/06/22 06:13:29  paulk
;; Fixed bug in jde-normalize-paths macro.
;;
;; Revision 1.5  2002/05/12 06:48:52  paulk
;; Added jde-with-file-contents jde-normalize-paths jde-search-src-dirs
;; jde-find-class-source-file jde-find-class-source functions.
;;
;; Revision 1.4  2002/03/31 07:49:49  paulk
;; Renamed jde-db-source-directories. The new name is jde-sourcepath.
;;
;; Revision 1.3  2002/02/03 06:52:54  paulk
;; Fixed quote bug in jde-assert-source-buffer macro.
;;
;; Revision 1.2  2002/01/19 06:42:22  paulk
;; Minor updates.
;;
;; Revision 1.1  2002/01/04 07:15:20  paulk
;; Initial revision.
;;

;; End of jde-util.el

