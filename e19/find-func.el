;;; find-func.el --- find the definition of the elisp function near point

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Jens Petersen <petersen@kurims.kyoto-u.ac.jp>
;; Maintainer: petersen@kurims.kyoto-u.ac.jp
;; Keywords: emacs-lisp, help, functions
;; Status: tested on emacs-19.28+ and xemacs-19.15+
;; Created: 97/07/25
;; URL: <http://www.kurims.kyoto-u.ac.jp/~petersen/emacs-lisp/find-func.el>

;; $Id: find-func.el,v 0.13 1997/09/08 08:30:06 petersen Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; The funniest thing about this is that I can't imagine why a package
;; so obviously useful as this hasn't been written before!!
;; This probably belongs in "help.el" or somewhere like that.
;;
;; Put this file in your `load-path', byte-compile it and add the
;; following code in your init file:
;;
;; ;;; find-func
;; (load "find-function")
;; (global-set-key [(control ?c) ?f] 'find-function)
;; (global-set-key [(control ?c) ?4 ?f] 'find-function-other-window)
;; (global-set-key [(control ?c) ?5 ?f] 'find-function-other-frame)
;; (global-set-key [(control ?c) ?k] 'find-function-on-key)
;;
;; and away you go!  It does pretty much what you would expect,
;; putting the cursor at the definition of the function at point.
;;
;; In XEmacs the source file of dumped functions is recorded (and can
;; be accessed with the function `compiled-function-annotation', which
;; doesn't exist in Emacs), so in XEmacs non-primitive dumped
;; functions can also be found.  Unfortunately this is not possible in
;; Emacs.  It would be nice if the location of primitive functions in
;; the C code was also recorded!

;; The code is adapted from `describe-function', `describe-key'
;; ("help.el") and `fff-find-loaded-emacs-lisp-function' (Noah Friedman's
;; "fff.el").

;;; To do:
;;
;; o custom?
;;
;; o improve handling of advice'd functions? (at the moment it goes to
;; the advice, not the actual definition)

;;;; Code:

;;; User variables:

(defvar find-function-function 'function-at-point
  "*The function used by `find-function' to select the function near
point.

For example `function-at-point' or `function-called-at-point'.")

(defvar find-function-source-path nil
  "The default list of directories where find-function searches.

If this variable is `nil' then find-function searches `load-path' by
default.")


;;; Functions:

(defun find-function-noselect (function &optional path)
  "Returns list `(buffer point)' pointing to the definition of FUNCTION.

Finds the emacs-lisp library containing the definition of FUNCTION
in a buffer and places point before the definition.  The buffer is
not selected.

If the optional argument PATH is given, the library where FUNCTION is
defined is searched in PATH instead of `load-path' (see
`find-function-source-path')."
  (and (subrp (symbol-function function))
       (error "%s is a primitive function" function))
  (if (not function)
      (error "You didn't specify a function"))
  (let ((def (symbol-function function))
	library aliases)
    (while (symbolp def)
      (or (eq def function)
	  (if aliases
	      (setq aliases (concat aliases
				    (format ", which is an alias for %s"
					    (symbol-name def))))
	    (setq aliases (format "an alias for %s" (symbol-name
						       def)))))
      (setq function (symbol-function function)
	    def (symbol-function function)))
    (if aliases
	(message aliases))
    (setq library
	  (cond ((eq (car-safe def) 'autoload)
		 (nth 1 def))
		((describe-function-find-file function))
		((and (compiled-function-p def)
		      (fboundp 'compiled-function-annotation))
		 (substring (compiled-function-annotation def) 0 -4))))
    (if (null library)
	(error "Can't find library"))
    (if (string-match "\\(\\.elc?\\'\\)" library)
	(setq library (substring library 0 (match-beginning 1))))
    (let* ((path (or path find-function-source-path))
	   (compression (or (rassq 'jka-compr-handler file-name-handler-alist)
			    (member 'crypt-find-file-hook find-file-hooks)))
	   (filename (or (locate-library (concat library ".el")
					 t path)
			 (locate-library library t path)
			 (if compression
			     (or (locate-library (concat library ".el.gz")
						 t path)
				 (locate-library (concat library ".gz")
						 t path))))))
      (if (not filename)
	  (error "The library \"%s\" is not in the path." library))
      (save-excursion
	(set-buffer (find-file-noselect filename))
	(save-match-data
	  (let (;; avoid defconst, defgroup, defvar (any others?)
		(regexp (format "^\\s-*(def[^cgv\W]\\w+\\s-+%s\\s-" function))
		(syntable (syntax-table)))
	    (set-syntax-table emacs-lisp-mode-syntax-table)
	    (goto-char (point-min))
	    (if (prog1
		    (re-search-forward regexp nil t)
		  (set-syntax-table syntable))
		(progn
		  (beginning-of-line)
		  (list (current-buffer) (point)))
	      (error "Cannot find definition of %s" function))))))))

(defun function-at-point ()
  (or (condition-case ()
	  (let ((stab (syntax-table)))
	    (unwind-protect
		(save-excursion
		  (set-syntax-table emacs-lisp-mode-syntax-table)
		  (or (not (zerop (skip-syntax-backward "_w")))
		      (eq (char-syntax (char-after (point))) ?w)
		      (eq (char-syntax (char-after (point))) ?_)
		      (forward-sexp -1))
		  (skip-chars-forward "`'")
		  (let ((obj (read (current-buffer))))
		    (and (symbolp obj) (fboundp obj) obj)))
	      (set-syntax-table stab)))
	(error nil))
      (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	      (backward-up-list 1)
	      (forward-char 1)
	      (let (obj)
		(setq obj (read (current-buffer)))
		(and (symbolp obj) (fboundp obj) obj))))
	(error nil))))

(defun find-function-read-function ()
  "Read and return a function, defaulting to the one near point.

The function named by `find-function-function' is used to select the
default function."
  (let ((fn (funcall find-function-function))
	(enable-recursive-minibuffers t)
	val)
    (setq val (completing-read
	       (if fn
		   (format "Find function (default %s): " fn)
		 "Find function: ")
	       obarray 'fboundp t))
    (list (if (equal val "")
	      fn (intern val)))))

(defun find-function-do-it (function path switch-fn)
  "find elisp FUNCTION in PATH and display it with SWITCH-FN."
  (let ((buffer-point (find-function-noselect function path)))
    (if buffer-point
	(progn
	  (funcall switch-fn (car buffer-point))
	  (goto-char (elt buffer-point 1))
	  (recenter 0))))  )

(defun find-function (function &optional path)
  "Find the definition of the function near point in the current window.

Finds the emacs-lisp library containing the definition of the function
near point (selected by `find-function-function') and places point
before the definition.

If the optional argument PATH is given, the library where FUNCTION is
defined is searched in PATH instead of `load-path'"
  (interactive (find-function-read-function))
  (find-function-do-it function path 'switch-to-buffer))

(defun find-function-other-window (function &optional path)
  "Find the definition of the function near point in the other window.

Finds the emacs-lisp package containing the definition of the function
near point (selected by `find-function-function') and places point
before the definition.

If the optional argument PATH is given, the package where FUNCTION is
defined is searched in PATH instead of `load-path'"
  (interactive (find-function-read-function))
  (find-function-do-it function path 'switch-to-buffer-other-window))

(defun find-function-other-frame (function &optional path)
  "Find the definition of the function near point in the another frame.

Finds the emacs-lisp package containing the definition of the function
near point (selected by `find-function-function') and places point
before the definition.

If the optional argument PATH is given, the package where FUNCTION is
defined is searched in PATH instead of `load-path'"
  (interactive (find-function-read-function))
  (find-function-do-it function path 'switch-to-buffer-other-frame))

(defun find-function-on-key (key)
  "Find the function that KEY invokes.  KEY is a string."
  (interactive "kFind function on key: ")
  (let ((defn (if (fboundp 'key-or-menu-binding)
		  (key-or-menu-binding key)
		(key-binding key))))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      (if (and (consp defn) (not (eq 'lambda (car-safe defn))))
	  (message "runs %s" (prin1-to-string defn))
	(find-function-other-window defn)))))

;; define newer `locate-library', if not available (eg emacs-19)
(if (and (fboundp 'locate-library)
	 (condition-case nil
	     (locate-library "help" nil load-path)
	   (wrong-number-of-arguments t)))
    (defun locate-library (library &optional nosuffix path interactive-call)
      "Show the precise file name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'."
      (interactive (list (read-string "Locate library: ")
			 nil nil
			 t))
      (let (result)
	(catch 'answer
	  (mapcar
	   (lambda (dir)
	     (mapcar
	      (lambda (suf)
		(let ((try (expand-file-name (concat library suf) dir)))
		  (and (file-readable-p try)
		       (null (file-directory-p try))
		       (progn
			 (setq result try)
			 (throw 'answer try)))))
	      (if nosuffix
		  '("")
		(let ((basic '(".elc" ".el" ""))
		      (compressed '(".Z" ".gz" "")))
		  (if (or (rassq 'jka-compr-handler file-name-handler-alist)
			  (and (boundp 'find-file-hooks)
			       (member 'crypt-find-file-hook find-file-hooks)))
		      (apply 'nconc
			     (mapcar (lambda (compelt)
				       (mapcar (lambda (baselt)
						 (concat baselt compelt))
					       basic))
				     compressed))
		    basic)))))
	   (or path load-path)))
	(and interactive-call
	     (if result
		 (message "Library is file %s" result)
	       (message "No library %s in search path" library)))
	result)))

;; for emacs-19.28 and earlier
(if (not (fboundp 'describe-function-find-file))
    (defun describe-function-find-file (function)
      (let ((files load-history)
	    file)
	(while files
	  (if (memq function (cdr (car files)))
	      (setq file (car (car files)) files nil))
	  (setq files (cdr files)))
	file)))

(provide 'find-func)
;;; find-func.el ends here
