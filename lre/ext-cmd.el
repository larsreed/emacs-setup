;;; ext-cmd.el --- running external commands from Emacs

;; Copyright (C) 2002  Lars Reed

;; Author:		Lars Reed <Lars@kalars.net>
;; Version:		1.4
;; Keywords: running external commands

;; This file is NOT part of GNU Emacs.

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

;; This package contains

;;; Todo
;; * Get path from HKEY_CLASSES_ROOT\Applications\WINWORD.EXE\shell\Open\command
;; etc
;; * Process-lines?


;;; Code:

(eval-when-compile (require 'cl))

(defgroup ext-cmd nil
  "Running external commands from Emacs"
  :group 'tools
  :prefix "ext-")

(defcustom ext-java-program "java"
  "* External java interpreter"
  :group 'ext-cmd
  :type 'string)

(defcustom ext-javaw-program "javaw"
  "* External javaw interpreter"
  :group 'ext-cmd
  :type 'string)

(defcustom ext-java-options nil
  "* Standard options for java and javaw"
  :group 'ext-cmd
  :type '(repeat string))

(defcustom ext-awk-program "awk"
  "* External awk interpreter"
  :group 'ext-cmd
  :type 'string)

(defcustom ext-menu-path '("tools")
  "* Path in global menu for external menu
\(see `easy-menu-change'\)"
  :type '(choice (const :tag "Top Level" nil)
                 (sexp :tag "Menu Path"))
  :group 'ext-cmd)

(defcustom ext-menu-before nil
  "* The element before which to place the external menu.
`nil' to put it at the end."
  :group 'ext-cmd
  :type '(choice (string :tag "Name")
                 (const :tag "Last" nil)))

(defcustom ext-menu-title "External"
  "* The title for the global external commands menu."
  :group 'ext-cmd
  :type 'string)

(defcustom ext-discard-buffer nil
  "If non-nil, the name of a buffer that receives otherwise discarded output,
both stderr and stdout, from the different commands."
  :group 'ext-cmd
  :type 'string)

(defcustom ext-cmd-load-hook nil
   "* Hook run at end of loading the `ext-cmd' package."
  :group 'ext-cmd
  :type 'hook)

(defcustom ext-cmd-w32-office-path nil
  "Path to Office applications on Windows"
  :group 'ext-cmd
  :type 'string)

(defvar ext-cmd-list (list
		      (list "ls" "ls" 'source t)
		      (list "Notepad" "notepad.exe" 'editor
			    (eq window-system 'w32)
			    nil
			    nil
			    "%f")
		      (list "WinWord" "winword.exe" 'editor
			    (eq window-system 'w32)
			    (list (list 'path ext-cmd-w32-office-path))
			    nil
			    "%f")
		      (list "Excel" "excel.exe" 'editor
			    (eq window-system 'w32)
			    (list (list 'path ext-cmd-w32-office-path))
			    nil
			    "%f"))
  "* List of external commands.

This is a list of commands of the form
  \(TITLE EXE CLASS ENABLE-IF &optional ENV-SPEC HELP &rest ARGS\)
TITLE is either a string, or an expression that evaluates to a string
   \(normally evalled at startup\).
   The TITLE must be unique.
EXE is one of:
   \"exe-file\": a string
   \(JAVA \"class\" [\"java-opt\" ...]\) where JAVA is either java or javaw
   \(awk . \"script-name\"\)
CLASS is one of
   free: a free-standing program, not related to the current buffer
   editor: a program that may operate freely on the current buffer \(no
           interaction\)
           - if no arguments are given, %f is assumed (see below)
   filter: a program that receives the current buffer \(or region\) as input,
           and returns a possibly modified version to replace the input
   creator: a program that creates output for a NEW file, collected in a new
      buffer
   source: like filter, but the original text is not removed \(output is
           appended\)
   sink: like source, but output from the program is discarded
ENABLE-IF: list of prerequisites - used to enable/disable the menu item
   - there is an implicit `and' around this list.
   Note: if `mark-active' is in this list, the command operates on the
         region, rather than the entire buffer
   Note 2: if this element is nil, the command is not included in the list
ENV-SPEC: a list, where each element is either
   \(env \"variable\" \"value\"\) to add to the environment, or
   \(path \"value\" [append|prepend|set]\) to add to or replace the
      `exec-path'.
HELP: an optional tool tip

The rest of the elements are individual arguments for the command.

Among these, the following variables are replaced:
   %%: A '%'
   %f: The full name of the file in the current buffer
   %w: like %f, but slashes converted to backslashes
   %d: The directory name of the file in the current buffer
   %w: like %d, but slashes converted to backslashes
   %b: The base name of the file in the current directory \(no path\)
   %B: The buffer name of the current buffer
   %t: The temp directory
   %T: A generated, temporary file name
   %p: The PID of the current emacs process
")

(defconst ext-cmd-categories '(filter source sink editor creator free)
  "Legal classes")

(defvar ext-cmd-list-internal
  (list '(filter  . (["Shell command on region" shell-command-on-region
		      :active mark-active]))
	'(source  . nil)
	'(sink    . nil)
	'(editor  . nil)
	'(creator . nil)
	'(free    . (["Shell" shell-command])))
;;	      (if (eq window-system 'w32) ["Execute URL" shell-execute-url])
  "Internal commands for the different categories.
Format is \(CATEGORY . MENU-ITEMS\) where menu-items is a list of valid menu
items.")

(defun ext-element-exe (item)
  "Extract name of EXE from ITEM."
  (let ((exe (nth 1 item)))
    (cond ((stringp exe) exe)
	  ((eq (car exe) 'awk) ext-awk-program)
	  ((eq (car exe) 'java) ext-java-program)
	  ((eq (car exe) 'javaw) ext-javaw-program)
	  (t (error "Unknown exe-type")))))

(defun ext-element-prog-name (item)
  "Extract name of program from ITEM."
  (let ((exe (nth 1 item)))
    (cond ((stringp exe) exe)
	  ((eq (car exe) 'awk) (cdr exe))
	  ((eq (car exe) 'java) (nth 2 exe))
	  ((eq (car exe) 'javaw) (nth 2 exe))
	  (t (error "Unknown exe-type")))))

(defsubst ext-element-title (item)
  "Extract title from ITEM."
  (let ((title (nth 0 item)))
    (if (null title) (setq title (ext-element-exe item)))
    (if (stringp title) title
      (eval title))))

(defsubst ext-element-class (item)
  "Extract class from ITEM."
  (let ((e (nth 2 item)))
    (if (member e ext-cmd-categories) e nil)))

(defsubst ext-element-on-region (item)
  "Check if command operates on region"
  (let ((x (nth 3 item)))
    (not (null (or (eq x 'mark-active)
		   (and (listp x)
			(member 'mark-active x)))))))

(defun ext-element-enable (item)
  "Extract enable clause from ITEM."
  (let ((org (nth 3 item))
	(class (ext-element-class item))
	(on-reg (ext-element-on-region item))
	stdlist)
    (if (and org (not (listp org))) (setq org (list org)))
    (setq stdlist
	  (cond ((member class '(editor sink)) '(buffer-file-name))
		((member class '(source filter)) '(not buffer-read-only))
		(t t)))
    (if org (append (list 'and stdlist) org)
      nil)))

(defun ext-element-pathspec (item)
  "Extract path specification from ITEM."
  (let ((env (nth 4 item))
	(npath (purecopy exec-path))
	pspec)
    (while env
      (setq pspec (car env))
      (if (eq (car pspec) 'path)
	  (cond ((or (= (length pspec) 2)
		     (eq 'append (nth 2 pspec)))
		 (setq npath (append npath (list (nth 1 pspec)))))
		((eq 'prepend (nth 2 pspec))
		 (setq npath (append (list (nth 1 pspec)) npath)))
		((eq 'set (nth 2 pspec))
		 (setq npath (list (nth 1 pspec))))
		(t (error "Illegal path specification"))))
      (setq env (cdr env)))
    npath))

(defun ext-element-envspec (item)
  "Extract environment settings from ITEM."
  (let ((env (nth 4 item))
	(envlist (purecopy process-environment))
	espec)
    (while env
      (setq espec (car env))
      (if (eq (car espec) 'env)
	  (setq envlist
		(cons (concat (nth 1 espec) "=" (nth 2 espec))
			envlist)))
      (setq env (cdr env)))
    envlist))

(defun ext-element-help-string (item)
  "Extract help string from ITEM."
  (let ((hs (nth 5 item))
	(class (ext-element-class item))
	(prog-n (ext-element-prog-name item)))
    (cond (hs hs)
	  ((eq class 'filter)  (concat "Run through " prog-n))
	  ((eq class 'source)  (concat "Get input from " prog-n))
	  ((eq class 'sink)    (concat "Send to " prog-n))
	  ((eq class 'editor)  (concat "Open file in " prog-n))
	  ((eq class 'creator) (concat "Create new file with " prog-n))
	  ((eq class 'free) (concat "Run " prog-n)))))

(defvar ext-element-temp-counter 0 "Counter for temp names")
(defvar ext-element-temp-bname "nofile" "Temporary buffer name...")

(defun ext-subst-1 (key alist)
  (let ((res (cdr-safe (assoc key alist))))
    (if (null res)
	(concat "%" (if (= key ?%) "" (char-to-string key)))
      (if (stringp res) res
	(eval res)))))

(defun ext-subst-in-string (str subst-list)
  (loop for i from 0 to (1- (length str))
	as chr = (aref str i)
	concat (if (eq chr ?%)
		   (ext-subst-1 (aref str (incf i)) subst-list)
		 (char-to-string (aref str i)))))


(defun ext-element-substitute-percent (arglist &optional do-reverse)
  "Do %-substitution in ARGLIST."
  (let* (nlist
	 (ext-element-temp-bname (or buffer-file-name (concat
						       (file-name-as-directory
							default-directory)
						       "nofile")))
	 (ext-element-temp-counter 0)
	 (rep-list '((?T . (make-temp-name
			    (format "%sx%d" temporary-file-directory
				    ext-element-temp-counter)))
		     (?t . temporary-file-directory)
		     (?b . (file-name-nondirectory ext-element-temp-bname))
		     (?B . (buffer-name))
		     (?f . ext-element-temp-bname)
		     (?d . (file-name-directory ext-element-temp-bname))
		     (?W . (replace-regexp-in-string ;; I hate backslash!
			     "/" "\\\\"
			     ext-element-temp-bname))
		     (?w . (replace-regexp-in-string ;; I hate backslash!
			     "/" "\\\\"
			     (file-name-directory ext-element-temp-bname)))))
	 (case-fold-search nil))
    (while arglist
      (incf ext-element-temp-counter)
      (setq nlist
	    (append (list (ext-subst-in-string (car arglist) rep-list))
		    nlist)
	    arglist (cdr arglist)))
    (if do-reverse
	(nreverse nlist)
      nlist)))

(defun ext-element-arguments (item)
  "Extract list of program arguments from ITEM."
  (let (arglist i j
	(exe (nth 1 item)))
    (cond ((stringp exe) t)
	  ((eq (car exe) 'awk) (setq arglist (list (concat "-f" (cadr exe)))))
	  ((or (eq (car exe) 'java)
	       (eq (car exe) 'javaw))
	   (setq arglist
		 (if ext-java-options (append (list (nth 1 exe))
					      (reverse (nthcdr 2 exe))
					      (reverse ext-java-options))
		   (nthcdr 1 exe))))
	  (t (error "Unknown exe-type")))
    (setq i 6
	  j (length item))
    (while (< i j)
      (setq arglist (append (list (nth i item)) arglist)
	    i (1+ i)))
    (if arglist
	(ext-element-substitute-percent arglist)
      (if (eq (ext-element-class item) 'editor)
	  (ext-element-substitute-percent '("%f"))))))

(defun ext-element-menu-command (name)
  "Run command when selected in menu"
  (interactive "x")
  (let ((xbuf (if ext-discard-buffer (get-buffer-create ext-discard-buffer)))
	(item (assoc name ext-cmd-list))
	cmd	title	exe	class	path	env	help	args)
    (if (not item)
	(error "Unknown command: %s" (prin1-to-string name))
      (setq title (ext-element-title       item)
	    exe	  (ext-element-exe         item)
	    class (ext-element-class       item)
	    path  (ext-element-pathspec    item)
	    env   (ext-element-envspec     item)
	    args  (ext-element-arguments   item)
	    help  (ext-element-help-string item))
      (let ((process-environment env)
	    (exec-path path))
	(cond ((member class '(free editor))
	       (message "%s" help)
	       (apply 'start-process title xbuf exe args))
	      ((eq class 'source)
	       (message "%s" help)
	       (apply 'call-process exe nil (list (current-buffer) t)
		      t args))
	      ((eq class 'sink)
	       (if (and (buffer-modified-p)
			(y-or-n-p "Save buffer first"))
		   (if (buffer-file-name)
		       (save-buffer)
		     (call-interactively 'write-file)))
	       (message "%s" help)
	       (if (buffer-file-name)
		   (apply 'call-process exe (buffer-file-name) (list xbuf t)
			  nil args)
		 (error "Buffer is not visiting a file")))
	      ((eq class 'creator)
	       (message "%s" help)
	       (apply 'call-process exe nil (list (get-buffer-create title)
						  xbuf)
		      t args))
	      ((eq class 'filter)
	       (message "%s" help)
	       (if mark-active
		   (shell-command-on-region (region-beginning)
					    (region-end)
					    (apply 'concat exe " " args)
					    t
					    t
					    xbuf)
		 (error "No region is active")))
	      (t (error "Unknown command call %s" (prin1-to-string class))))
	))))

(defun ext-create-menu-item (item)
  "Create a menu item from ITEM.
It consists of the given title, and `ext-element-menu-command' with the title
as argument."
  (vector (ext-element-title item)
	  (list 'ext-element-menu-command  (ext-element-title item))
	  :help (ext-element-help-string item)
	  :active (ext-element-enable item)))

(defun ext-create-submenu (cat &optional menu-list)
  "Create a list of menu items containing only those elements of class CAT.
The sublist is created from `ext-cmd-list' unless MENU-LIST is given."
  (let ((m-list (or menu-list
		    ext-cmd-list))
	(result (cdr-safe (assoc cat ext-cmd-list-internal))))
    (mapc (function
           (lambda (item)
             (if (and (eq (ext-element-class item) cat)
                      (not (eq (ext-element-enable item) nil)))
                 (setq result (append (list (ext-create-menu-item item))
                                      result)))))
          m-list)
    result))

(defun ext-create-menu (&optional menu-list)
  "Create the complete menu.
The menu is created from `ext-cmd-list' unless MENU-LIST is given."
  (let (reslist sublist)
    (dolist (currcat ext-cmd-categories (nreverse reslist))
      (setq sublist (ext-create-submenu currcat menu-list))
      (if (and sublist reslist) (setq reslist (cons "---" reslist)))
      (if sublist (setq reslist (append sublist reslist))))))

(defun ext-global-menu()
  "Bind the global external commands menu"
  (interactive)
  (easy-menu-change ext-menu-path
		    ext-menu-title
		    (ext-create-menu)
		    ext-menu-before))

(provide 'ext-cmd)

(run-hooks 'ext-cmd-load-hook)

;;; ext-cmd.el ends here
