;;; project-am.el --- A project management scheme based on automake files.

;;;  Copyright (C) 1998, 1999, 2000, 2003  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.0.3
;; Keywords: project, make
;; RCS: $Id: project-am.el,v 1.24 2003/04/14 12:30:23 zappo Exp $

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
;; 
;; The GNU Automake tool is the first step towards having a really
;; good project management system.  It provides a simple and concise
;; look at what is actually in a project, and records it in a simple
;; fashion.
;;
;; project-am uses the structure defined in all good GNU projects with
;; the Automake file as it's base template, and then maintains that
;; information during edits, automatically updating the automake file
;; where appropriate.

;;; History:
;; 

(eval-and-compile
  ;; Compatibility for makefile mode.
  (condition-case nil
      (require 'makefile "make-mode")
    (error (require 'make-mode)))

  (require 'eieio)
  (require 'ede))

(eval-when-compile (require 'ede-speedbar))
(eval-when-compile (require 'compile))

;; customization stuff
(defgroup project-am nil
  "File and tag browser frame."
  :group 'tools
  :group 'ede
  )

(defcustom project-am-compile-project-command nil
  "*Default command used to compile a project."
  :group 'project-am
  :type 'string)

(defcustom project-am-compile-target-command "make -k %s"
  "*Default command used to compile a project."
  :group 'project-am
  :type 'string)

(defcustom project-am-debug-target-function 'gdb
  "*Default Emacs command used to debug a target."
  :group 'project-am
  :type 'sexp) ; make this be a list some day

(defconst project-am-type-alist
  '(("bin" project-am-program "bin_PROGRAMS")
    ("sbin" project-am-program "sbin_PROGRAMS")
    ("lib" project-am-lib "noinst_LIBS")
    ("texinfo" project-am-texinfo "texinfo_TEXINFOS")
    ("man" project-am-man "man_MANS")
    ("lisp" project-am-lisp "lisp_LISP"))
  "Alist of type names and the type of object to create for them.")

(defclass project-am-target (ede-target)
  nil
  "Base target class for everything in project-am.")

(defclass project-am-objectcode (project-am-target)
  ((source :initarg :source :documentation "List of source files."))
  "A target which creates object code, like a C program or library.")

(defclass project-am-program (project-am-objectcode)
  ((ldadd :initarg :ldadd :documentation "Additional LD args."
	  :initform nil))
  "A top level program to build")

(defclass project-am-lib (project-am-objectcode)
  nil
  "A top level library to build")

(defclass project-am-lisp (project-am-target)
  ((lisp :initarg :lisp :documentation "List of lisp files to build."))
  "A group of Emacs Lisp programs to byte compile.")

(defclass project-am-texinfo (project-am-target)
  ((include :initarg :include
	    :initform nil
	    :documentation "Additional texinfo included in this one."))
  "A top level texinfo file to build.")

(defclass project-am-man (project-am-target)
  nil
  "A top level man file to build.")

(defclass project-am-makefile (ede-project)
  ((targets :initarg :targets
	    :initform nil
	    :documentation "Top level targets in this makefile.")
   )
  "Encode one makefile.")

;;; Code:
(defmethod project-add-file ((ot project-am-target))
  "Add the current buffer into a project.
OT is the object target.  DIR is the directory to start in."
  (let* ((target (if ede-object (error "Already assocated w/ a target")
		   (let ((amf (project-am-load default-directory)))
		     (if (not amf) (error "No project file"))
		     (completing-read "Target: "
				      (object-assoc-list 'name
							 (oref amf targets))
				      nil t))))
	 ;; The input target might be new.  See if we can find it.
	 (amf (ede-load-project-file (oref ot path)))
	 (ot (object-assoc target 'name (oref amf targets)))
	 (ofn (file-name-nondirectory (buffer-file-name))))
    (if (not ot)
	(setq ot
	      (project-new-target
	       target (project-am-preferred-target-type (buffer-file-name)))))
    (ede-with-projectfile ot
      (makefile-move-to-macro (project-am-macro ot))
      (ede-maybe-checkout)
      (makefile-end-of-command)
      (insert " " ofn)
      (makefile-fill-paragraph nil)
      (project-rescan ot)
      (save-buffer))
    (setq ede-object ot)))

(defmethod project-remove-file ((ot project-am-target) fnnd)
  "Remove the current buffer from any project targets."
  (ede-with-projectfile ot
    (makefile-move-to-macro (project-am-macro ot))
    (if (and buffer-read-only vc-mode
	     (y-or-n-p "Checkout Makefile.am from VC? "))
	(vc-toggle-read-only t))
    (ede-maybe-checkout)
    (makefile-navigate-macro (concat " *" (regexp-quote (ede-name fnnd))))
    (replace-match "" t t nil 0)
    (makefile-fill-paragraph nil)
    (project-rescan ot)
    (save-buffer))
  (setq ede-object nil))

(defmethod project-edit-file-target ((obj project-am-target))
  "Edit the target associated w/ this file."
  (find-file (concat (oref obj path) "Makefile.am"))
  (goto-char (point-min))
  (makefile-move-to-macro (project-am-macro obj))
  (if (= (point-min) (point))
      (re-search-forward (ede-target-name obj))))

(defmethod project-new-target ((proj project-am-makefile))
  "Create a new target named NAME.
Argument TYPE is the type of target to insert.  This is a string
matching something in `project-am-type-alist' or type class symbol.
Despite the fact that this is a method, it depends on the current
buffer being in order to provide a smart default target type."
  (let* ((name (read-string "Name: " ""))
	 (type (completing-read "Type: "
				project-am-type-alist
				nil t
				(cond ((eq major-mode 'texinfo-mode)
				       "texinfo")
				      ((eq major-mode 'nroff-mode)
				       "man")
				      ((eq major-mode 'emacs-lisp-mode)
				       "lisp")
				      (t "bin"))))
	 (ntype (assoc type project-am-type-alist))
	 (ot nil))
    (setq ot (apply (car (cdr ntype)) name :name name
		    :path (expand-file-name default-directory) nil))
    (if (not ot) (error "Error creating target object %S" ntype))
    (ede-with-projectfile ot
      (goto-char (point-min))
      (ede-maybe-checkout)
      (makefile-next-dependency)
      (if (= (point) (point-min))
	  (goto-char (point-max))
	(beginning-of-line)
	(insert "\n")
	(forward-char -1))
      ;; Add the new target sources macro (if needed)
      (if (project-am-macro ot)
	  (makefile-insert-macro (project-am-macro ot)))
      ;; Add to the list of objects.
      (goto-char (point-min))
      (makefile-move-to-macro (car (cdr (cdr ntype))))
      (if (= (point) (point-min))
	  (progn
	    (if (re-search-forward makefile-macroassign-regex nil t)
		(progn (forward-line -1)
		       (end-of-line)
		       (insert "\n"))
	      ;; If the above search fails, thats ok.  We'd just want to be at
	      ;; point-min anyway.
	      )
	    (makefile-insert-macro (car (cdr (cdr ntype))))))
      (makefile-end-of-command)
      (insert " " (ede-target-name ot))
      (save-buffer)
      ;; Rescan the object in this makefile.
      (project-rescan ede-object))))

;(defun project-am-rescan-toplevel ()
;  "Rescan all projects in which the current buffer resides."
;  (interactive)
;  (let* ((tlof (project-am-find-topmost-level default-directory))
;	 (tlo (project-am-load tlof))
;	 (ede-deep-rescan t))  ; scan deep in this case.
;    ;; tlo is the top level object for whatever file we are in
;    ;; or nil.  If we have an object, call the rescan method.
;    (if tlo (project-am-rescan tlo))))

;;
;; NOTE TO SELF
;;
;;  This should be handled at the EDE level, calling a method of the
;; top most project.
;;
(defmethod project-compile-project ((obj project-am-target) &optional command)
  "Compile the entire current project.
Argument COMMAND is the command to use when compiling."
  (require 'compile)
  (if (not command)
      (setq
       command
       ;; This interactive statement was taken from compile, and I'll
       ;; use the same command history too.
       (progn
	 (if (not project-am-compile-project-command)
	     (setq project-am-compile-project-command compile-command))
	 (if (or compilation-read-command current-prefix-arg)
	     (read-from-minibuffer "Project compile command: "
				   ;; hardcode make -k
				   ;; This is compile project after all.
				   project-am-compile-project-command
				   nil nil '(compile-history . 1))
	   project-am-compile-project-command))))
  ;; When compile a project, we might be in a subdirectory,
  ;; so we have to make sure we move all the way to the top.
  (let* ((default-directory (project-am-find-topmost-level default-directory)))
    (compile command)))

(defmethod project-compile-project ((obj project-am-makefile) 
				    &optional command)
  "Compile the entire current project.
Argument COMMAND is the command to use when compiling."
  (require 'compile)
  (if (not command)
      (setq
       command
       ;; This interactive statement was taken from compile, and I'll
       ;; use the same command history too.
       (progn
	 (if (not project-am-compile-project-command)
	     (setq project-am-compile-project-command compile-command))
	 (if (or compilation-read-command current-prefix-arg)
	     (read-from-minibuffer "Project compile command: "
				   ;; hardcode make -k
				   ;; This is compile project after all.
				   project-am-compile-project-command
				   nil nil '(compile-history . 1))
	   project-am-compile-project-command))))
  ;; When compile a project, we might be in a subdirectory,
  ;; so we have to make sure we move all the way to the top.
  (let* ((default-directory (project-am-find-topmost-level default-directory)))
    (compile command)))

(defmethod project-compile-target ((obj project-am-target) &optional command)
  "Compile the current target.
Argument COMMAND is the command to use for compiling the target."
  (require 'compile)
  (if (not project-am-compile-project-command)
      (setq project-am-compile-project-command compile-command))
  (if (not command)
      (setq
       command
       (if compilation-read-command
	   (read-from-minibuffer "Project compile command: "
				 ;; hardcode make -k
				 ;; This is compile project after all.
				 (if ede-object
				     (format
				      project-am-compile-target-command
				      (project-compile-target-command
				       ede-object))
				   project-am-compile-target-command)
				 nil nil
				 '(compile-history . 1))
	 (if ede-object
	     project-am-compile-project-command
	   (format
	    project-am-compile-target-command
	    (project-compile-target-command ede-object))))))
  ;; We better be in the right place when compiling a specific target.
  (compile command))

(defmethod project-debug-target ((obj project-am-objectcode))
  "Run the current project target in a debugger."
  (let ((tb (get-buffer-create " *padt*"))
	(dd (oref obj path))
	(cmd nil))
    (unwind-protect
	(progn
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (read-from-minibuffer
		     "Run (like this): "
		     (concat (symbol-name project-am-debug-target-function)
			     " " (ede-target-name obj))))
	  (funcall project-am-debug-target-function cmd))
      (kill-buffer tb))))

(defmethod project-make-dist ((this project-am-target))
  "Run the current project in the debugger."
  (require 'compile)
  (if (not project-am-compile-project-command)
      (setq project-am-compile-project-command compile-command))
  (project-compile-project this (concat project-am-compile-project-command
					" dist")))

;;; Project loading and saving
;;
(defun project-am-load (project)
  "Read an automakefile PROJECT into our data structure.
Make sure that the tree down to our makefile is complete so that there
is cohesion in the project.  Return the project file (or sub-project).
If a given set of projects has already been loaded, then do nothing
but return the project for the directory given."
  (setq project (expand-file-name project))
  (let* ((ede-constructing t)
	 (fn (project-am-find-topmost-level
	      (if (string-match "/$" project)
		  project
		(concat project "/"))))
	 (amo nil)
	 (trimmed (if (string-match (regexp-quote fn)
				    project)
		      (replace-match "" t t project)
		    ""))
	 (subdir nil))
    (setq amo (object-assoc (concat fn "Makefile.am")
			    'file ede-projects))
    (if amo
	(error "synchronous error in ede/project-am objects.")
      (let ((project-am-constructiong t))
	(setq amo (project-am-load-makefile fn))))
    (if (not amo)
	nil
      ;; Now scan down from amo, and find the current directory
      ;; from the PROJECT file.
      (while (< 0 (length trimmed))
	(if (string-match "\\([a-zA-Z0-9.-]+\\)/" trimmed)
	    (setq subdir (match-string 0 trimmed)
		  trimmed (replace-match "" t t trimmed))
	  (error "Error scanning down path for project"))
	(setq amo (project-am-subtree amo (concat fn subdir "Makefile.am"))
	      fn (concat fn subdir)))
      amo)
    ))

(defun project-am-find-topmost-level (path)
  "Find the topmost automakefile starting with PATH."
  (let ((newpath path))
    (while (file-exists-p (concat newpath "Makefile.am"))
      (setq path newpath newpath
	    (file-name-directory (substring path 0 (1- (length path))))))
    (expand-file-name path)))

(defun project-am-load-makefile (path)
  "Converts PATH into a project Makefile, and return it's object object.
It does not check for existing project objects.  Use `project-am-load'."
  (let* ((fn (expand-file-name (concat path "Makefile.am")))
	 (kb (get-file-buffer fn)))
    (if (not (file-exists-p fn))
	nil
      (save-excursion
	(set-buffer (find-file-noselect fn))
	(prog1
	    (if (and ede-object (project-am-makefile-p ede-object))
		ede-object
	      (let ((ampf (project-am-makefile (project-am-last-dir fn)
					       :name (project-am-last-dir fn)
					       :file fn)))
		(project-rescan ampf)
		(make-local-variable 'ede-object)
		(setq ede-object ampf)
		ampf))
	  ;; If the buffer was not already loaded, kill it.
	  (if (not kb) (kill-buffer (current-buffer))))))))

;;; Methods:
(defmethod ede-find-target ((amf project-am-makefile) buffer)
  "Fetch the target belonging to BUFFER."
  (or (call-next-method)
      (let ((targ (oref amf targets))
	    (sobj (oref amf subproj))
	    (obj nil))
	(while (and targ (not obj))
	  (if (ede-buffer-mine (car targ) buffer)
	      (setq obj (car targ)))
	  (setq targ (cdr targ)))
	(while (and sobj (not obj))
	  (setq obj (project-am-buffer-object (car sobj) buffer)
		sobj (cdr sobj)))
	obj)))

(defmethod project-targets-for-file ((proj project-am-makefile))
  "Return a list of targets the project PROJ."
  (oref proj targets))

(defmethod project-rescan ((this project-am-makefile))
  "Rescan the makefile for all targets and sub targets."
  (let ((osubproj (oref this subproj))
	(otargets (oref this targets))
	(csubproj (or
		   ;; If DIST_SUBDIRS doesn't exist, then go for the
		   ;; static list of SUBDIRS.  The DIST version should
		   ;; contain SUBDIRS plus extra stuff.
		   (makefile-macro-file-list "DIST_SUBDIRS")
		   (makefile-macro-file-list "SUBDIRS")))
	(nsubproj nil)
	;; Targets are excluded here because they require
	;; special attention.
	(ntargets nil)
	(tmp nil)
	;; Here are target prefixes as strings
	(tp '(("bin_PROGRAMS" . project-am-program)
	      ("sbin_PROGRAMS" . project-am-program)
	      ("noinst_LIBRARIES" . project-am-lib)
	      ("info_TEXINFOS" . project-am-texinfo)
	      ("man_MANS" . project-am-man)))
	(path (expand-file-name default-directory))
	)
    (mapcar
     ;; Map all tye different types
     (lambda (typecar)
       ;; Map all the found objects
       (mapcar (lambda (lstcar)
		 (setq tmp (object-assoc lstcar 'name otargets))
		 (if (not tmp)
		     (setq tmp (apply (cdr typecar) lstcar
				      :name lstcar
				      :path path nil)))
		 (project-rescan tmp)
		 (setq ntargets (cons tmp ntargets)))
	       (makefile-macro-file-list (car typecar))))
     tp)
    ;; LISP is different.  Here there is only one kind of lisp (that I know of
    ;; anyway) so it doesn't get mapped when it is found.
    (if (makefile-move-to-macro "lisp_LISP")
	(let ((tmp (project-am-lisp "lisp"
				    :name "lisp"
				    :path path)))
	  (project-rescan tmp)
	  (setq ntargets (cons tmp ntargets))))
    ;; Now that we have this new list, chuck the old targets
    ;; and replace it with the new list of targets I just created.
    (oset this targets (nreverse ntargets))
    ;; We still have a list of targets.  For all buffers, make sure
    ;; their object still exists!

    ;; FIGURE THIS OUT

    ;; Ok, now lets look at all our sub-projects.
    (mapcar (lambda (sp)
	      ;; For each project id found, see if we need to recycle,
	      ;; and if we do not, then make a new one.  Check the deep
	      ;; rescan value for behavior patterns.
	      (setq tmp (object-assoc
			 (concat default-directory sp "/Makefile.am")
			 'file osubproj))
	      (if (not tmp)
		  ;; No tmp?  Create a new one.  Don't bother with
		  ;; non-deep business since we need this object.
		  (setq tmp (project-am-load-makefile
			     (concat (file-name-directory (oref this :file))
				     sp "/")))
		;; If we have tmp, then rescan it only if deep mode.
		(if ede-deep-rescan
		    (project-rescan tmp)))
	      ;; Tac tmp onto our list of things to keep
	      (setq nsubproj (cons tmp nsubproj)))
	    csubproj)
    (oset this subproj nsubproj)
    ;; All elements should be updated now.
    ))

(defmethod project-rescan ((this project-am-program))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this)))
  (oset this :ldadd (makefile-macro-file-list
		     (concat (oref this :name) "_LDADD"))))

(defmethod project-rescan ((this project-am-lib))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this))))

(defmethod project-rescan ((this project-am-texinfo))
  "Rescan object THIS."
  (oset this :include (makefile-macro-file-list (project-am-macro this))))

(defmethod project-rescan ((this project-am-man))
  "Rescan object THIS."
  )

(defmethod project-rescan ((this project-am-lisp))
  "Rescan the lisp sources."
  (oset this :lisp (makefile-macro-file-list (project-am-macro this))))

(defmethod project-am-macro ((this project-am-objectcode))
  "Return the default macro to 'edit' for this object type."
  (concat (oref this :name) "_SOURCES"))

(defmethod project-am-macro ((this project-am-texinfo))
  "Return the default macro to 'edit' for this object type."
  (concat (oref this :name) "_TEXINFOS"))

(defmethod project-am-macro ((this project-am-man))
  "Return the default macro to 'edit' for this object type."
  (concat (oref this :name) "_MANS"))

(defmethod project-am-macro ((this project-am-lisp))
  "Return the default macro to 'edit' for this object."
  "lisp_LISP")

(defun project-am-buffer-object (amf buffer)
  "Return an object starting with AMF associated with BUFFER.
nil means that this buffer belongs to no-one."
  (if (not amf)
      nil
    (if (ede-buffer-mine amf buffer)
	amf
      (let ((targ (oref amf targets))
	    (sobj (oref amf subproj))
	    (obj nil))
	(while (and targ (not obj))
	  (if (ede-buffer-mine (car targ) buffer)
	      (setq obj (car targ)))
	  (setq targ (cdr targ)))
	(while (and sobj (not obj))
	  (setq obj (project-am-buffer-object (car sobj) buffer)
		sobj (cdr sobj)))
	obj))))
  
(defmethod ede-buffer-mine ((this project-am-makefile) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (string= (oref this :file) (expand-file-name (buffer-file-name buffer))))

(defmethod ede-buffer-mine ((this project-am-objectcode) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (member (file-name-nondirectory (buffer-file-name buffer))
	  (oref this :source)))

(defmethod ede-buffer-mine ((this project-am-texinfo) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (let ((bfn (buffer-file-name buffer)))
    (or (string= (oref this :name)  (file-name-nondirectory bfn))
	(member (file-name-nondirectory bfn) (oref this :include)))))
	
(defmethod ede-buffer-mine ((this project-am-man) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (string= (oref this :name) (buffer-file-name buffer)))

(defmethod ede-buffer-mine ((this project-am-lisp) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (member (file-name-nondirectory (buffer-file-name buffer))
	  (oref this :lisp)))

(defmethod project-am-subtree ((ampf project-am-makefile) subpath)
  "Return the sub project in AMPF specified by SUBPATH."
  (object-assoc (expand-file-name subpath) 'file (oref ampf subproj)))

(defmethod project-compile-target-command ((this project-am-target))
  "Default target to use when compiling a given target."
  ;; This is a pretty good default for most.
  "")

(defmethod project-compile-target-command ((this project-am-objectcode))
  "Default target to use when compiling an object code target."
  (oref this :name))

(defmethod project-compile-target-command ((this project-am-texinfo))
  "Default target t- use when compling a texinfo file."
  (let ((n (oref this :name)))
    (if (string-match "\\.texi?\\(nfo\\)?" n)
	(setq n (replace-match ".info" t t n)))
    n))


;;; Generic useful functions

(defun project-am-last-dir (file)
  "Return the last part of a directory name.
Argument FILE is the file to extract the end directory name from."
  (let ((s (file-name-directory file)))
    (string-match "/\\([a-zA-Z0-9_]+\\)/$" s)
    (match-string 1 s)))

(defun project-am-preferred-target-type (file)
  "For FILE, return the preferred type for that file."
  (cond ((string-match "\\.texi?\\(nfo\\)$" file)
	 project-am-texinfo)
	((string-match "\\.[0-9]$" file)
	 project-am-man)
	((string-match "\\.el$" file)
	 project-am-lisp)
	(t
	 project-am-program)))

(defmethod ede-buffer-header-file((this project-am-objectcode) buffer)
  "There are no default header files."
  (or (call-next-method)
      (let ((s (oref this source))
	    (found nil))
	(while (and s (not found))
	  ;; Add more logic here if applicable.
	  (if (string-match "\\.\\(h\\|H\\|hh\\|hpp\\)" (car s))
	      (setq found (car s)))
	  (setq s (cdr s)))
	found)))

(defmethod ede-documentation ((this project-am-texinfo))
  "Return a list of files that provides documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  (append (oref this source)
	  (oref this include)))


;;; Makefile editing and scanning commands
;;
;; Formatting of a makefile
;;
;; 1) Creating an automakefile, stick in a top level comment about
;;    being created by emacs
;; 2) Leave order of variable contents alone, except for SOURCE
;;    SOURCE always keep in the order of .c, .h, the other stuff.

;; personal reference until I'm done
; makefile-fill-paragraph -- refill a macro w/ backslashes
; makefile-insert-macro -- insert "foo = "

(defun makefile-beginning-of-command ()
  "Move the the beginning of the current command."
  (interactive)
  (if (save-excursion
	(forward-line -1)
	(makefile-line-continued-p))
      (forward-line -1))
  (beginning-of-line)
  (if (not (makefile-line-continued-p))
      nil
    (while (and (makefile-line-continued-p)
		(not (bobp)))
      (forward-line -1))
    (forward-line 1)))

(defun makefile-end-of-command ()
  "Move the the beginning of the current command."
  (interactive)
  (end-of-line)
  (while (and (makefile-line-continued-p)
	      (not (eobp)))
    (forward-line 1)
    (end-of-line)))

(defun makefile-line-continued-p ()
  "Return non-nil if the current line ends in continuation."
  (save-excursion
    (end-of-line)
    (= (preceding-char) ?\\)))

;;; Programatic editing of a Makefile
;;
(defun makefile-move-to-macro (macro)
  "Move to the definition of MACRO.  Return t if found."
  (let ((oldpt (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" macro "\\s-*=") nil t)
	t
      (goto-char oldpt)
      nil)))

(defun makefile-navigate-macro (stop-before)
  "In a list of files, move forward until STOP-BEFORE is reached.
STOP-BEFORE is a regular expression matching a file name."
  (save-excursion
    (makefile-beginning-of-command)
    (let ((e (save-excursion
	       (makefile-end-of-command)
	       (point))))
      (if (re-search-forward stop-before nil t)
	  (goto-char (match-beginning 0))
	(goto-char e)))))

(defun makefile-macro-file-list (macro)
  "Return a list of all files in MACRO."
  (save-excursion
    (if (makefile-move-to-macro macro)
	(let ((e (save-excursion
		   (makefile-end-of-command)
		   (point)))
	      (lst nil))
	  (while (re-search-forward "\\s-**\\([-a-zA-Z0-9./_@$%()]+\\)\\s-*" e t)
	    (setq lst (cons
		       (buffer-substring-no-properties
			(match-beginning 1)
			(match-end 1))
		       lst)))
	  (nreverse lst)))))

;;; Methods used in speedbar
;;
(defmethod ede-sb-button ((this project-am-program) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this (ede-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-lib) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this (ede-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-texinfo) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'bracket ?+
			  'ede-object-expand
			  this
			  (ede-name this)
			  'ede-file-find
			  (concat (oref this :path)
				  (oref this :name))
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-man) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'bracket ?? nil nil
			  (ede-name this)
			  'ede-file-find
			  (concat (oref this :path)
				  (oref this :name))
			  'speedbar-file-face depth))

(defmethod ede-sb-button ((this project-am-lisp) depth)
  "Create a speedbar button for object THIS at DEPTH."
  (speedbar-make-tag-line 'angle ?+
			  'ede-object-expand
			  this (ede-name this)
			  nil nil  ; nothing to jump to
			  'speedbar-file-face depth))

(defmethod ede-sb-expand ((this project-am-objectcode) depth)
  "Expand node describing something built into objectcode.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((sources (oref this :source)))
    (while sources
      (speedbar-make-tag-line 'bracket ?+
			      'ede-tag-file
			      (concat (oref this :path)
				      (car sources))
			      (car sources)
			      'ede-file-find
			      (concat
			       (oref this :path)
			       (car sources))
			      'speedbar-file-face depth)
      (setq sources (cdr sources)))))

(defmethod ede-sb-expand ((this project-am-texinfo) depth)
  "Expand node describing a texinfo manual.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((includes (oref this :include)))
    (while includes
      (speedbar-make-tag-line 'bracket ?+
			      'ede-tag-file
			      (concat (oref this :path)
				      (car includes))
			      (car includes)
			      'ede-file-find
			      (concat
			       (oref this :path)
			       (car includes))
			      'speedbar-file-face depth)
      (setq includes (cdr includes)))
    ;; Not only do we show the included files (for future expansion)
    ;; but we also want to display tags for this file too.
    (ede-create-tag-buttons (concat (oref this :path)
					   (oref this :name))
				   depth)))

(defmethod ede-sb-expand ((this project-am-lisp) depth)
  "Expand node describing lisp code.
TEXT is the text clicked on.  TOKEN is the object we are expanding from.
INDENT is the current indentatin level."
  (let ((sources (oref this :lisp)))
    (while sources
      (speedbar-make-tag-line 'bracket ?+
			      'ede-tag-file
			      (concat (oref this :path)
				      (car sources))
			      (car sources)
			      'ede-file-find
			      (concat
			       (oref this :path)
			       (car sources))
			      'speedbar-file-face depth)
      (setq sources (cdr sources)))))

(provide 'project-am)

;;; project-am.el ends here
