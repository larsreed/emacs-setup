;;; elint.el -- Lint Emacs Lisp

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; $Id: elint.el,v 1.10 1997/09/22 20:55:04 petli Exp $

;; Author: Peter Liljenberg <petli@lysator.liu.se>
;; Created: May 1997
;; Keywords: lint debugging
;; Availability: http://www.lysator.liu.se/~petli/elisp

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

;; This is a linter for Emacs Lisp. Currently, it mainly catches
;; mispellings and undefined variables, although it can also catch
;; function calls with the wrong number of arguments, and some typos
;; such as (let (a (car b)) ...)

;; Before using, call `elint-initialize' to set up some argument
;; data. This takes a while. Then call elint-current-buffer or
;; elint-defun to lint a buffer or a defun.

;; The linter will try to "include" any require'd libraries to find
;; the variables and functions defined in them. There is a fair amount
;; of voodoo involved in this, but it seems to work in normal
;; situations.

;;; History:

;;   1.10 (22 September 1997)

;; * Added support for linting the Emacs LysKOM-client. This was
;;   coded by David Byers <byers@lysator.liu.se>.
;; * Now knows standard Emacs variables! This also caused a change
;;   from the one-file dist to a tar-archive with a Makefile...
;; * Slightly more XEmacs friendly

;;   1.9 (8 August 1997)

;; * Fixed to work with 19.28.

;;   1.8 (7 August 1997)

;; * initializer had (?) problems in the betas of Emacs 20.1 and
;;   XEmacs 20.3.
;; * defcustom is recognised as a variable definition
;; * Symbols beginning with : are treated as quoted.
;; * Tried to expand macros with incorrect number of arguments. Fixed.

;;   1.7 (29 July 1997)

;; * Linting environments are now cached, which also improves the
;;   handling of require'd files.

;;   1.6 (28 July 1997)

;; * Defvars buried in code wasn't added to the environment.
;; * Comments at the end of buffers was treated as a sexp.

;;; Tested on:

;; FSF Emacs 19.28, 19.34.1,
;; XEmacs 20.2

;;; To do:

;; * Adding type checking. (Stop that sniggering!)

;;; Code:

;; We need some data which are difficult to find in other ways
(require 'elint-data)

(defvar elint-log-buffer "*Elint*"
  "*The buffer to insert lint messages in.")

;;
;; ADT: top-form
;;

(defsubst elint-make-top-form (form pos)
  "Create a top form.
FORM is the form, and POS is the point where it starts in the buffer."
  (cons form pos))

(defsubst elint-top-form-form (top-form)
  "Extract the form from a TOP-FORM."
  (car top-form))

(defsubst elint-top-form-pos (top-form)
  "Extract the position from a TOP-FORM."
  (cdr top-form))

;;
;; ADT: env
;;

(defsubst elint-make-env ()
  "Create an empty environment."
  (list (list nil) nil nil))

(defsubst elint-env-add-env (env newenv)
  "Augment ENV with NEWENV.
None of them is modified, and the new env is returned."
  (list (append (car env) (car newenv))
	(append (car (cdr env)) (car (cdr newenv)))
	(append (car (cdr (cdr env))) (car (cdr (cdr newenv))))))

(defsubst elint-env-add-var (env var)
  "Augment ENV with the variable VAR.
The new environment is returned, the old is unmodified."
  (cons (cons (list var) (car env)) (cdr env)))

(defsubst elint-env-add-global-var (env var)
  "Augment ENV with the variable VAR.
ENV is modified so VAR is seen everywhere.
ENV is returned."
  (nconc (car env) (list (list var)))
  env)

(defsubst elint-env-find-var (env var)
  "Non-nil if ENV contains the variable VAR.
Actually, a list with VAR as a single element is returned."
  (assq var (car env)))

(defsubst elint-env-add-func (env func args)
  "Augment ENV with the function FUNC, which has the arguments ARGS.
The new environment is returned, the old is unmodified."
  (list (car env)
        (cons (list func args) (car (cdr env)))
        (car (cdr (cdr env)))))

(defun elint-env-add-kom-type (env type)
  (let ((name (symbol-name (car type)))
        (field-list (mapcar 'symbol-name (cdr type))))
    (setq env (elint-env-add-func env
                                  (intern (concat "lyskom-create-" name))
                                  (cdr type)))
    (setq env (elint-env-add-func env
                                  (intern (concat "lyskom-" name "-p"))
                                  '(obj)))
    (mapcar
     (function
      (lambda (field)
        (setq env (elint-env-add-func env
                                      (intern (concat name "->" field))
                                      '(obj)))
        (setq env (elint-env-add-func env
                                      (intern (concat "set-" name "->" field))
                                      '(obj arg)))))
     field-list))
  env)

(defsubst elint-env-find-func (env func)
  "Non-nil if ENV contains the function FUNC.
Actually, a list of (FUNC ARGS) is returned."
  (assq func (car (cdr env))))

(defsubst elint-env-add-macro (env macro def)
  "Augment ENV with the macro named MACRO.
DEF is the macro definition (a lambda expression or similar).
The new environment is returned, the old is unmodified."
  (list (car env)
	(car (cdr env))
	(cons (cons macro def) (car (cdr (cdr env))))))

(defsubst elint-env-macro-env (env)
  "Return the macro environment of ENV.
This environment can be passed to `macroexpand'."
  (car (cdr (cdr env))))

(defsubst elint-env-macrop (env macro)
  "Non-nil if ENV contains MACRO."
  (assq macro (elint-env-macro-env env)))

;;
;; User interface
;;

(defun elint-current-buffer ()
  "Lint the current buffer."
  (interactive)
  (elint-clear-log (format "Linting %s" (if (buffer-file-name)
					    (buffer-file-name)
					  (buffer-name))))
  (elint-display-log)
  (mapcar 'elint-top-form (elint-update-env))

  ;; Tell the user we're finished. This is terribly klugy: we set
  ;; elint-top-form-logged so elint-log-message doesn't print the
  ;; ** top form ** header...
  (let ((elint-top-form-logged t))
    (elint-log-message "\nLinting complete.\n")))
	
(defun elint-defun ()
  "Lint the function at point."
  (interactive)
  (save-excursion
    (if (not (beginning-of-defun))
	(error "Lint what?"))

    (let ((pos (point))
	  (def (read (current-buffer))))
      (elint-display-log)

      (elint-update-env)
      (elint-top-form (elint-make-top-form def pos)))))

;;
;; Top form functions
;;

(defvar elint-buffer-env nil
  "The environment of a elisp buffer.
Will be local in linted buffers.")

(defvar elint-buffer-forms nil
  "The top forms in a buffer.
Will be local in linted buffers.")

(defvar elint-last-env-time -1
  "The last time the buffers env was updated.
Is measured in buffer-modified-ticks and is local in linted buffers.")

(defun elint-update-env ()
  "Update the elint environment in the current buffer.
Don't do anything if the buffer hasn't been changed since this
function was called the last time.
Returns the forms."
  (if (and (local-variable-p 'elint-buffer-env (current-buffer))
	   (local-variable-p 'elint-buffer-forms (current-buffer))
	   (local-variable-p 'elint-last-env-time (current-buffer))
	   (= (buffer-modified-tick) elint-last-env-time))
      ;; Env is up to date
      elint-buffer-forms
    ;; Remake env
    (set (make-local-variable 'elint-buffer-forms) (elint-get-top-forms))
    (set (make-local-variable 'elint-buffer-env)
	 (elint-init-env elint-buffer-forms))
    (set (make-local-variable 'elint-last-env-time) (buffer-modified-tick))
    elint-buffer-forms))
       
(defun elint-get-top-forms ()
  "Collect all the top forms in the current buffer."
  (save-excursion
    (let ((tops nil))
      (goto-char (point-min))
      (while (elint-find-next-top-form)
	(let ((pos (point)))
	  (condition-case nil
	      (setq tops (cons
			  (elint-make-top-form (read (current-buffer)) pos)
			  tops))
	    (end-of-file
	     (goto-char pos)
	     (end-of-line)
	     (error "Missing ')' in top form: %s" (buffer-substring pos (point)))))
	  ))
      (nreverse tops))))

(defun elint-find-next-top-form ()
  "Find the next top form from point.
Returns nil if there are no more forms, T otherwise."
  (parse-partial-sexp (point) (point-max) nil t)
  (not (eobp)))

(defun elint-init-env (forms)
  "Initialise the environment from FORMS."
  (let ((env (elint-make-env))
	form)
    (while forms
      (setq form (elint-top-form-form (car forms))
	    forms (cdr forms))
      (cond
       ;; Add defined variable
       ((memq (car form) '(defvar defconst defcustom))
	(setq env (elint-env-add-var env (car (cdr form)))))
       ;; Add function
       ((memq (car form) '(defun defsubst
                            lyskom-provide-function
                            lyskom-provide-subst
                            def-kom-command))
        (setq env (elint-env-add-func env (car (cdr form))
                                      (car (cdr (cdr form))))))
       ;; Add derived mode
       ((eq (car form) 'define-derived-mode)
        (setq env (elint-env-add-func env (car (cdr form))
                                      nil)))
       ;; Add KOM type
       ((eq (car form) 'def-komtype)
        (setq env (elint-env-add-kom-type env (cdr form))))
       ;; Add macro, both as a macro and as a function
       ((eq (car form) 'defmacro)
	(setq env (elint-env-add-macro env (car (cdr form))
				       (cons 'lambda
					     (cdr (cdr form))))
	      env (elint-env-add-func env (car (cdr form))
				      (car (cdr (cdr form))))))

       ;; Import variable definitions
       ((eq (car form) 'require)
	(let ((name (eval (car (cdr form))))
	      (file (eval (car (cdr (cdr form))))))
	  (setq env (elint-add-required-env env name file))))
       ))
    env))

(defun elint-add-required-env (env name file)
  "Augment ENV with the variables definied by feature NAME in FILE."
  (condition-case nil
      (let* ((libname (if (stringp file)
			  file
			(symbol-name name)))

	     ;; First try to find .el files, then the raw name
	     (lib1 (locate-library (concat libname ".el") t))
	     (lib (if lib1 lib1 (locate-library libname t))))
	;; Clear the messages :-/
	(message nil)
	(if lib
	    (save-excursion
	      (set-buffer (find-file-noselect lib))
	      (elint-update-env)
	      (setq env (elint-env-add-env env elint-buffer-env)))
	  (error "dummy error...")))
    (error
     (ding)
     (message "Can't get variables from require'd library %s" name)))
  env)
					
(defun regexp-assoc (regexp alist)
  "Search for a key matching REGEXP in ALIST."
  (let ((res nil))
    (while (and alist (not res))
      (if (and (stringp (car (car alist)))
	       (string-match regexp (car (car alist))))
	  (setq res (car alist))
	(setq alist (cdr alist))))
    res))

(defvar elint-top-form nil
  "The currently linted top form, or nil.")

(defvar elint-top-form-logged nil
  "T if the currently linted top form has been mentioned in the log buffer.")

(defun elint-top-form (form)
  "Lint a top FORM."
  (let ((elint-top-form form)
	(elint-top-form-logged nil))
    (elint-form (elint-top-form-form form) elint-buffer-env)))

;;
;; General form linting functions
;;

(defconst elint-special-forms
  '((let . elint-check-let-form)
    (let* . elint-check-let-form)
    (setq . elint-check-setq-form)
    (quote . elint-check-quote-form)
    (cond . elint-check-cond-form)
    (lambda . elint-check-defun-form)
    (function . elint-check-function-form)
    (setq-default . elint-check-setq-form)
    (defun . elint-check-defun-form)
    (defsubst . elint-check-defun-form)
    (defmacro . elint-check-defun-form)
    (defvar . elint-check-defvar-form)
    (def-kom-var . elint-check-def-kom-var-form)
    (def-kom-command . elint-check-defun-form)
    (def-kom-emacs-command . elint-check-defun-form)
    (define-derived-mode . elint-check-define-derived-mode-form)
    (lyskom-provide-function . elint-check-defun-form)
    (lyskom-provide-subst . elint-check-defun-form)
    (lyskom-provide-macro . elint-check-defun-form)
    (defconst . elint-check-defvar-form)
    (defcustom . elint-check-defvar-form)
    (macro . elint-check-macro-form)
    (condition-case . elint-check-condition-case-form))
  "Functions to call when some special form should be linted.")
								
(defun elint-form (form env)
  "Lint FORM in the environment ENV.
The environment created by the form is returned."
  (cond
   ((consp form)
    (let ((func (cdr (assq (car form) elint-special-forms))))
      (if func
	  ;; Special form
	  (funcall func form env)

	(let* ((func (car form))
	       (args (elint-get-args func env))
	       (argsok t))
	  (cond
	   ((eq args 'undefined)
	    (setq argsok nil)
	    (elint-error "Call to undefined function: %s" form))
       
	   ((eq args 'unknown) nil)
	 
	   (t (setq argsok (elint-match-args form args))))

	  ;; Is this a macro?
	  (if (elint-env-macrop env func)
	      ;; Macro defined in buffer, expand it
	      (if argsok
		  (elint-form (macroexpand form (elint-env-macro-env env)) env)
		env)

	    (let ((fcode (if (symbolp func)
			     (if (fboundp func)
				 (indirect-function func)
			       nil)
			   func)))
	      (if (and (listp fcode) (eq (car fcode) 'macro))
		  ;; Macro defined outside buffer
		  (if argsok
		      (elint-form (macroexpand form) env)
		    env)
		;; Function, lint its parameters
		(elint-forms (cdr form) env))))
	  ))
      ))
   ((symbolp form)
    ;; :foo variables are quoted
    (if (and (/= (aref (symbol-name form) 0) ?:)
             (elint-unbound-variable form env)
             (not (elint-builtin-variable form)))
        (elint-warning "Reference to unbound symbol: %s" form))
    env)

   (t env)
   ))

(defun elint-forms (forms env)
  "Lint the FORMS, accumulating an environment, starting with ENV."
  ;; grumblegrumbletailrecursiongrumblegrumble
  (while forms
    (setq env (elint-form (car forms) env)
	  forms (cdr forms)))
  env)

(defun elint-unbound-variable (var env)
  "T if VAR is unbound in ENV."
  (not (or (eq var nil)
           (eq var t)
           (elint-builtin-variable var)
           (elint-env-find-var env var))))

(defun elint-builtin-variable (var)
  "T if VAR is a built in variable."
  (get var 'elint-builtin-var))

;;
;; Function argument checking
;;

(defun elint-match-args (arglist argpattern)
  "Match ARGLIST against ARGPATTERN."

  (let ((state 'all)
	(al (cdr arglist))
	(ap argpattern)
	(ok t))
    (while
	(cond
	 ((and (null al) (null ap)) nil)
	 ((eq (car ap) '&optional)
	  (setq state 'optional)
	  (setq ap (cdr ap))
	  t)
	 ((eq (car ap) '&rest)
	  nil)
	 ((or (and (eq state 'all) (or (null al) (null ap)))
	      (and (eq state 'optional) (and al (null ap))))
	  (elint-error "Wrong number of args: %s, %s" arglist argpattern)
	  (setq ok nil)
	  nil)
	 ((and (eq state 'optional) (null al))
	  nil)
	 (t (setq al (cdr al)
		  ap (cdr ap))
	    t)))
    ok))

(defun elint-get-args (func env)
  "Find the args of FUNC in ENV.
Returns `unknown' if we couldn't find arguments."
  (let ((f (elint-env-find-func env func)))
    (if f
	(car (cdr f))
      (if (symbolp func)
	  (if (fboundp func)
	      (let ((fcode (indirect-function func)))
		(if (subrp fcode)
		    (let ((args (get func 'elint-args)))
		      (if args args 'unknown))
		  (elint-find-args-in-code fcode)))
	    'undefined)
	(elint-find-args-in-code func)))))

(defun elint-find-args-in-code (code)
  "Extract the arguments from CODE.
CODE can be a lambda expression, a macro, or byte-compiled code."
  (cond
   ((byte-code-function-p code)
    (compiled-function-arglist code))
   ((and (listp code) (eq (car code) 'lambda))
    (car (cdr code)))
   ((and (listp code) (eq (car code) 'macro))
    (elint-find-args-in-code (cdr code)))
   (t 'unknown)))

;;
;; Functions to check some special forms
;;

(defun elint-check-cond-form (form env)
  "Lint a cond FORM in ENV."
  (setq form (cdr form))
  (while form
    (if (consp (car form))
	(elint-forms  (car form) env)
      (elint-error "cond clause should be a list: %s" (car form)))
    (setq form (cdr form)))
  env)

(defun elint-check-defun-form (form env)
  "Lint a defun/defmacro/lambda FORM in ENV."
  (setq form (if (eq (car form) 'lambda) (cdr form) (cdr (cdr form))))
  (mapcar (function (lambda (p)
		      (or (memq p '(&optional &rest))
			  (setq env (elint-env-add-var env p)))
		      ))
	  (car form))
  (elint-forms (cdr form) env))

(defun elint-check-define-derived-mode-form (form env)
  "Lint a `define-derived-mode' FORM in ENV."
  (cond ((not (and (symbolp (car (cdr form)))
                   (symbolp (car (cdr (cdr form))))
                   (stringp (car (cdr (cdr (cdr form)))))
                   (stringp (car (cdr (cdr (cdr (cdr form))))))))
         (elint-error "Malformed define-derived-mode: %s" form))
        ((and (not (elint-env-find-func env (car (cdr (cdr form)))))
              (not (fboundp (car (cdr (cdr form))))))
         (elint-warning "Reference to undefined base mode: %s" form))
        (t
         (setq env (elint-env-add-func env (car (cdr form)) nil))))
  env)

(defun elint-check-let-form (form env)
  "Lint the let/let* FORM in ENV."
  (let ((varlist (car (cdr form))))
    (if (not varlist)
	(progn
	  (elint-error "Missing varlist in let: %s" form)
	  env)

      ;; Check for (let (a (car b)) ...) type of error
      (if (and (= (length varlist) 2)
	       (symbolp (car varlist))
	       (listp (car (cdr varlist)))
	       (fboundp (car (car (cdr varlist)))))
	  (elint-warning "Suspect varlist: %s" form))

      ;; Add variables to environment, and check the init values
      (let ((newenv env))
	(mapcar (function (lambda (s)
			    (cond
			     ((symbolp s)
			      (setq newenv (elint-env-add-var newenv s)))
			     ((and (consp s) (<= (length s) 2))
			      (elint-form (car (cdr s))
					  (if (eq (car form) 'let)
					      env
					    newenv))
			      (setq newenv
				    (elint-env-add-var newenv (car s))))
			     (t (elint-error
				 "Malformed `let' declaration: %s" s))
			     )))
		varlist)

	;; Lint the body forms
	(elint-forms (cdr (cdr form)) newenv)
	))))

(defun elint-check-setq-form (form env)
  "Lint the setq FORM in ENV."
  (or (= (mod (length form) 2) 1)
      (elint-error "Missing value in setq: %s" form))

  (let ((newenv env)
	sym val)
    (setq form (cdr form))
    (while form
      (setq sym (car form)
	    val (car (cdr form))
	    form (cdr (cdr form)))
      (if (symbolp sym)
	  (if (elint-unbound-variable sym newenv)
	      (elint-warning "Setting previously unbound symbol: %s" sym))
	(elint-error "Setting non-symbol in setq: %s" sym))
      (elint-form val newenv)
      (if (symbolp sym)
	  (setq newenv (elint-env-add-var newenv sym))))
    newenv))
			
(defun elint-check-defvar-form (form env)
  "Lint the defvar/defconst FORM in ENV."
  (if (or (= (length form) 2)
	  (= (length form) 3)
	  (and (= (length form) 4) (stringp (nth 3 form))))
          (elint-env-add-global-var (elint-form (nth 2 form) env)
			     (car (cdr form)))
    (elint-error "Malformed variable declaration: %s" form)
    env))

(defun elint-check-def-kom-var-form (form env)
  "Lint the def-kom-var FORM in ENV."
  (cond ((< (length form) 3)
         (elint-error "Too short kom variable declaration: %s" form))
        ((= 3 (length form))
         (elint-env-add-global-var (elint-form (nth 2 form) env)
                                   (car (cdr form))))
        ((> (length form) 3)
         (elint-env-add-global-var (elint-form (nth 2 form) env)
                                   (car (cdr form)))
         (setq form (cdr (cdr (cdr form))))
         (while form
           (if (and (not (memq (car form) '(server server-hook protected
                                                   inherited local local-hook
                                                   minibuffer)))
                    (not (stringp (car form))))
               (elint-error "Invalid variable modifier: %S" (car form)))
           (setq form (cdr form)))))
  env)

(defun elint-check-function-form (form env)
  "Lint the function FORM in ENV."
  (let ((func (car (cdr-safe form))))
    (cond
     ((symbolp func)
      (or (elint-env-find-func env func)
	  (fboundp func)
	  (elint-warning "Reference to undefined function: %s" form))
      env)
     ((and (consp func) (memq (car func) '(lambda macro)))
      (elint-form func env))
     ((stringp func) env)
     (t (elint-error "Not a function object: %s" form)
	env)
     )))

(defun elint-check-quote-form (form env)
  "Lint the quote FORM in ENV."
  env)

(defun elint-check-macro-form (form env)
  "Check the macro FORM in ENV."
  (elint-check-function-form (list (car form) (cdr form)) env))

(defun elint-check-condition-case-form (form env)
  "Check the `condition-case' FORM in ENV."
  (let ((resenv env))
    (if (< (length form) 3)
	(elint-error "Malformed condition-case: %s" form)
      (or (symbolp (car (cdr form)))
	  (elint-warning "First parameter should be a symbol: %s" form))
      (setq resenv (elint-form (nth 2 form) env))

      (let ((newenv (elint-env-add-var env (car (cdr form))))
	    (errforms (nthcdr 3 form))
	    errlist)
	(while errforms
	  (setq errlist (car (car errforms)))
	  (mapcar (function (lambda (s)
			      (or (get s 'error-conditions)
				  (get s 'error-message)
				  (elint-warning
				   "Not an error symbol in error handler: %s" s))))
		  (cond
		   ((symbolp errlist) (list errlist))
		   ((listp errlist) errlist)
		   (t (elint-error "Bad error list in error handler: %s"
				   errlist)
		      nil))
		  )
	  (elint-forms (cdr (car errforms)) newenv)
	  (setq errforms (cdr errforms))
	  )))
    resenv))
	
;;
;; Message functions
;;

;; elint-error and elint-warning are identical, but they might change
;; to reflect different kinds of linting errors

(defun elint-error (string &rest args)
  "Report an linting error.
STRING and ARGS are thrown on `format' to get the message."
  (let ((errstr (apply 'format string args)))
    (elint-log-message errstr)
    ))
    
(defun elint-warning (string &rest args)
  "Report an linting warning.
STRING and ARGS are thrown on `format' to get the message."
  (let ((errstr (apply 'format string args)))
    (elint-log-message errstr)
    ))

(defun elint-log-message (errstr)
  "Insert ERRSTR last in the lint log buffer."
  (save-excursion
    (set-buffer (elint-get-log-buffer))
    (goto-char (point-max))
    (or (bolp) (newline))

    ;; Do we have to say where we are?
    (if elint-top-form-logged
	nil
      (insert
       (let* ((form (elint-top-form-form elint-top-form))
	      (top (car form)))
	 (cond
	  ((memq top '(defun defsubst))
	   (format "\n** function %s **\n" (car (cdr form))))
	  ((eq top 'defmacro)
	   (format "\n** macro %s **\n" (car (cdr form))))
	  ((memq top '(defvar defconst))
	   (format "\n** variable %s **\n" (car (cdr form))))
	  (t "\n** top level expression **\n"))))
      (setq elint-top-form-logged t))

    (insert errstr)
    (newline)))

(defun elint-clear-log (&optional header)
  "Clear the lint log buffer.
Insert HEADER followed by a blank line if non-nil."
  (save-excursion
    (set-buffer (elint-get-log-buffer))
    (erase-buffer)
    (if header
	(progn
	  (insert header)
	  (newline))
      )))

(defun elint-display-log ()
  "Display the lint log buffer."
  (let ((pop-up-windows t))
    (display-buffer (elint-get-log-buffer))
    (sit-for 0)))

(defun elint-get-log-buffer ()
  "Return a log buffer for elint."
  (let ((buf (get-buffer elint-log-buffer)))
    (if buf
	buf
      (let ((oldbuf (current-buffer)))
	(prog1
	    (set-buffer (get-buffer-create elint-log-buffer))
	  (setq truncate-lines t)
	  (set-buffer oldbuf)))
      )))
      
;;
;; Initializing code
;;

(defvar elint-unknown-builtin-args)

(defvar elint-initialised nil
  "T if elint has been initialised.")

;;;###autoload
(defun elint-initialize ()
  "Initialise elint."
  (interactive)
  (if elint-initialised
      (message "Elint has already been initialised.")
    (message "Elint: Finding arguments of builtin functions...")
    (mapcar (function (lambda (x)
			(or (not (symbolp (car x)))
			    (eq (cdr x) 'unknown)
			    (put (car x) 'elint-args (cdr x)))))
	    (elint-find-builtin-args))
    (mapcar (function (lambda (x)
			(put (car x) 'elint-args (cdr x))))
	    elint-unknown-builtin-args)
    (message "Elint: Finding builtin variables...")
    (mapcar (function (lambda (v)
			(put v 'elint-builtin-var t)))
	    elint-builtin-variables)
    (message "Elint is initialised. You may lint at will.")
    (setq elint-initialised t)))

(defun elint-find-builtin-functions ()
  "Returns a list of all built-in functions."
  (let ((subrs nil))
    (mapatoms (lambda (s) (if (and (fboundp s) (subrp (symbol-function s)))
			      (setq subrs (cons s subrs)))))
    subrs
    ))

(defun elint-find-builtin-args (&optional list)
  "Returns a list of the built-in functions and their arguments.

If LIST is nil, call `elint-find-builtins' to get a list of all built-in
functions, otherwise use LIST.

Each functions is represented by a cons cell:
\(function-symbol . args)
If no documentation could be found args will be `unknown'."

    (mapcar (function (lambda (f)
			(let ((doc (documentation f t)))
			  (if (and doc (string-match "\n\n\\((.*)\\)" doc))
			      (read (match-string 1 doc))
			    (cons f 'unknown))
			  )))
	    (if list list
	      (elint-find-builtin-functions))))


;;
;; Compitablity functions
;;

(if (not (fboundp 'match-string))
    (defun match-string (n &optional s)
      "Return string matched by last search.
N specifies the nth parenthesized expression in the last regexp.
N=0 means the entire text matched by the whole regexp or whole string.
S should be given if the last search was by `string-match' on string S.

Return value is nil if there is no Nth match."
      (and (match-beginning n)
	   (if s
	       (substring s (match-beginning n) (match-end n))
	     (buffer-substring (match-beginning n) (match-end n))))))

(if (not (fboundp 'local-variable-p))
    (defun local-variable-p (var &optional buffer)
      t))

(if (not (fboundp 'compiled-function-arglist))
    (defun compiled-function-arglist (object)
      "Return the arglist of the compiled code OBJECT."
      (aref object 0)))

(provide 'elint)
;;; elint ends here
