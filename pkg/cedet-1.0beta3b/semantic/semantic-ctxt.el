;;; semantic-ctxt.el --- Context calculations for Semantic tools.

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ctxt.el,v 1.36 2004/07/15 20:34:50 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; Semantic, as a tool, provides a nice list of searchable tags.
;; That information can provide some very accurate answers if the current
;; context of a position is known.
;;
;; This library provides the hooks needed for a language to specify how
;; the current context is calculated.
;;
(require 'semantic)
(eval-when-compile (require 'semanticdb))

;;; Code:
;;
(defvar semantic-command-separation-character
 ";"
  "String which indicates the end of a command.
Used for identifying the end of a single command.")
(make-variable-buffer-local 'semantic-command-separation-character)

(defvar semantic-function-argument-separation-character
 ","
  "String which indicates the end of an argument.
Used for identifying arguments to functions.")
(make-variable-buffer-local 'semantic-function-argument-separation-character)

;;; Local variable parsing.
;;
(define-overload semantic-up-context (&optional point bounds-type)
  "Move point up one context from POINT.
Return non-nil if there are no more context levels.
Overloaded functions using `up-context' take no parameters.
BOUNDS-TYPE is a symbol representing a tag class to restrict
movement to.  If this is nil, 'function is used.
This will find the smallest tag of that class (function, variable,
type, etc) and make sure non-nil is returned if you cannot
go up past the bounds of that tag."
  (if point (goto-char point))
  (let ((nar (semantic-current-tag-of-class (or bounds-type 'function))))
    (if nar
	(semantic-with-buffer-narrowed-to-tag nar (:override-with-args ()))
      (when bounds-type
        (error "No context of type %s to advance in" bounds-type))
      (:override-with-args ()))))

(defun semantic-up-context-default ()
  "Move the point up and out one context level.
Works with languages that use parenthetical grouping."
  ;; By default, assume that the language uses some form of parenthetical
  ;; do dads for their context.
  (condition-case nil
      (progn
	(up-list -1)
	nil)
    (error t)))

(define-overload semantic-beginning-of-context (&optional point)
  "Move POINT to the beginning of the current context.
Return non-nil if there is no upper context.
The default behavior uses `semantic-up-context'.")

(defun semantic-beginning-of-context-default (&optional point)
  "Move POINT to the beginning of the current context via parenthisis.
Return non-nil if there is no upper context."
  (if point (goto-char point))
  (if (semantic-up-context)
      t
    (forward-char 1)
    nil))

(define-overload semantic-end-of-context (&optional point)
  "Move POINT to the end of the current context.
Return non-nil if there is no upper context.
Be default, this uses `semantic-up-context', and assumes parenthetical
block delimiters.")

(defun semantic-end-of-context-default (&optional point)
  "Move POINT to the end of the current context via parenthisis.
Return non-nil if there is no upper context."
  (if point (goto-char point))
  (if (semantic-up-context)
      t
    ;; Go over the list, and back over the end parenthisis.
    (forward-sexp 1)
    (forward-char -1)
    nil))

(defun semantic-narrow-to-context ()
  "Narrow the buffer to the extent of the current context."
  (let (b e)
    (save-excursion
      (if (semantic-beginning-of-context)
	  nil
	(setq b (point))))
    (save-excursion
      (if (semantic-end-of-context)
	  nil
	(setq e (point))))
    (if (and b e) (narrow-to-region b e))))

(defmacro semantic-with-buffer-narrowed-to-context (&rest body)
  "Execute BODY with the buffer narrowed to the current context."
  `(save-restriction
     (semantic-narrow-to-context)
     ,@body))
(put 'semantic-with-buffer-narrowed-to-context 'lisp-indent-function 0)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec semantic-with-buffer-narrowed-to-context
	      (def-body))))

(define-overload semantic-get-local-variables (&optional point)
  "Get the local variables based on POINT's context.
Local variables are returned in Semantic tag format.
This can be overriden with `get-local-variables'."
  ;; The working status is to let the parser work properly
  (working-status-forms
      (semantic-parser-working-message "Local")
      "done"
    (save-excursion
      (if point (goto-char point))
      (let* ((semantic-working-type nil)
             ;; Disable parsing messages
             (working-status-dynamic-type nil)
             (case-fold-search semantic-case-fold))
        (:override-with-args ())))))

(defun semantic-get-local-variables-default ()
  "Get local values from a specific context.
Uses the bovinator with the special top-symbol `bovine-inner-scope'
to collect tags, such as local variables or prototypes."
  ;; This assumes a bovine parser.  Make sure we don't do
  ;; anything in that case.
  (when (and semantic--parse-table (not (eq semantic--parse-table t)))
    (let ((vars nil)
	  ;; We want nothing to do with funny syntaxing while doing this.
	  (semantic-unmatched-syntax-hook nil))
      (while (not (semantic-up-context (point) 'function))
	(save-excursion
	  (forward-char 1)
	  (setq vars
		;; Note to self: This is specific to bovine parsers.
		;; We need a better way to configure this generically.
		(append (semantic-parse-region
			 (point)
			 (save-excursion (semantic-end-of-context) (point))
			 'bovine-inner-scope
			 nil
			 t)
			vars))))
      vars)))

(define-overload semantic-get-local-arguments (&optional point)
  "Get arguments (variables) from the current context at POINT.
Parameters are available if the point is in a function or method.
Return a list of tags unlinked from the originating buffer.
Arguments are obtained by overriding `get-local-arguments', or by the
default function `semantic-get-local-arguments-default'.  This, must
return a list of tags, or a list of strings that will be converted to
tags."
  (save-excursion
    (if point (goto-char point))
    (let* ((case-fold-search semantic-case-fold)
           (args (:override-with-args ()))
           arg tags)
      ;; Convert unsafe arguments to the right thing.
      (while args
        (setq arg  (car args)
              args (cdr args)
              tags (cons (cond
                          ((semantic-tag-p arg)
                           ;; Return a copy of tag without overlay.
                           ;; The overlay is preserved.
                           (semantic-tag-copy arg))
                          ((stringp arg)
                           (semantic-tag-new-variable arg nil nil))
                          (t
                           (error "Unknown parameter element %S" arg)))
                         tags)))
      (nreverse tags))))

(defun semantic-get-local-arguments-default ()
  "Get arguments (variables) from the current context.
Parameters are available if the point is in a function or method."
  (let ((tag (semantic-current-tag)))
    (if (and tag (semantic-tag-of-class-p tag 'function))
	(semantic-tag-function-arguments tag))))

(define-overload semantic-get-all-local-variables (&optional point)
  "Get all local variables for this context, and parent contexts.
Local variables are returned in Semantic tag format.
Be default, this gets local variables, and local arguments.
Optional argument POINT is the location to start getting the variables from.")

(defun semantic-get-all-local-variables-default (&optional point)
  "Get all local variables for this context.
Optional argument POINT is the location to start getting the variables from.
That is a cons (LOCAL-ARGUMENTS . LOCAL-VARIABLES) where:

- LOCAL-ARGUMENTS is collected by `semantic-get-local-arguments'.
- LOCAL-VARIABLES is collected by `semantic-get-local-variables'."
  (save-excursion
    (if point (goto-char point))
    (let ((case-fold-search semantic-case-fold))
      (cons (semantic-get-local-arguments)
	    (semantic-get-local-variables)))))

;;; Local context parsing
;;
;; Context parsing assumes a series of language independent commonalities.
;; These terms are used to describe those contexts:
;;
;; command      - One command in the language.
;; symbol       - The symbol the cursor is on.
;;                This would include a series of type/field when applicable.
;; assignment   - The variable currently being assigned to
;; function     - The function call the cursor is on/in
;; argument     - The index to the argument the cursor is on.
;;
;;
(define-overload semantic-end-of-command ()
  "Move to the end of the current command.
Be default, uses `semantic-command-separation-character'.")

(defun semantic-end-of-command-default ()
  "Move to the beginning of the current command.
Depends on `semantic-command-separation-character' to find the
beginning and end of a command."
  (semantic-with-buffer-narrowed-to-context
    (let ((case-fold-search semantic-case-fold))
      (with-syntax-table semantic-lex-syntax-table

	(if (re-search-forward (regexp-quote semantic-command-separation-character)
			       nil t)
	    (forward-char -1)
	  ;; If there wasn't a command after this, we are the last
	  ;; command, and we are incomplete.
	  (goto-char (point-max)))))))

(define-overload semantic-beginning-of-command ()
  "Move to the beginning of the current command.
Be default, uses `semantic-command-separation-character'.")

(defun semantic-beginning-of-command-default ()
  "Move to the beginning of the current command.
Depends on `semantic-command-separation-character' to find the
beginning and end of a command."
  (semantic-with-buffer-narrowed-to-context
    (with-syntax-table semantic-lex-syntax-table
      (let ((case-fold-search semantic-case-fold))
	(skip-chars-backward semantic-command-separation-character)
	(if (re-search-backward (regexp-quote semantic-command-separation-character)
				nil t)
	    (goto-char (match-end 0))
	  ;; If there wasn't a command after this, we are the last
	  ;; command, and we are incomplete.
	  (goto-char (point-min)))
	(skip-chars-forward " \t\n")
	))))


(defsubst semantic-point-at-beginning-of-command ()
  "Return the point at the beginning of the current command."
  (save-excursion (semantic-beginning-of-command) (point)))

(defsubst semantic-point-at-end-of-command ()
  "Return the point at the beginning of the current command."
  (save-excursion (semantic-end-of-command) (point)))

(defsubst semantic-narrow-to-command ()
  "Narrow the current buffer to the current command."
  (narrow-to-region (semantic-point-at-beginning-of-command)
		    (semantic-point-at-end-of-command)))

(defmacro semantic-with-buffer-narrowed-to-command (&rest body)
  "Execute BODY with the buffer narrowed to the current command."
  `(save-restriction
     (semantic-narrow-to-command)
     ,@body))
(put 'semantic-with-buffer-narrowed-to-command 'lisp-indent-function 0)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec semantic-with-buffer-narrowed-to-command
	      (def-body))))


(define-overload semantic-ctxt-current-symbol (&optional point)
  "Return the current symbol the cursor is on at POINT in a list.
This will include a list of type/field names when applicable.")

(defun semantic-ctxt-current-symbol-default (&optional point)
  "Return the current symbol the cursor is on at POINT in a list.
This will include a list of type/field names when applicable.
Depends on `semantic-type-relation-separator-character'."
  (if point (goto-char point))
  (let* ((fieldsep1 (mapconcat (lambda (a) (regexp-quote a))
			       semantic-type-relation-separator-character
			       "\\|"))
	 (fieldsep (concat "\\(" fieldsep1 "\\)\\(\\w\\|\\s_\\)"))
	 (case-fold-search semantic-case-fold)
	 (symlist nil)
	 end)
    (with-syntax-table semantic-lex-syntax-table    
      (save-excursion
	(if (looking-at "\\w\\|\\s_")
	    (forward-sexp 1)
	  ;; Not on a sym, are we at a separator char with no field
	  ;; specified yet?
	  (when (or (looking-at fieldsep1)
		    (save-excursion
		      (and (condition-case nil
			       (progn (forward-sexp -1)
				      (forward-sexp 1)
				      t)
			     (error nil))
			   (looking-at fieldsep1))))
	    (setq symlist (list ""))
	    (forward-sexp -1)
	    (forward-sexp 1)))
	(setq end (point))
	(condition-case nil
	    (while (save-excursion
		     (forward-char -1)
		     (looking-at "\\w\\|\\s_"))
	      ;; We have a symbol.. Do symbol things
	      (forward-sexp -1)
	      (setq symlist (cons (buffer-substring-no-properties (point) end)
				  symlist))
	      ;; Skip the next syntactic expression backwards, then go forwards.
	      (let ((cp (point)))
		(forward-sexp -1)
		(forward-sexp 1)
		;; If we end up at the same place we started, we are at the
		;; beginning of a buffer, or narrowed to a command and
		;; have to stop.
		(if (<= cp (point)) (error nil)))
	      (if (looking-at fieldsep)
		  (setq end (point))
		(error nil))
	      )
	  (error nil)))
      symlist)))

(define-overload semantic-ctxt-current-assignment (&optional point)
  "Return the current assignment near the cursor at POINT.
Return a list as per `semantic-ctxt-current-symbol'.
Return nil if there is nothing relevant.")

(defun semantic-ctxt-current-assignment-default (&optional point)
  "Return the current assignment near the cursor at POINT.
By default, assume that \"=\" indicates an assignment."
  (if point (goto-char point))
  (let ((case-fold-search semantic-case-fold))
    (with-syntax-table semantic-lex-syntax-table    
      (condition-case nil
	  (semantic-with-buffer-narrowed-to-command
	    (save-excursion
	      (skip-chars-forward " \t=")
	      (condition-case nil (forward-char 1) (error nil))
	      (re-search-backward "[^=]=\\([^=]\\|$\\)")
	      ;; We are at an equals sign.  Go backwards a sexp, and
	      ;; we'll have the variable.  Otherwise we threw an error
	      (forward-sexp -1)
	      (semantic-ctxt-current-symbol)))
	(error nil)))))

(define-overload semantic-ctxt-current-function (&optional point)
  "Return the current function call the cursor is in at POINT.
The function returned is the one accepting the arguments that
the cursor is currently in.  It will not return function symbol if the
cursor is on the text representing that function.")

(defun semantic-ctxt-current-function-default (&optional point)
  "Return the current function call the cursor is in at POINT.
The call will be identifed for C like langauges with the form
 NAME ( args ... )"
  (if point (goto-char point))
  (let ((case-fold-search semantic-case-fold))
    (with-syntax-table semantic-lex-syntax-table
      (save-excursion
	(semantic-up-context)
	(when (looking-at "(")
	  (semantic-ctxt-current-symbol))))
    ))

(define-overload semantic-ctxt-current-argument (&optional point)
  "Return the index of the argument position the cursor is on at POINT.")

(defun semantic-ctxt-current-argument-default (&optional point)
  "Return the index of the argument the cursor is on.
Depends on `semantic-function-argument-separation-character'."
  (if point (goto-char point))
  (let ((case-fold-search semantic-case-fold))
    (with-syntax-table semantic-lex-syntax-table
      (when (semantic-ctxt-current-function)
	(save-excursion
	  ;; Only get the current arg index if we are in function args.
	  (let ((p (point))
		(idx 1))
	    (semantic-up-context)
	    (while (re-search-forward
		    (regexp-quote semantic-function-argument-separation-character)
		    p t)
	      (setq idx (1+ idx)))
	    idx))))))

(defun semantic-ctxt-current-thing ()
  "Calculate a thing identified by the current cursor position.
Calls previously defined `semantic-ctxt-current-...' calls until something
gets a match.  See `semantic-ctxt-current-symbol',
`semantic-ctxt-current-function', and `semantic-ctxt-current-assignment'
for details on the return value."
  (or (semantic-ctxt-current-symbol)
      (semantic-ctxt-current-function)
      (semantic-ctxt-current-assignment)))

(define-overload semantic-ctxt-current-class-list (&optional point)
  "Return a list of tag classes that are allowed at POINT.
If POINT is nil, the current buffer location is used.
For example, in Emacs Lisp, the symbol after a ( is most likely
a function.  In a makefile, symbols after a : are rules, and symbols
after a $( are variables.")

(defun semantic-ctxt-current-class-list-default (&optional point)
  "Return a list of tag classes that are allowed at POINT.
Assume a functional typed language.  Uses very simple rules."
  (save-excursion
    (if point (goto-char point))

    (let ((tag (semantic-current-tag)))
      (when tag
	(cond ((semantic-tag-of-class-p tag 'function)
	       '(function variable))
	      ((or (semantic-tag-of-class-p tag 'type)
		   (semantic-tag-of-class-p tag 'variable))
	       '(type))
	      (t nil))))))


;;; Scoped Types
;;
;; Scoped types are types that the current code would have access to.
;; The come from the global namespace or from special commands such as "using"
(define-overload semantic-ctxt-scoped-types (&optional point)
  "Return a list of type names currently in scope at POINT.
The return value can be a mixed list of either strings (names of
types that are in scope) or actual tags (type declared locally
that may or may not have a name.)")

(defun semantic-ctxt-scoped-types-default (&optional point)
  "Return a list of scoped types by name for the current context at POINT.
This is very different for various languages, and does nothing unless
overriden."
  (if point (goto-char point))
  (let ((case-fold-search semantic-case-fold))
    ;; We need to look at TYPES within the bounds of locally parse arguments.
    ;; C needs to find using statements and the like too.  Bleh.
    nil
    ))

(provide 'semantic-ctxt)

;;; semantic-ctxt.el ends here
