;;; semantic-format.el --- Routines for formatting tags

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-format.el,v 1.18 2004/06/24 00:50:32 zappo Exp $

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
;; Once a language file has been parsed into a TAG, it is often useful
;; then display that tag information in browsers, completion engines, or
;; help routines.  The functions and setup in this file provide ways
;; to reformat a tag into different standard output types.
;;
;; In addition, macros for setting up customizable variables that let
;; the user choose their default format type are also provided.
;;

;;; Code:
(eval-when-compile (require 'font-lock))
(require 'semantic-tag)
(require 'ezimage)

;;; Tag to text overload functions
;;
;; abbreviations, prototypes, and coloring support.
;;;###autoload
(defvar semantic-format-tag-functions
  '(semantic-format-tag-name
    semantic-format-tag-abbreviate
    semantic-format-tag-summarize
    semantic-format-tag-prototype
    semantic-format-tag-concise-prototype
    semantic-format-tag-uml-abbreviate
    semantic-format-tag-uml-prototype
    semantic-format-tag-uml-concise-prototype
    semantic-format-tag-prin1
    )
  "List of functions which convert a tag to text.
Each function must take the parameters TAG &optional PARENT COLOR.
TAG is the tag to convert.
PARENT is a parent tag or name which refers to the structure
or class which contains TAG.  PARENT is NOT a class which a TAG
would claim as a parent.
COLOR indicates that the generated text should be colored using
`font-lock'.")

;;;###autoload
(semantic-varalias-obsolete 'semantic-token->text-functions
                            'semantic-format-tag-functions)
;;;###autoload
(defvar semantic-format-tag-custom-list
  (append '(radio)
	  (mapcar (lambda (f) (list 'const f))
		  semantic-format-tag-functions)
	  '(function))
  "A List used by customizeable variables to choose a tag to text function.
Use this variable in the :type field of a customizable variable.")

(semantic-varalias-obsolete 'semantic-token->text-custom-list
                            'semantic-format-tag-custom-list)

(defcustom semantic-format-use-images-flag ezimage-use-images
  "Non-nil means semantic format functions use images.
Images can be used as icons instead of some types of text strings."
  :group 'semantic
  :type 'boolean)

(defvar semantic-function-argument-separator ","
  "Text used to separate arguments when creating text from tags.")
(make-variable-buffer-local 'semantic-function-argument-separator)

(defun semantic-test-all-format-tag-functions ()
  "Test all outputs from `semantic-format-tag-functions'.
Output is generated from the function under `point'."
  (interactive)
  (semantic-fetch-tags)
  (let* ((tag (semantic-current-tag))
	 (par (or (semantic-current-tag-parent)
		  (if (semantic-tag-function-parent tag)
		      (semantic-find-first-tag-by-name
		       (semantic-tag-function-parent tag)
		       (current-buffer)))
		  ))
	 (fns semantic-format-tag-functions))
    (with-output-to-temp-buffer "*format-tag*"
      (princ "Tag->format function tests:")
      (while fns
	(princ "\n")
	(princ (car fns))
	(princ ":\n ")
	(let ((s (funcall (car fns) tag par t)))
	  (save-excursion
	    (set-buffer "*format-tag*")
	    (goto-char (point-max))
	    (insert s)))
	(setq fns (cdr fns))))
      ))

(defvar semantic-format-face-alist
  `( (function . font-lock-function-name-face)
     (variable . font-lock-variable-name-face)
     (type . font-lock-type-face)
     ;; These are different between Emacsen.
     (include . ,(if (featurep 'xemacs)
		     'font-lock-preprocessor-face
		   'font-lock-constant-face))
     (package . ,(if (featurep 'xemacs)
		     'font-lock-preprocessor-face
		   'font-lock-constant-face))
     ;; Not a tag, but instead a feature of output
     (label . font-lock-string-face)
     (comment . font-lock-comment-face)
     (keyword . font-lock-keyword-face)
     (abstract . italic)
     (static . underline)
     )
  "Face used to colorize tags of different types.
Override the value locally if a language supports other tag types.
When adding new elements, try to use symbols also returned by the parser.
The form of an entry in this list is of the form:
 ( SYMBOL .  FACE )
where SYMBOL is a tag type symbol used with semantic.  FACE
is a symbol representing a face.
Faces used are generated in `font-lock' for consistency, and will not
be used unless font lock is a feature.")

(semantic-varalias-obsolete 'semantic-face-alist
                            'semantic-format-face-alist)



;;; Coloring Functions
;;
(defun semantic--format-colorize-text (text face-class)
  "Apply onto TEXT a color associated with FACE-CLASS.
FACE-CLASS is a tag type found in `semantic-face-alist'.  See this variable
for details on adding new types."
  (when (featurep 'font-lock)
    (let ((face (cdr-safe (assoc face-class semantic-format-face-alist)))
	  (newtext (concat text)))
      (put-text-property 0 (length text) 'face face newtext)
      newtext)
    ))

(make-obsolete 'semantic-colorize-text
               'semantic--format-colorize-text)

(defun semantic--format-colorize-merge-text (precoloredtext face-class)
  "Apply onto PRECOLOREDTEXT a color associated with FACE-CLASS.
FACE-CLASS is a tag type found in 'semantic-face-alist'.  See this
variable for details on adding new types."
  (let ((face (cdr-safe (assoc face-class semantic-format-face-alist)))
	(newtext (concat precoloredtext))
	)
    (if (featurep 'xemacs)
	(add-text-properties 0 (length newtext) (list 'face face) newtext)
      (alter-text-property 0 (length newtext) 'face
			   (lambda (current-face)
			     (let ((cf
				    (cond ((facep current-face)
					   (list current-face))
					  ((listp current-face)
					   current-face)
					  (t nil)))
				   (nf
				    (cond ((facep face)
					   (list face))
					  ((listp face)
					   face)
					  (t nil))))
			       (append cf nf)))
			   newtext))
    newtext))

;;; Function Arguments
;;
(defun semantic--format-tag-arguments (args formatter color)
  "Format the argument list ARGS with FORMATTER.
FORMATTER is a function used to format a tag.
COLOR specifies if color should be used."
  (let ((out nil))
    (while args
      (cond ((stringp (car args))
	     (let ((a (car args)))
	       (if color
		   (setq a (semantic--format-colorize-text a 'variable)))
	       (setq out (cons a out))
	       ))
	    ((semantic-tag-p (car args))
	     (setq out
		   (cons (funcall formatter (car args) nil color)
			 out))))
      (setq args (cdr args)))
    (mapconcat 'identity (nreverse out) semantic-function-argument-separator)
    ))

;;; Data Type
;;
;;;###autoload
(define-overload semantic-format-tag-type (tag color)
  "Convert the data type of TAG to a string usable in tag formatting.
It is presumed that TYPE is a string or semantic tag.")

(defun semantic-format-tag-type-default (tag color)
  "Convert the data type of TAG to a string usable in tag formatting.
Argument COLOR specifies to colorize the text."
  (let* ((type (semantic-tag-type tag))
	 (out (cond ((semantic-tag-p type)
		     (let ((typetype (semantic-tag-type type))
			   (name (semantic-tag-name type)))
		       (semantic--format-colorize-text
			(if typetype
			    (concat typetype " " name)
			  name)
			'type)))
		    ((and (listp type)
			  (stringp (car type)))
		     (car type))
		    ((stringp type)
		     type)
		    (t nil))))
    (if (and color out)
	(setq out (semantic--format-colorize-text out 'type))
      out)
    ))


;;; Abstract formatting functions
;;

;;;###autoload
(defun semantic-format-tag-prin1 (tag &optional parent color)
  "Convert TAG to a string that is the print name for TAG.
PARENT and COLOR are ignored."
  (format "%S" tag))

;;;###autoload
(define-overload semantic-format-tag-name (tag &optional parent color)
  "Return the name string describing TAG.
The name is the shortest possible representation.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-name-default (tag &optional parent color)
  "Return an abbreviated string describing TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let ((name (semantic-tag-name tag))
	(destructor
	 (if (eq (semantic-tag-class tag) 'function)
	     (semantic-tag-function-destructor-p tag))))
    (when destructor
      (setq name (concat "~" name)))
    (if color
	(setq name (semantic--format-colorize-text name (semantic-tag-class tag))))
    name))

;;;###autoload
(define-overload semantic-format-tag-abbreviate (tag &optional parent color)
  "Return an abbreviated string describing TAG.
The abbreviation is to be short, with possible symbols indicating
the type of tag, or other information.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-abbreviate-default (tag &optional parent color)
  "Return an abbreviated string describing TAG.
Optional argument PARENT is a parent tag in the tag hierarchy.
In this case PARENT refers to containment, not inheritance.
Optional argument COLOR means highlight the prototype with font-lock colors.
This is a simple C like default."
  ;; Do lots of complex stuff here.
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	(suffix "")
	(prefix "")
	str)
    (cond ((eq class 'function)
	   (setq suffix "()"))
	  ((eq class 'include)
	   (setq suffix "<>"))
	  ((eq class 'variable)
	   (setq suffix (if (semantic-tag-variable-default tag)
			    "=" "")))
	  ((eq class 'label)
	   (setq suffix ":"))
	  ((eq class 'code)
	   (setq prefix "{"
		 suffix "}"))
	  ((eq class 'type)
	   (setq suffix "{}"))
	  )
    (setq str (concat prefix name suffix))
    (if parent
	(setq str
	      (concat (semantic-format-tag-name parent color)
		      (car semantic-type-relation-separator-character)
		      str)))
    str))

;; Semantic 1.2.x had this misspelling.  Keep it for backwards compatibiity.
(semantic-alias-obsolete
 'semantic-summerize-nonterminal 'semantic-format-tag-summarize)

;;;###autoload
(define-overload semantic-format-tag-summarize (tag &optional parent color)
  "Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-summarize-default (tag &optional parent color)
  "Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((proto (semantic-format-tag-prototype tag nil color))
         (names (if parent
                    semantic-symbol->name-assoc-list-for-type-parts
                  semantic-symbol->name-assoc-list))
         (tsymb (semantic-tag-class tag))
         (label (capitalize (or (cdr-safe (assoc tsymb names))
                                (symbol-name tsymb)))))
    (if color
        (setq label (semantic--format-colorize-text label 'label)))
    (concat label ": " proto)))

;;; Prototype generation
;;
;;;###autoload
(define-overload semantic-format-tag-prototype (tag &optional parent color)
  "Return a prototype for TAG.
This function should be overloaded, though it need not be used.
This is because it can be used to create code by language independent
tools.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-prototype-default (tag &optional parent color)
  "Default method for returning a prototype for TAG.
This will work for C like languages.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((class (semantic-tag-class tag))
	 (name (semantic-format-tag-name tag parent color))
	 (type (if (member class '(function variable type))
		   (semantic-format-tag-type tag color)))
	 (args (if (member class '(function type))
                   (semantic--format-tag-arguments
                    (if (eq class 'function)
                        (semantic-tag-function-arguments tag)
                      (semantic-tag-type-members tag))
                    #'semantic-format-tag-prototype
                    color)))
	 (const (semantic-tag-get-attribute tag :constant-flag))
	 (mods (append
		(if const '("const") nil)
		(semantic-tag-get-attribute tag :typemodifiers)))
	 (array (if (eq class 'variable)
		    (let ((deref
			   (semantic-tag-get-attribute
 			    tag :dereference))
 			  (r ""))
 		      (while (and deref (/= deref 0))
 			(setq r (concat r "[]")
 			      deref (1- deref)))
 		      r)))
 	 )
    (if args
	(setq args
	      (concat " "
		      (if (eq class 'type) "{" "(")
		      args
		      (if (eq class 'type) "}" ")"))))
    (when mods
      (setq mods (concat (mapconcat 'identity mods " ") " ")))
    (concat (or mods "")
	    (if type (concat type " "))
	    name
	    (or args "")
	    (or array ""))))

;;;###autoload
(define-overload semantic-format-tag-concise-prototype (tag &optional parent color)
  "Return a concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-concise-prototype-default (tag &optional parent color)
  "Return a concise prototype for TAG.
This default function will make a cheap concise prototype using C like syntax.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let ((class (semantic-tag-class tag)))
    (cond
     ((eq class 'type)
      (concat (semantic-format-tag-name tag parent color) "{}"))
     ((eq class 'function)
      (concat (semantic-format-tag-name tag parent color)
	      " ("
	      (semantic--format-tag-arguments
	       (semantic-tag-function-arguments tag)
	       'semantic-format-tag-concise-prototype
	       color)
	      ")"))
     ((eq class 'variable)
      (let* ((deref (semantic-tag-get-attribute
                     tag :dereference))
             (array "")
             )
        (while (and deref (/= deref 0))
          (setq array (concat array "[]")
                deref (1- deref)))
        (concat (semantic-format-tag-name tag parent color)
                array)))
     (t
      (semantic-format-tag-abbreviate tag parent color)))))

;;; UML display styles
;;
(defcustom semantic-uml-colon-string " : "
  "*String used as a color separator between parts of a UML string.
In UML, a variable may appear as `varname : type'.
Change this variable to change the output separator."
  :group 'semantic
  :type 'string)

(defcustom semantic-uml-no-protection-string ""
  "*String used to describe when no protection is specified.
Used by `semantic-format-tag-uml-protection-to-string'."
  :group 'semantic
  :type 'string)

(defun semantic--format-uml-post-colorize (text tag parent)
  "Add color to TEXT created from TAG and PARENT.
Adds augmentation for `abstract' and `static' entries."
  (if (semantic-tag-abstract-p tag parent)
      (setq text (semantic--format-colorize-merge-text text 'abstract)))
  (if (semantic-tag-static-p tag parent)
      (setq text (semantic--format-colorize-merge-text text 'static)))
  text
  )

(defun semantic-uml-attribute-string (tag &optional parent)
  "Return a string for TAG, a child of PARENT representing a UML attribute.
UML attribute strings are things like {abstract} or {leaf}."
  (cond ((semantic-tag-abstract-p tag parent)
	 "{abstract}")
	((semantic-tag-leaf-p tag parent)
	 "{leaf}")
	))

(defvar semantic-format-tag-protection-image-alist
  '(("+" . ezimage-unlock)
    ("#" . ezimage-key)
    ("-" . ezimage-lock)
    )
  "Association of protection strings, and images to use.")

(defvar semantic-format-tag-protection-symbol-to-string-assoc-list
  '((public . "+")
    (protected . "#")
    (private . "-")
    )
  "Association list of the form (SYMBOL . \"STRING\") for protection symbols.
This associates a symbol, such as 'public with the st ring \"+\".")

(define-overload semantic-format-tag-uml-protection-to-string (protection-symbol color)
  "Convert PROTECTION-SYMBOL to a string for UML.
By default, uses `semantic-format-tag-protection-symbol-to-string-assoc-list'
to convert.
By defaul character returns are:
  public    -- +
  private   -- -
  protected -- #.
If PROTECTION-SYMBOL is unknown, then the return value is
`semantic-uml-no-protection-string'.
COLOR indicates if we should use an image on the text.")

(defun semantic-format-tag-uml-protection-to-string-default (protection-symbol color)
  "Convert PROTECTION-SYMBOL to a string for UML.
Uses `semantic-format-tag-protection-symbol-to-string-assoc-list' to convert.
If PROTECTION-SYMBOL is unknown, then the return value is
`semantic-uml-no-protection-string'.
COLOR indicates if we should use an image on the text."
  (let ((ezimage-use-images (and semantic-format-use-images-flag color))
	(key (assoc protection-symbol
		    semantic-format-tag-protection-symbol-to-string-assoc-list)))
    (ezimage-image-over-string
     (or (cdr-safe key)
	 semantic-uml-no-protection-string)
     semantic-format-tag-protection-image-alist)))

(defsubst semantic-format-tag-uml-protection (tag parent color)
  "Retrieve the protection string for TAG with PARENT.
Argument COLOR specifies that color should be added to the string as
needed."
  (semantic-format-tag-uml-protection-to-string
   (semantic-tag-protection tag parent)
   color))

(defun semantic--format-tag-uml-type (tag color)
  "Format the data type of TAG to a string usable for formatting.
COLOR indicates if it should be colorized."
  (let ((str (semantic-format-tag-type tag color)))
    (if str
	(concat semantic-uml-colon-string str))))

;;;###autoload
(define-overload semantic-format-tag-uml-abbreviate (tag &optional parent color)
  "Return a UML style abbreviation for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-uml-abbreviate-default (tag &optional parent color)
  "Return a UML style abbreviation for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((name (semantic-format-tag-name tag parent color))
	 (type  (semantic--format-tag-uml-type tag color))
	 (protstr (semantic-format-tag-uml-protection tag parent color))
	 (text nil))
    (setq text
	  (concat
	   protstr
	   (if type (concat name type)
	     name)))
    (if color
	(setq text (semantic--format-uml-post-colorize text tag parent)))
    text))

;;;###autoload
(define-overload semantic-format-tag-uml-prototype (tag &optional parent color)
  "Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-uml-prototype-default (tag &optional parent color)
  "Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((class (semantic-tag-class tag))
	 (cp (semantic-format-tag-name tag parent color))
	 (type (semantic--format-tag-uml-type tag color))
	 (prot (semantic-format-tag-uml-protection tag parent color))
	 (argtext
	  (cond ((eq class 'function)
		 (concat
		  " ("
		  (semantic--format-tag-arguments
		   (semantic-tag-function-arguments tag)
		   #'semantic-format-tag-uml-prototype
		   color)
		  ")"))
		((eq class 'type)
		 "{}")))
	 (text nil))
    (setq text (concat prot cp argtext type))
    (if color
	(setq text (semantic--format-uml-post-colorize text tag parent)))
    text
    ))

;;;###autoload
(define-overload semantic-format-tag-uml-concise-prototype (tag &optional parent color)
  "Return a UML style concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.")

(defun semantic-format-tag-uml-concise-prototype-default (tag &optional parent color)
  "Return a UML style concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((cp (semantic-format-tag-concise-prototype tag parent color))
	 (type (semantic--format-tag-uml-type tag color))
	 (prot (semantic-format-tag-uml-protection tag parent color))
	 (text nil)
	 )
    (setq text (concat prot cp type))
    (if color
	(setq text (semantic--format-uml-post-colorize text tag parent)))
    text
    ))


;;; Compatibility and aliases
;;
(semantic-alias-obsolete 'semantic-test-all-token->text-functions
			 'semantic-test-all-format-tag-functions)

(semantic-alias-obsolete 'semantic-prin1-nonterminal
			 'semantic-format-tag-prin1)

(semantic-alias-obsolete 'semantic-name-nonterminal
			 'semantic-format-tag-name)

(semantic-alias-obsolete 'semantic-abbreviate-nonterminal
			 'semantic-format-tag-abbreviate)

(semantic-alias-obsolete 'semantic-summarize-nonterminal
			 'semantic-format-tag-summarize)

(semantic-alias-obsolete 'semantic-prototype-nonterminal
			 'semantic-format-tag-prototype)

(semantic-alias-obsolete 'semantic-concise-prototype-nonterminal
			 'semantic-format-tag-concise-prototype)

(semantic-alias-obsolete 'semantic-uml-abbreviate-nonterminal
			 'semantic-format-tag-uml-abbreviate)

(semantic-alias-obsolete 'semantic-uml-prototype-nonterminal
			 'semantic-format-tag-uml-prototype)

(semantic-alias-obsolete 'semantic-uml-concise-prototype-nonterminal
			 'semantic-format-tag-uml-concise-prototype)


(provide 'semantic-format)

;;; semantic-format.el ends here
