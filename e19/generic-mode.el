;;; generic-mode.el --- A generic mode which provides basic comment and
;;;   font-lock support

;; Author:  Peter Breton
;; Created: Fri Sep 27 1996
;; Version: $Header: e:/users/peter/lib/emacs/RCS/generic-mode.el,v 1.4 1996/10/19 12:16:59 peter Exp $
;; Keywords: generic, comment, font-lock
;; Time-stamp: <96/10/19 08:14:42 peter>

;;; Commentary:
;;
;; INTRODUCTION:
;;
;; Generic-mode is a meta-mode which can be used to define small modes
;; which provide basic comment and font-lock support. These modes are intended
;; for the many configuration files and such which are too small for a
;; "real" mode, but still have a regular syntax, comment characters and
;; the like.
;;
;; Each derived mode can define the following:
;;
;; * List of comment-characters. The entries in this list should be
;;   either a character, a one-character string or a cons pair.
;;   A character or one-character string will be set to
;;   comment-start, and be added to the local syntax table.
;;   NOT YET IMPLEMENTED:
;;   The elements of the cons pair (if non-nil) will be added to the
;;   local syntax table and set to comment-start and comment-end, respectively.
;;   LIMITATIONS:  Emacs does not support comment strings of more than
;;   two characters in length.
;;
;; * List of keywords to font-lock. Each keyword should be a string.
;; 
;; * Additional expressions to font-lock. This should be a list of
;;   expressions, each of which should be of the same form
;;   as those in 'font-lock-defaults-alist'.
;;   
;; * List of regular expressions to be placed in auto-mode-alist.
;;
;; * List of functions to call to do some additional setup
;;
;; This should pretty much cover basic functionality; if you need much
;; more than this, or you find yourself writing extensive customizations,
;; perhaps you should be writing a major mode instead!
;;
;; INSTALLATION:
;;
;; Place the following in your .emacs file:
;;
;;   (require 'generic-mode)
;;
;; If you want to use some pre-defined generic modes, add:
;;
;;   (require 'generic-extras)
;;
;; Loading these generic modes will cause some new entries to be placed in
;; your auto-mode-alist. See 'generic-extras.el' for details.
;;
;; LOCAL VARIABLES:
;;
;; To put a file into generic mode using local variables, use a line
;; like this in a Local Variables block:
;;
;;   mode: default-generic
;;
;; Do NOT use "mode: generic"! 
;;   
;; DEFINING NEW MODES:
;;
;; Use the 'define-generic-mode' function to define new modes.
;; For example:
;;
;;   (require 'generic-mode)
;;   (define-generic-mode 'foo
;;                        (list ?% )
;;                        (list "keyword")
;;                        nil
;;			  (list "\.FOO")
;;			  (list 'foo-setup-function))
;;
;; defines a new generic-mode 'foo', which has '%' as a comment character,
;; and "keyword" as a keyword. When files which end in '.FOO' are loaded,
;; Emacs will go into foo-generic-mode and call foo-setup-function.
;; 
;; ALTERING EXISTING MODES:
;;
;; To alter an existing generic-mode, use the convenience functions:
;;
;; (alter-generic-mode-comments  MODE COMMENT-LIST   HOW-TO-ALTER)
;; (alter-generic-mode-keywords  MODE KEYWORD-LIST   HOW-TO-ALTER)
;; (alter-generic-mode-font-lock MODE FONT-LOCK-LIST HOW-TO-ALTER)
;; (alter-generic-mode-auto-mode MODE AUTO-MODE-LIST HOW-TO-ALTER)
;; (alter-generic-mode-functions MODE FUNCTION-LIST  HOW-TO-ALTER)
;;
;; HOW-TO-ALTER should be one of the following symbols: 'append, 'prepend,
;; or 'overwrite. If it is omitted, 'append is assumed.
;;
;; GOTCHAS:
;;
;; Be careful that your font-lock definitions are correct. Getting them
;; wrong can cause emacs to continually attempt to fontify. 
;; 
;; TODO:
;;
;;  * comment functionality (cons pair) 

;; Credit for suggestions, patches and bug-fixes:
;;   ACorreir@pervasive-sw.com (Alfred Correira)

;;; Change log:
;; $Log: generic-mode.el,v $
;; Revision 1.4  1996/10/19 12:16:59  peter
;; Small bug fixes: fontlock -> font-lock
;; New entries are added to the end of auto-mode-alist
;; Generic-font-lock-defaults are set to nil, not (list nil)
;; Comment-regexp in generic-mode-find-file-hook changed to allow optional
;; blank lines
;;
;; Revision 1.3  1996/10/17 08:24:25  peter
;; Added generic-mode-find-file-hook and associated variables
;;
;; Revision 1.2  1996/10/17 01:00:45  peter
;; Moved from a data-centered approach (generic-mode-alist) to
;; a function-based one (define-generic-mode)
;;
;; Revision 1.1  1996/10/10 11:37:36  peter
;; Initial revision
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar generic-font-lock-defaults nil
  "Global defaults for font-lock in generic mode")

(defvar generic-mode-type 'default
  "The type of generic mode, which is the car of one of the items in 
generic-mode-alist. This variable is buffer-local")

(make-variable-buffer-local 'generic-mode-type)

(defvar generic-mode-syntax-table nil
  "Syntax table for use in generic-mode")

(defvar generic-mode-alist nil
  "An association list for generic-mode. 
Each entry in the list looks like this: 

 NAME COMMENT-LIST KEYWORD-LIST FONT-LOCK-LIST AUTO-MODE-LIST FUNCTION-LIST.

Do not add entries to this list directly; use 'define-generic-mode' 
instead (which see). 

To alter an already existing generic-mode, use 
one of the 'alter-generic-mode-' convenience functions (which see)"
)

(defvar generic-use-find-file-hook t
  "*If non-nil, add a hook to find-file-hooks to enter default-generic-mode
automatically if the first few lines of a file in fundamental mode
start with a hash comment character")

(defvar generic-lines-to-scan 3
  "*How many lines to look at when deciding whether or not to enter
generic-mode automatically. See 'generic-mode-find-file-hook'. 
This variable should be set to a small positive number")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst generic-read-type ()
  (completing-read
   "Generic Type: "
   (mapcar
    '(lambda (elt) (list (symbol-name (car elt))))
    generic-mode-alist) nil t))

(defsubst generic-function-name (name)
 (intern (concat (symbol-name name) "-generic-mode")))

;; Basic sanity checks. It does *not* check whether the elements of the lists
;; are of the correct type.
(defsubst generic-mode-sanity-check (name comment-list   keyword-list   
					  font-lock-list auto-mode-list  
					  function-list  &optional description)
  (if (not (symbolp name))
      (error "%s is not a symbol" (princ name)))

  (mapcar '(lambda (elt) 
	     (if (not (listp elt))
		 (error "%s is not a list" (princ elt))))
	  (list comment-list   keyword-list font-lock-list 
		auto-mode-list function-list))

  (if (not (or (null description) (stringp description)))
      (error "Description must be a string or nil"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;### autoload
(defun define-generic-mode (name comment-list    keyword-list   font-lock-list 
				 auto-mode-list  function-list  
				 &optional description)
  "Create a new generic mode with NAME.
NAME should be a symbol; it is used to create a new function 
NAME-generic-mode. If DESCRIPTION is provided, it is used as the docstring 
for the new function.

COMMENT-LIST is a list, whose entries are either a single character or
one-character string or a cons pair. If the entry is a character or a 
one-character string, it is added to the mode's syntax table with
comment-start syntax. If the entry is a cons pair, the elements of the
pair are considered to be comment-start and comment-end respectively. 
Note that Emacs has limitations regarding comment characters.

KEYWORD-LIST is a list of keywords to highlight with 'font-lock-keyword-face'.
Each keyword should be a string.

FONT-LOCK-LIST is a list of additional expressions to highlight. Each entry
in the list should have the same form as an entry in 'font-lock-defaults-alist'

AUTO-MODE-LIST is a list of regular expressions to add to auto-mode-alist.
These regexps are added to auto-mode-alist as soon as 'define-generic-mode' 
is called; any old regexps with the same name are removed. To modify the 
auto-mode-alist expressions, use 'alter-generic-mode-auto-mode' (which see).

FUNCTION-LIST is a list of functions to call to do some additional setup.

See the file 'generic-extras.el for some examples of 'define-generic-mode'."

  (generic-mode-sanity-check name 
			     comment-list    keyword-list   font-lock-list 
			     auto-mode-list  function-list  description)

    ;; Remove any old entry
    (setq generic-mode-alist
	  (delq (assq name generic-mode-alist) 
		generic-mode-alist))

    ;; Add a new entry
    (setq generic-mode-alist
	  (append
	   (list
	    (list
	     name comment-list keyword-list font-lock-list 
	     auto-mode-list    function-list
	     ))
	   generic-mode-alist))

    ;; Add it to auto-mode-alist
    (generic-add-to-auto-mode name auto-mode-list t)

    ;; Define a function for it
    (generic-create-generic-function name description)
    )

(defun generic-add-to-auto-mode (mode auto-mode-list &optional remove-old)
  "Add the entries for mode to 'auto-mode-alist'. 
If remove-old is non-nil, removes old entries first"

  (if (not (listp auto-mode-list))
      (error "%s is not a list" (princ auto-mode-list)))

  (let ((new-mode (generic-function-name mode)))
    (if remove-old
	(let ((auto-mode-entry))
	  (while (setq auto-mode-entry (rassq new-mode auto-mode-alist))
	    (setq auto-mode-alist
		  (delq auto-mode-entry
			auto-mode-alist)))))

    (mapcar '(lambda (entry) 
	       (generic-add-auto-mode-entry new-mode entry))
	    auto-mode-list)))

(defun generic-add-auto-mode-entry (name entry)
  "Add a new entry to the end of auto-mode-alist"
    (setq auto-mode-alist
	  (append
	   auto-mode-alist
	   (list (cons entry name)))))
   
(defun generic-create-generic-function (name &optional description)
  "Create a generic mode function NAME-generic-mode"
  (let ((symname (symbol-name name)))
    (fset (generic-function-name name)
	  (list 'lambda nil
		(or description 
		    (concat "Generic mode for type " symname))
		(list 'interactive)
		(list 'generic-mode-with-type (list 'quote name))))
    )
  )

(defun generic-mode-with-type (&optional mode-type)
  "Go into generic-mode with the given type"
  (let* ((type (or mode-type generic-mode-type))
	 (generic-mode-list  (assoc type generic-mode-alist))
	 (generic-comment-list)
	 (generic-keywords)
	 (generic-font-lock-expressions)
	 (generic-mode-function-list)
	)

    (if (not generic-mode-list)
	(error "Can't find generic-mode information for type %s"
	       (princ generic-mode-type)))

    ;; Put this after the point where we read generic-mode-type!
    (kill-all-local-variables)
    (make-local-variable 'generic-mode-type)
    (setq generic-mode-type type)

    (setq generic-comment-list          (nth 1 generic-mode-list)
	  generic-keywords		(nth 2 generic-mode-list)
	  generic-font-lock-expressions (nth 3 generic-mode-list)
	  generic-mode-function-list	(nth 5 generic-mode-list)
	  )

    (setq major-mode          'generic-mode
	  mode-name           "generic"
	  )

    (generic-mode-set-comments     generic-comment-list)

    ;; Font-lock functionality
    ;; Font-lock-defaults are always set even if there are no keywords
    ;; or font-lock expressions, so comments can be highlighted.
    (make-local-variable	    'generic-font-lock-defaults)
    (setq generic-font-lock-defaults nil)
    (generic-mode-set-font-lock      generic-keywords
				     generic-font-lock-expressions)
    (make-local-variable	    'font-lock-defaults)
    (setq font-lock-defaults (list 'generic-font-lock-defaults nil))

    ;; Call a list of functions
    (if generic-mode-function-list
	(mapcar 'funcall generic-mode-function-list))
    )
  )

;;;###autoload
(defun generic-mode (type)
  "A mode to do basic comment and font-lock functionality 
for files which are too small to warrant their own mode, but have
comment characters, keywords, and the like.

To define a generic-mode, use the function 'define-generic-mode'.
To alter an existing generic-mode, use the 'alter-generic-mode-'
convenience functions. 
Some generic modes are defined in generic-extras.el" 
  (interactive
   (list (generic-read-type)))
  (generic-mode-with-type (intern type)))

(defun generic-mode-set-comments (comment-list)
  "Set up comment functionality for generic mode"
  (if (null comment-list)
      nil
    (let ((generic-mode-syntax-table (make-syntax-table)))
      (make-local-variable	     'comment-start)
      (make-local-variable	     'comment-start-skip)
      (make-local-variable	     'comment-end)
      (mapcar
       '(lambda (elt)
	  (if (char-or-string-p elt)
	      (generic-mode-set-comment-char 
	       (if (stringp elt)
		   (string-to-char elt)
		 elt)))
	  (if (consp elt)
	      (generic-mode-set-comment-pair elt)))
       comment-list)
      (set-syntax-table    generic-mode-syntax-table))))

(defun generic-mode-set-comment-char (comment-char)
  "Set the given character as a comment character for generic mode"
  (if (not comment-char)
      nil
    (setq 
     comment-end         ""
     comment-start       (char-to-string comment-char)
     comment-start-skip  (concat comment-start "+ *")
     )
      
    (modify-syntax-entry comment-char "<"
			 generic-mode-syntax-table)
    (modify-syntax-entry ?\n ">"
			 generic-mode-syntax-table)))

;; Not implemented
(defun generic-mode-set-comment-pair (comment-pair)
  "Set the given comment pair as a comment start and end for generic mode"
  (let ((generic-comment-start (car comment-pair))
	(generic-comment-end   (cdr comment-pair))
	)
    (setq 
     comment-end         generic-comment-end
     comment-start       generic-comment-start
     comment-start-skip  (concat generic-comment-start "+ *")
     )
    )) 

(defun generic-mode-set-font-lock (keywords font-lock-expressions)
  "Sets up font-lock functionality for generic mode."
  (let ((generic-font-lock-expressions))
    ;; Keywords
    (if keywords
	(setq
	 generic-font-lock-expressions
	 (append
	  (list
	   (list
	    (concat "\\(\\<"
		    (mapconcat 'identity
			       keywords
			       "\\>\\|\\<")
		    "\\>\\)") 1 'font-lock-keyword-face))
	  generic-font-lock-expressions)))
    ;; Other font-lock expressions
    (if font-lock-expressions
	(setq generic-font-lock-expressions
	      (append
	       font-lock-expressions
	       generic-font-lock-expressions)))
    (if (not (or font-lock-expressions keywords))
	nil
      (setq generic-font-lock-defaults generic-font-lock-expressions))
    ))

(defun alter-generic-mode (mode alter-list &optional how-to-alter)
  "Alter the specified generic mode.
How-to-alter, if specified, should be one of the following symbols:
'append, 'prepend, 'overwrite. The default is 'append"
  (let ((generic-mode-list  (assoc mode generic-mode-alist))
	(item-number 0)
	(current-elt) 
	(current-list)
	(alter-elt)
	(alter-method (or how-to-alter 'append))
	)
    (if (not generic-mode-list)
	(error "Can't find generic-mode information for type %s"
	       (princ mode)))
    ;; Ignore the name
    (setq generic-mode-list (cdr generic-mode-list))
    (while (< item-number (length alter-list)) 
      (setq current-list (nthcdr item-number generic-mode-list)
	    current-elt  (nth    item-number generic-mode-list)
	    alter-elt    (nth    item-number alter-list))
      (cond 
       ;; Ignore items with value t
       ((eq alter-elt 't)
	     nil)
       ((eq alter-method 'overwrite)
	(setcar current-list alter-elt))
       ((eq alter-method 'prepend)
	(setcar current-list (append alter-elt current-elt)))
       ((eq alter-method 'append)
	(setcar current-list (append current-elt alter-elt)))
       (t
	(error "Optional argument %s not understood" (princ alter-method))))
      (setq item-number (1+ item-number))
      )
    )
  )

;; Convenience functions
(defun alter-generic-mode-comments (mode comment-list &optional how-to-alter)
  "Alter comments in the specified generic mode.
How-to-alter, if specified, should be one of the following symbols:
'append, 'prepend, 'overwrite. The default is 'append"
  (alter-generic-mode mode (list comment-list t t t t) how-to-alter))

(defun alter-generic-mode-keywords (mode keyword-list &optional how-to-alter)
  "Alter keywords in the specified generic mode.
How-to-alter, if specified, should be one of the following symbols:
'append, 'prepend, 'overwrite. The default is 'append"
  (alter-generic-mode mode (list t keyword-list t t t) how-to-alter))

(defun alter-generic-mode-font-lock (mode font-lock-list &optional how-to-alter)
  "Alter font-lock expressions in the specified generic mode.
How-to-alter, if specified, should be one of the following symbols:
'append, 'prepend, 'overwrite. The default is 'append"
  (alter-generic-mode mode (list t t font-lock-list t t) how-to-alter))

(defun alter-generic-mode-functions (mode function-list &optional how-to-alter)
  "Alter functions in the specified generic mode.
How-to-alter, if specified, should be one of the following symbols:
'append, 'prepend, 'overwrite. The default is 'append"
  (alter-generic-mode mode (list t t t t function-list) how-to-alter))

;; This one is different because it takes effect immediately
;; Appending or Prepending to auto-mode-alist is ignored,
;; since the effect is the same either way
(defun alter-generic-mode-auto-mode 
  (mode auto-mode-list &optional how-to-alter)
  "Alter auto-mode-alist regular expressions in the specified generic mode.
How-to-alter, if specified, should be one of the following symbols:
'append, 'prepend, 'overwrite. The default is 'append"
  (alter-generic-mode mode (list t t t auto-mode-list  t) how-to-alter)
  (let ((alter-method (or how-to-alter 'append)))
    (cond ((eq alter-method 'overwrite)
	   (generic-add-to-auto-mode mode auto-mode-list t))
	  ((memq alter-method (list 'append 'prepend))
	   (generic-add-to-auto-mode mode auto-mode-list nil))
	  (t
	   (error "Optional argument %s not understood" (princ alter-method))))
    ))

;; Support for [KEYWORD] constructs found in INF, INI and Samba files
(defun generic-bracket-support ()
  (setq imenu-generic-expression 
	'((nil "^\\[\\(.*\\)\\]" 1))))

;; This generic mode is always defined
(define-generic-mode 'default (list ?#)  nil nil nil nil)

;; A more general solution would allow us to enter generic-mode for
;; *any* comment character, but would require us to synthesize a new
;; generic-mode on the fly. I think this gives us most of what we
;; want.
(defun generic-mode-find-file-hook ()
  "Enter default-generic-mode automatically if the first few lines
of a file in fundamental-mode start with a hash comment character.
This hook will be installed if the variable 'generic-use-find-file-hook'
is non-nil. The number of lines to look at is set by the variable
'generic-lines-to-scan'"
  (if (not (eq major-mode 'fundamental-mode))
      nil
    (if (or (> 1  generic-lines-to-scan)
	    (< 50 generic-lines-to-scan))
	(error "Variable 'generic-lines-to-scan' should be set to a small"
	       " positive number"))
    (let ((comment-regexp "")
	  (count 0)
	  )
      (while (< count generic-lines-to-scan)
	(setq comment-regexp (concat comment-regexp "#.*\n\\(.*\n\\)?"))
	(setq count (1+ count)))
      (save-excursion
	(goto-char (point-min))
	(if (looking-at comment-regexp)
	    (generic-mode-with-type 'default))))))

(if generic-use-find-file-hook
    (add-hook 'find-file-hooks 'generic-mode-find-file-hook))

(provide 'generic-mode)

;;; generic-mode.el ends here

