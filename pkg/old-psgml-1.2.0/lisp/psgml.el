;;; psgml.el --- SGML-editing mode with parsing support
;; $Id: psgml.el,v 2.48 1999/10/10 13:48:38 lenst Exp $

;; Copyright (C) 1993-1999 Lennart Staflin
;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;;	James Clark <jjc@clark.com>
;; Maintainer: Lennart Staflin <lenst@lysator.liu.se>
;; Keywords: languages

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Major mode for editing the SGML document-markup language.

;; Send bugs to lenst@lysator.liu.se

;; WHAT IT CAN DO

;; - Identify structural errors (but it is not a validator)
;; - Menus for inserting tags with only the contextually valid tags
;; - Edit attribute values in separate window with information about types
;;   and defaults
;; - Hide attributes
;; - Fold elements
;; - Indent according to element nesting depth
;; - Show context
;; - Structure editing: move and kill by element
;; - Find next data context

;; LIMITATIONS

;; - only accepts the referece concrete syntax, though it does allow
;;   unlimited lengths on names


;;; Code:

(defconst psgml-version "1.2.0"
  "Version of psgml package.")

(defconst psgml-maintainer-address "lenst@lysator.liu.se")

(require 'cl)
(require 'easymenu)

(defvar psgml-debug nil)

(defmacro psgml-debug (&rest x)
  (list 'if 'psgml-debug (cons 'psgml-log-message x)))


;;;; Variables

(defvar psgml-mode-abbrev-table nil
  "Abbrev table in use in psgml-mode.")
(define-abbrev-table 'psgml-mode-abbrev-table ())

(defvar psgml-running-lucid (string-match "Lucid" emacs-version))

(defvar psgml-pxml-p nil
  "Is this an XML document?")
(make-variable-buffer-local 'psgml-pxml-p)

;;; User settable options:

(defvar psgml-insert-missing-element-comment t
  "*If true, and psgml-auto-insert-required-elements also true,
`psgml-insert-element' will insert a comment if there is an element required
but there is more than one to choose from." )

(defvar psgml-insert-end-tag-on-new-line nil
  "*If true, `psgml-insert-element' will put the end-tag on a new line
after the start-tag. Useful on slow terminals if you find the end-tag after
the cursor irritating." )

(defvar psgml-doctype nil
  "*If set, this should be the name of a file that contains the doctype
declaration to use.
Setting this variable automatically makes it local to the current buffer.")
(put 'psgml-doctype 'psgml-type 'string)
(make-variable-buffer-local 'psgml-doctype)

(defvar psgml-system-identifiers-are-preferred nil
  "*If nil, PSGML will look up external entities by searching the catalogs
in `psgml-local-catalogs' and `psgml-catalog-files' and only if the
entity is not found in the catalogs will a given system identifer be
used. If the variable is non-nil and a system identifer is given, the
system identifier will be used for the entity. If no system identifier
is given the catalogs will searched.")

(defvar psgml-range-indicator-max-length 9
  "*Maximum number of characters used from the first and last entry
of a submenu to indicate the range of that menu.")

(defvar psgml-default-doctype-name nil
  "*Document type name to use if no document type declaration is present.")
(put 'psgml-default-doctype-name 'psgml-type 'string-or-nil)

(defvar psgml-markup-faces '((start-tag	. bold)
			    (end-tag	. bold)
			    (comment	. italic)
			    (pi	. bold)
			    (psgml	. bold)
			    (doctype	. bold)
			    (entity	. bold-italic)
			    (shortref   . bold))
  "*List of markup to face mappings.
Element are of the form (MARKUP-TYPE . FACE).
Possible values for MARKUP-TYPE is:
comment	- comment declaration
doctype	- doctype declaration
end-tag
ignored	- ignored marked section
ms-end	- marked section start, if not ignored
ms-start- marked section end, if not ignored
pi	- processing instruction
psgml	- SGML declaration
start-tag
entity  - general entity reference
shortref- short reference")

(defvar psgml-buggy-subst-char-in-region
  (or (not (boundp 'emacs-minor-version))
      (not (natnump emacs-minor-version))
      (and (eq emacs-major-version 19)
	   (< emacs-minor-version 23)))
  "*If non-nil, work around a bug in subst-char-in-region.
The bug sets the buffer modified.  If this is set, folding commands
will be slower.")

(defvar psgml-set-face nil
  "*If non-nil, psgml will set the face of parsed markup.")
(put 'psgml-set-face 'psgml-desc "Set face of parsed markup")

(defvar psgml-live-element-indicator nil
  "*If non-nil, indicate current element in mode line.")

(defvar psgml-auto-activate-dtd nil
  "*If non-nil, loading a sgml-file will automatically try to activate its DTD.
Activation means either to parse the document type declaration or to
load a previously saved parsed DTD.  The name of the activated DTD
will be shown in the mode line.")
(put 'psgml-auto-activate-dtd 'psgml-desc "Auto Activate DTD")

(defvar psgml-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[psgml-validate] is run.")

(defvar psgml-parent-document nil
  "* Used when the current file is part of a bigger document.

The variable describes how the current file's content fit into the element
hierarchy. The variable should have the form

  (PARENT-FILE CONTEXT-ELEMENT* TOP-ELEMENT (HAS-SEEN-ELEMENT*)?)

PARENT-FILE	is a string, the name of the file contatining the
		document entity.
CONTEXT-ELEMENT is a string, that is the name of an element type.
		It can occur 0 or more times and is used to set up
		exceptions and short reference map. Good candidates
		for these elements are the elements open when the
		entity pointing to the current file is used.
TOP-ELEMENT	is a string that is the name of the element type
		of the top level element in the current file. The file
		should contain one instance of this element, unless
		the last \(lisp) element of psgml-parent-document is a
		list. If it is a list, the top level of the file
		should follow the content model of top-element.
HAS-SEEN-ELEMENT is a string that is the name of an element type. This
		element is satisfied in the content model of top-element.

Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'psgml-parent-document)
(put 'psgml-parent-document 'psgml-type 'list)

(defvar psgml-tag-region-if-active nil
  "*If non-nil, the Tags menu will tag a region if the region is
considered active by emacs.  If nil, region must be active and
transient-mark-mode must be on for the region to be tagged.")

(defvar psgml-normalize-trims t
  "*If non-nil, psgml-normalize will trim off white space from end of element
when adding end tag.")

(defvar psgml-omittag t
  "*Set to non-nil, if you use OMITTAG YES.

Setting this variable automatically makes it local to the current buffer.")

(make-variable-buffer-local 'psgml-omittag)
(put 'psgml-omittag 'psgml-desc "OMITTAG")

(defvar psgml-shorttag t
  "*Set to non-nil, if you use SHORTTAG YES.

Setting this variable automatically makes it local to the current buffer.")

(make-variable-buffer-local 'psgml-shorttag)
(put 'psgml-shorttag 'psgml-desc "SHORTTAG")

(defvar psgml-namecase-general t
  "*Set to non-nil, if you use NAMECASE GENERAL YES.

Setting this variable automatically makes it local to the current buffer.")

(make-variable-buffer-local 'psgml-namecase-general)
(put 'psgml-namecase-general 'psgml-desc "NAMECASE GENERAL")



;;[lenst/1998-03-09 19:51:55]
(defconst psgml-namecase-entity nil)

(defvar psgml-general-insert-case 'lower
  "*The case that will be used for general names in inserted markup.
This can be the symbol `lower' or `upper'. Only effective if
psgml-namecase-general is true.")
(put 'psgml-general-insert-case 'psgml-type '(lower upper))

(defvar psgml-entity-insert-case nil)


(defvar psgml-minimize-attributes nil
  "*Determines minimization of attributes inserted by edit-attributes.
Actually two things are done
1. If non-nil, omit attribute name, if attribute value is from a token group.
2. If 'max, omit attributes with default value.

Setting this variable automatically makes it local to the current buffer.")

(make-variable-buffer-local 'psgml-minimize-attributes)
(put 'psgml-minimize-attributes 'psgml-type
     '(("No" . nil) ("Yes" . t) ("Max" . max)))

(defvar psgml-always-quote-attributes t
  "*If non-nil, quote all attribute values inserted after finishing edit attributes.
Setting this variable automatically makes it local to the current buffer.")

(make-variable-buffer-local 'psgml-always-quote-attributes)

(defvar psgml-auto-insert-required-elements t
  "*If non-nil, automatically insert required elements in the content
of an inserted element.")

(defvar psgml-balanced-tag-edit t
  "*If non-nil, always insert start-end tag pairs.")

(defvar psgml-omittag-transparent (not psgml-balanced-tag-edit)
  "*If non-nil, will show legal tags inside elements with omitable start tags
and legal tags beyond omitable end tags.")

(defvar psgml-leave-point-after-insert nil
  "*If non-nil, the point will remain after inserted tag(s).
If nil, the point will be placed before the inserted tag(s).")

(defvar psgml-warn-about-undefined-elements t
  "*If non-nil, print a warning when a tag for an undefined element is found.")

(defvar psgml-warn-about-undefined-entities t
  "*If non-nil, print a warning when an undefined entity is found.")

(defvar psgml-ignore-undefined-elements nil
  "*If non-nil, recover from an undefined element by ignoring the tag.
If nil, recover from an undefined element by assuming it can occur any
where and has content model ANY.")

(defvar psgml-recompile-out-of-date-cdtd 'ask
  "*If non-nil, out of date compiled DTDs will be automatically recompiled.
If the value is `ask', PSGML will ask before recompiling. A `nil'
value will cause PSGML to silently load an out of date compiled DTD.
A DTD that referes to undefined external entities is always out of
date, thus in such case it can be useful to set this variable to
`nil'.")
(put 'psgml-recompile-out-of-date-cdtd 'psgml-type '(("No" . nil)
						   ("Yes" . t)
						   ("Ask" . ask)))

(defvar psgml-trace-entity-lookup nil
  "*If non-nil, log messages about catalog files used to look for
external entities.")

(defvar psgml-indent-step 2
  "*How much to increment indent for every element level.
If nil, no indentation.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'psgml-indent-step)
(put 'psgml-indent-step 'psgml-type '(("None" . nil) 0 1 2 3 4 5 6 7 8))

(defvar psgml-indent-data nil
  "*If non-nil, indent in data/mixed context also.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'psgml-indent-data)


(defun psgml-parse-colon-path (cd-path)
  "Explode a colon-separated list of paths into a string list."
  (if (null cd-path)
      nil
    (let ((cd-sep ":")
	  cd-list (cd-start 0) cd-colon)
      (if (boundp 'path-separator)
	  (setq cd-sep path-separator))
      (setq cd-path (concat cd-path cd-sep))
      (while (setq cd-colon (string-match cd-sep cd-path cd-start))
	(setq cd-list
	      (nconc cd-list
		     (list (if (= cd-start cd-colon)
			       nil
			     (substitute-in-file-name
			      (substring cd-path cd-start cd-colon))))))
	(setq cd-start (+ cd-colon 1)))
      cd-list)))

(defvar psgml-system-path (psgml-parse-colon-path
			  (or (getenv "SGML_SEARCH_PATH")
			      "."))
  "*List of directories used to look for system identifiers.")
(put 'psgml-system-path 'psgml-type 'file-list)

(defvar psgml-public-map (or (psgml-parse-colon-path (getenv "SGML_PATH"))
			    '("%S" "/usr/local/lib/sgml/%o/%c/%d" ))
  "*Mapping from public identifiers to file names.
This is a list of possible file names.  To find the file for a public
identifier the elements of the list are used one at the time from the
beginning.  If the element is a string a file name is constructed from
the string by substitution of the whole public identifier for %P,
owner for %O, public text class for %C, and public text description
for %D.  The text class will be converted to lower case and the owner
and description will be transliterated according to the variable
psgml-public-transliterations.  If the file exists it will be the file
used for the public identifier.  An element can also be a dotted pair
(regexp . filename), the filename is a string treated as above, but
only if the regular expression, regexp, matches the public
identifier.")
(put 'psgml-public-map 'psgml-type 'list)

(defvar psgml-local-catalogs nil
"*A list of SGML entity catalogs to be searched first when parsing the buffer.
This is used in addtion to `psgml-catalog-files',  and `psgml-public-map'.
This variable is automatically local to the buffer.")
(make-variable-buffer-local 'psgml-local-catalogs)
(put 'psgml-local-catalogs 'psgml-type 'file-list)

(defvar psgml-catalog-files (or (delete nil
				       (psgml-parse-colon-path
					(getenv "SGML_CATALOG_FILES")))
			       '("catalog" "/usr/local/lib/sgml/catalog"))
  "*List of catalog entry files.
The files are in the format defined in the SGML Open Draft Technical
Resolution on Entity Management.")
(put 'psgml-catalog-files 'psgml-type 'file-list)

(defvar psgml-ecat-files '("ECAT" "~/sgml/ECAT" "/usr/local/lib/sgml/ECAT")
  "*List of catalog files for PSGML.")
(put 'psgml-ecat-files 'psgml-type 'file-list)

(defvar psgml-local-ecat-files nil
  "*List of local catalog files for PSGML.
Automatically becomes buffer local if set.")

(make-variable-buffer-local 'psgml-local-ecat-files)
(put 'psgml-local-ecat-files 'psgml-type 'file-list)

(defvar psgml-public-transliterations '((? . ?_) (?/ . ?%))
  "*Transliteration for characters that should be avoided in file names.
This is a list of dotted pairs (FROM . TO); where FROM is the the
character to be translated to TO.  This is used when parts of a public
identifier are used to construct a file name.")

(defvar psgml-default-dtd-file nil
  "*This is the default file name for saved DTD.
This is set by psgml-mode from the buffer file name.
Can be changed in the Local variables section of the file.")
(put 'psgml-default-dtd-file 'psgml-type 'string)
(put 'psgml-default-dtd-file 'psgml-desc "Default (saved) DTD File")

(defvar psgml-exposed-tags '()
  "*The list of tag names that remain visible, despite \\[psgml-hide-tags].
Each name is a lowercase string, and start-tags and end-tags must be
listed individually.

`psgml-exposed-tags' is local to each buffer in which it has been set;
use `setq-default' to set it to a value that is shared among buffers.")
(make-variable-buffer-local 'psgml-exposed-tags)
(put 'psgml-exposed-tags 'psgml-type 'list)


(defvar psgml-custom-markup nil
  "*Menu entries to be added to the Markup menu.
The value should be a list of lists of two strings.  The first is a
string is the menu line and the second string is the text inserted
when the menu item is chosen.  The second string can contain a \\r
where the cursor should be left.  Also if a selection is made
according the same rules as for the Tags menu, the selection is
replaced with the second string and \\r is replaced with the
selection.

Example:

  ((\"Version1\" \"<![%Version1[\\r]]>\")
   (\"New page\"  \"<?NewPage>\"))
")

(defvar psgml-custom-dtd nil
  "Menu entries to be added to the DTD menu.
The value should be a list of entrys to be added to the DTD menu.
Every entry should be a list. The first element of the entry is a string
used as the menu entry.  The second element is a string containing a
doctype declaration (this can be nil if no doctype).  The rest of the
list should be a list of variables and values.  For backward
compatibility a singel string instead of a variable is assigned to
psgml-default-dtd-file.  All variables are made buffer local and are also
added to the buffers local variables list.

Example:
   ((\"HTML\" nil
     psgml-default-dtd-file \"~/sgml/html.ced\"
     psgml-omittag nil psgml-shorttag nil)
    (\"HTML+\" \"<!doctype htmlplus system 'htmlplus.dtd'>\"
     \"~/sgml/htmlplus.ced\"
     psgml-omittag t psgml-shorttag nil)
    (\"DOCBOOK\" \"<!doctype docbook system 'docbook.dtd'>\"
     \"~/sgml/docbook.ced\"
     psgml-omittag nil psgml-shorttag t)))
")


;;; Faces used in edit attribute buffer:
(put 'psgml-default 'face 'underline)	; Face for #DEFAULT
(put 'psgml-fixed 'face 'underline)	; Face of #FIXED "..."


;;; sgmls is a free SGML parser available from
;;; ftp.uu.net:pub/text-processing/sgml
;;; Its error messages can be parsed by next-error.
;;; The -s option suppresses output.

(defvar psgml-validate-command   "nsgmls -s %s %s"
  "*The shell command to validate an SGML document.

This is a `format' control string that by default should contain two
`%s' conversion specifications: the first will be replaced by the
value of `psgml-declaration' \(or the empty string, if nil\); the
second will be replaced by the current buffer's file name \(or the
empty string, if nil\).

If `psgml-validate-files' is non-nil, the format string should contain
one `%s' conversion specification for each element of its result.

If psgml-validate-command is a list, then every element should be a
string.  The strings will be tried in order and %-sequences in the
string will be replaced according to the list below, if the string contains
%-sequences with no replacement value the next string will be tried.

%b means the visited file of the current buffer
%s means the SGML declaration specified in the psgml-declaration variable
%d means the file containing the DOCTYPE declaration, if not in the buffer
")
(make-variable-buffer-local 'psgml-validate-command)

(defvar psgml-validate-files nil
  "If non-nil, a function of no arguments that returns a list of file names.
These file names will serve as the arguments to the `psgml-validate-command'
format control string instead of the defaults.")

(defvar psgml-validate-error-regexps
  '((".*:\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[EX]: " 1 2 3)
    ("\\(error\\|warning\\) at \\([^,]+\\), line \\([0-9]+\\)" 2 3)
    ("\n[a-zA-Z]?:?[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
\\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4))
  "Alist of regexps to recognize error messages from `psgml-validate'.
See `compilation-error-regexp-alist'.")

(defvar psgml-declaration nil
  "*If non-nil, this is the name of the SGML declaration file.")
(put 'psgml-declaration 'psgml-type 'file-or-nil)

(defvar psgml-pxml-declaration nil
  "*If non-nil, this is the name of the SGML declaration for XML files.")
(put 'psgml-pxml-declaration 'psgml-type 'file-or-nil)

(defvar psgml-mode-hook nil
  "A hook or list of hooks to be run when entering psgml-mode")

(defconst psgml-file-options
  '(
    psgml-omittag
    psgml-shorttag
    psgml-namecase-general
    psgml-general-insert-case
    psgml-minimize-attributes
    psgml-always-quote-attributes
    psgml-indent-step
    psgml-indent-data
    psgml-doctype
    psgml-parent-document
    psgml-default-dtd-file
    psgml-exposed-tags
    psgml-local-catalogs
    psgml-local-ecat-files
    )
  "Options for the current file, can be saved or set from menu."
  )

(defconst psgml-user-options
  '(
    psgml-set-face
    psgml-live-element-indicator
    psgml-auto-activate-dtd
    psgml-offer-save
    psgml-tag-region-if-active
    psgml-normalize-trims
    psgml-auto-insert-required-elements
    psgml-balanced-tag-edit
    psgml-omittag-transparent
    psgml-leave-point-after-insert
    psgml-insert-missing-element-comment
    psgml-insert-end-tag-on-new-line
    psgml-warn-about-undefined-elements
    psgml-warn-about-undefined-entities
    psgml-ignore-undefined-elements
    psgml-recompile-out-of-date-cdtd
    psgml-default-doctype-name
    psgml-declaration
    psgml-validate-command
    psgml-markup-faces
    psgml-system-identifiers-are-preferred
    psgml-trace-entity-lookup
    psgml-public-map
    psgml-catalog-files
    psgml-ecat-files
    psgml-general-insert-case
    )
  "User options that can be saved or set from menu."
  )

;;; Internal variables

(defvar psgml-validate-command-history nil
  "The minibuffer history list for `psgml-validate''s COMMAND argument.")

(defvar psgml-mode-map nil "Keymap for SGML mode")

(defvar psgml-active-dtd-indicator nil
  "Displayed in the mode line")


;;;; User options handling

(defun psgml-variable-description (var)
  (or (get var 'psgml-desc)
      (let ((desc (symbol-name var)))
	(if (string= "psgml-" (substring desc 0 5))
	    (setq desc (substring desc 5)))
	(loop for c across-ref desc
	      do (if (eq c ?-) (setf c ? )))
	(capitalize desc))))

(defun psgml-variable-type (var)
  (or (get var 'psgml-type)
      (if (memq (symbol-value var) '(t nil))
	  'toggle)))

(defun psgml-set-local-variable (var val)
  "Set the value of variable VAR to VAL in buffer and local variables list."
  (set (make-local-variable var) val)
  (save-excursion
    (let ((prefix "")
	  (suffix "")
	  (case-fold-search t))
      (goto-char (max (point-min) (- (point-max) 3000)))
      (cond ((search-forward "Local Variables:" nil t)
	     (setq suffix (buffer-substring (point)
					    (save-excursion (end-of-line 1)
							    (point))))
	     (setq prefix
		   (buffer-substring (save-excursion (beginning-of-line 1)
						     (point))
				     (match-beginning 0))))
	    (t
	     (goto-char (point-max))
	     (unless (bolp)
	       (insert ?\n))
	     (insert
	      "<!-- Keep this comment at the end of the file\n"
	      "Local variables:\n"
	      (if psgml-pxml-p
		  "mode: pxml\n"
		"mode: psgml\n")
	      "End:\n"
	      "-->\n")
	     (forward-line -3)))
      (let* ((endpos (save-excursion
		       (search-forward (format "\n%send:" prefix))))
	     (varpos (search-forward (format "\n%s%s:" prefix var) endpos t)))
	(cond (varpos
	       (delete-region (point)
			      (save-excursion (end-of-line 1)
					      (point)))
	       (insert (format "%S" val) suffix))
	      (t
	       (goto-char endpos)
	       (beginning-of-line 1)
	       (insert prefix (format "%s:%S" var val) suffix ?\n)))))))

(defun psgml-valid-option (var)
  (let ((type (psgml-variable-type var))
	(val (symbol-value var)))
    (cond ((eq 'string type)
	   (stringp val))
	  ((eq 'list-or-string type)
	   (or (stringp val)
	       (consp val)))
	  (t
	   t))))

(defun psgml-save-options ()
  "Save user options for psgml-mode that have buffer local values."
  (interactive)
  (loop for var in psgml-file-options do
	(when (psgml-valid-option var)
	  (psgml-set-local-variable var (symbol-value var)))))


;;;; Run hook with args

(unless (fboundp 'run-hook-with-args)
  (defun run-hook-with-args (hook &rest args)
    "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a list
of functions, those functions are called, in order,
with the given arguments ARGS.
It is best not to depend on the value return by `run-hook-with-args',
as that may change."
    (and (boundp hook)
	 (symbol-value hook)
	 (let ((value (symbol-value hook)))
	   (if (and (listp value) (not (eq (car value) 'lambda)))
	       (mapcar '(lambda (foo) (apply foo args))
		       value)
	     (apply value args))))))




;;;; SGML mode: template functions

(defun psgml-markup (entry text)
  (cons entry
	(` (lambda ()
	     (interactive)
	     (psgml-insert-markup (, text))))))

(defun psgml-insert-markup (text)
  (let ((end (psgml-mouse-region))
	before after
	old-text)
    (when end
      (setq old-text (buffer-substring (point) end))
      (delete-region (point) end))
    (setq before (point))
    (if (stringp text)
	(insert text)
      (eval text))
    (setq after (point))
    (goto-char before)
    (when (search-forward "\r" after t)
      (delete-char -1))
    (when old-text (insert old-text))))

(defun psgml-mouse-region ()
  (let (start end)
    (cond
     (psgml-running-lucid
      (cond
       ((null (mark-marker)) nil)
       (t (setq start (region-beginning)
		end (region-end)))))
     ((and transient-mark-mode
	   mark-active)
      (setq start (region-beginning)
	    end (region-end)))
     ((and mouse-secondary-overlay
	   (eq (current-buffer)
	       (overlay-buffer mouse-secondary-overlay)))
      (setq start (overlay-start mouse-secondary-overlay)
	    end (overlay-end mouse-secondary-overlay))
      (delete-overlay mouse-secondary-overlay)))
    (when start
      (goto-char start))
    end))


;;;; SGML mode: indentation

(defun psgml-indent-or-tab ()
  "Indent line in proper way for current major mode."
  (interactive)
  (if (null psgml-indent-step)
      (insert-tab)
    (funcall indent-line-function)))

;;;; Bug reporting

(eval-and-compile
  (autoload 'reporter-submit-bug-report "reporter"))

(defun psgml-submit-bug-report ()
  "Submit via mail a bug report on PSGML."
  (interactive)
  (and (y-or-n-p "Do you really want to submit a report on PSGML? ")
       (reporter-submit-bug-report
	psgml-maintainer-address
	(concat "psgml.el " psgml-version)
	(list
	 'major-mode
	 'psgml-always-quote-attributes
	 'psgml-auto-activate-dtd
	 'psgml-auto-insert-required-elements
	 'psgml-balanced-tag-edit
	 'psgml-catalog-files
	 'psgml-declaration
	 'psgml-doctype
	 'psgml-ecat-files
	 'psgml-indent-data
	 'psgml-indent-step
	 'psgml-leave-point-after-insert
	 'psgml-live-element-indicator
	 'psgml-local-catalogs
	 'psgml-local-ecat-files
	 'psgml-markup-faces
	 'psgml-minimize-attributes
	 'psgml-normalize-trims
	 'psgml-omittag
	 'psgml-omittag-transparent
	 'psgml-parent-document
	 'psgml-public-map
	 'psgml-set-face
	 'psgml-shorttag
	 'psgml-namecase-general
	 'psgml-tag-region-if-active
	 'psgml-use-text-properties
	 ))))


;;;; SGML mode: keys and menus

(if psgml-mode-map
    ()
  (setq psgml-mode-map (make-sparse-keymap)))

;;; Key commands

(define-key psgml-mode-map "\t"    'psgml-indent-or-tab)
;(define-key psgml-mode-map "<"	  'psgml-insert-tag)
(define-key psgml-mode-map ">"     'psgml-close-angle)
(define-key psgml-mode-map "/"     'psgml-slash)
(define-key psgml-mode-map "\C-c#"    'psgml-make-character-reference)
(define-key psgml-mode-map "\C-c-"    'psgml-untag-element)
(define-key psgml-mode-map "\C-c+"    'psgml-insert-attribute)
(define-key psgml-mode-map "\C-c/"    'psgml-insert-end-tag)
(define-key psgml-mode-map "\C-c<"    'psgml-insert-tag)
(define-key psgml-mode-map "\C-c="    'psgml-change-element-name)
(define-key psgml-mode-map "\C-c\C-a" 'psgml-edit-attributes)
(define-key psgml-mode-map "\C-c\C-c" 'psgml-show-context)
(define-key psgml-mode-map "\C-c\C-d" 'psgml-next-data-field)
(define-key psgml-mode-map "\C-c\C-e" 'psgml-insert-element)
(define-key psgml-mode-map "\C-c\C-f\C-e" 'psgml-fold-element)
(define-key psgml-mode-map "\C-c\C-f\C-r" 'psgml-fold-region)
(define-key psgml-mode-map "\C-c\C-f\C-s" 'psgml-fold-subelement)
(define-key psgml-mode-map "\C-c\C-f\C-x" 'psgml-expand-element)
(define-key psgml-mode-map "\C-c\C-i" 'psgml-add-element-to-element)
(define-key psgml-mode-map "\C-c\C-k" 'psgml-kill-markup)
(define-key psgml-mode-map "\C-c\C-l" 'psgml-show-or-clear-log)
(define-key psgml-mode-map "\C-c\r"   'psgml-split-element)
(define-key psgml-mode-map "\C-c\C-n" 'psgml-up-element)
(define-key psgml-mode-map "\C-c\C-o" 'psgml-next-trouble-spot)
(define-key psgml-mode-map "\C-c\C-p" 'psgml-parse-prolog)
(define-key psgml-mode-map "\C-c\C-q" 'psgml-fill-element)
(define-key psgml-mode-map "\C-c\C-r" 'psgml-tag-region)
(define-key psgml-mode-map "\C-c\C-s" 'psgml-unfold-line)
(define-key psgml-mode-map "\C-c\C-t" 'psgml-list-valid-tags)
(define-key psgml-mode-map "\C-c\C-u\C-a" 'psgml-unfold-all)
(define-key psgml-mode-map "\C-c\C-u\C-d" 'psgml-custom-dtd)
(define-key psgml-mode-map "\C-c\C-u\C-e" 'psgml-unfold-element)
(define-key psgml-mode-map "\C-c\C-u\C-l" 'psgml-unfold-line)
(define-key psgml-mode-map "\C-c\C-u\C-m" 'psgml-custom-markup)
(define-key psgml-mode-map "\C-c\C-v" 'psgml-validate)
(define-key psgml-mode-map "\C-c\C-w" 'psgml-what-element)
(define-key psgml-mode-map "\C-c\C-z" 'psgml-trim-and-leave-element)

(define-key psgml-mode-map "\e\C-a"   'psgml-beginning-of-element)
(define-key psgml-mode-map "\e\C-e"   'psgml-end-of-element)
(define-key psgml-mode-map "\e\C-f"   'psgml-forward-element)
(define-key psgml-mode-map "\e\C-b"   'psgml-backward-element)
(define-key psgml-mode-map "\e\C-d"   'psgml-down-element)
(define-key psgml-mode-map "\e\C-u"   'psgml-backward-up-element)
(define-key psgml-mode-map "\e\C-k"   'psgml-kill-element)
(define-key psgml-mode-map "\e\C-@"   'psgml-mark-element)
;;(define-key psgml-mode-map [?\M-\C-\ ] 'psgml-mark-element)
(define-key psgml-mode-map "\e\C-h"   'psgml-mark-current-element)
(define-key psgml-mode-map "\e\C-t"   'psgml-transpose-element)
(define-key psgml-mode-map "\M-\t"    'psgml-complete)

;;;; Menu bar

(easy-menu-define
 psgml-dtd-menu psgml-mode-map "DTD menu"
 '("DTD"
    ["Parse DTD"  psgml-parse-prolog t]
    ("Insert DTD")
    ("Info"
     ["General DTD info"	psgml-general-dtd-info           t]
     ["Describe element type"	psgml-describe-element-type	t]
     ["Describe entity"		psgml-describe-entity		t]
     ["List elements"		psgml-list-elements		t]
     ["List attributes"	psgml-list-attributes		t]
     ["List terminals"		psgml-list-terminals		t]
     ["List content elements"	psgml-list-content-elements	t]
     ["List occur in elements"	psgml-list-occur-in-elements	t]
     )
    "--"
    ["Load Parsed DTD"  psgml-load-dtd t]
    ["Save Parsed DTD"  psgml-save-dtd t]
   ))

(easy-menu-define
 psgml-view-menu psgml-mode-map "View menu"
 '("View"
   ["Fold Element"	psgml-fold-element	t]
   ["Fold Subelement"	psgml-fold-subelement	t]
   ["Unfold Line"	psgml-unfold-line	t]
   ["Unfold Element"	psgml-unfold-element	t]
   ["Expand"		psgml-expand-element	t]
   ["Fold Region"	psgml-fold-region	t]
   ["Unfold All"	psgml-unfold-all		t]
   ["Hide Tags"		psgml-hide-tags		t]
   ["Hide Attributes"	psgml-hide-attributes	t]
   ["Show All Tags"	psgml-show-tags		t]
   ))


(easy-menu-define
 psgml-markup-menu psgml-mode-map "Markup menu"
 '("Markup"
   ["Insert Element"	psgml-element-menu	t]
   ["Insert Start-Tag" psgml-start-tag-menu	t]
   ["Insert End-Tag"	psgml-end-tag-menu	t]
   ["End Current Element"	psgml-insert-end-tag t]
   ["Tag Region"	psgml-tag-region-menu	t]
   ["Insert Attribute"  psgml-attrib-menu	t]
   ["Insert Entity"	psgml-entities-menu	t]
   ["Add Element to Element"	psgml-add-element-menu	t]
   ("Custom markup"   "---")
   ))

(easy-menu-define
 psgml-move-menu psgml-mode-map "Menu of move commands"
 '("Move"
   ["Next trouble spot" psgml-next-trouble-spot t]
   ["Next data field"   psgml-next-data-field   t]
   ["Forward element"	psgml-forward-element t]
   ["Backward element"  psgml-backward-element t]
   ["Up element"	psgml-up-element t]
   ["Down element"	psgml-down-element t]
   ["Backward up element" psgml-backward-up-element t]
   ["Beginning of element" psgml-beginning-of-element t]
   ["End of element"	psgml-end-of-element t]
   ))

(easy-menu-define
 psgml-modify-menu psgml-mode-map "Menu of modification commands"
 '("Modify"
   ["Normalize"			psgml-normalize	t]
   ["Expand All Short References"	psgml-expand-all-shortrefs t]
   ["Expand Entity Reference"	psgml-expand-entity-reference t]
   ["Normalize Element"		psgml-normalize-element t]
   ["Make Character Reference"	psgml-make-character-reference t]
   ["Unmake Character Reference"	(psgml-make-character-reference t) t]
   ["Fill Element"		psgml-fill-element t]
   ["Change Element Name..."	psgml-change-element-name t]
   ["Edit Attributes..."	psgml-edit-attributes t]
   ["Kill Markup"		psgml-kill-markup t]
   ["Kill Element"		psgml-kill-element t]
   ["Untag Element"		psgml-untag-element t]
   ["Trim and leave element"	psgml-trim-and-leave-element t]
   ["Decode Character Entities"  psgml-charent-to-display-char t]
   ["Encode Characters"		psgml-display-char-to-charent t]
   )
 )

(easy-menu-define
 psgml-main-menu psgml-mode-map "Main menu"
 '("PSGML"
   ["Reset Buffer"	normal-mode t]
   ["Show Context"	psgml-show-context t]
   ["What Element"	psgml-what-element t]
   ["List Valid Tags"	psgml-list-valid-tags t]
   ["Show/Hide Warning Log"  psgml-show-or-clear-log t]
   ["Validate"		psgml-validate t]
   ("File Options"   "---")
   ("User Options"   "---")
   ["Submit Bug Report"  psgml-submit-bug-report t]
   ))


(defun psgml-options-menu-items (vars)
  (mapcar (lambda (var)
	    (let ((desc (format "%s [%s]"
				(psgml-variable-description var)
				(psgml-option-value-indicator var)))
		  (type (psgml-variable-type var)))
	      (cond ((consp type)
		     (cons desc
			   (mapcar (lambda (c)
				     (vector
				      (if (consp c) (car c) (format "%s" c))
				      `(setq ,var ',(if (consp c) (cdr c) c))
				      t))
				   type)))
		    (t
		     (vector desc `(psgml-do-set-option ',var) t)))))
	  vars))

(defun psgml-option-value-indicator (var)
  (let ((type (psgml-variable-type var))
	(val (symbol-value var)))
    (cond
     ((eq type 'toggle)
      (if val "Yes" "No"))
     ((eq type 'string)
      (if (stringp val)
	  (substring val 0 (min (length val) 4))
	"-"))
     ((and (atom type) val)
      "...")
     ((consp type)
      (or (car (rassq val type))
	  val))
     (t
      "-"))))

(defvar psgml-last-options-menu-values ())

(defun psgml-any-option-changed (oldvalues vars)
  (not (loop for val in oldvalues
	     for var in vars
	     always (eq val (symbol-value var)))))

(defun psgml-update-options-menu (menuname option-vars &optional save-func)
  (let ((last-values (assoc menuname psgml-last-options-menu-values)))
    (when (or (null last-values)
	      (psgml-any-option-changed (cdr last-values)
				       option-vars))
      (easy-menu-change '("PSGML") menuname
		      (nconc (psgml-options-menu-items option-vars)
			     (if save-func
				 (list "---"
				       (vector (format "Save %s" menuname)
					       save-func t)))))
      (unless last-values
	(setq last-values (cons menuname nil))
	(push last-values psgml-last-options-menu-values))
      (setf (cdr last-values) (mapcar (function symbol-value) option-vars)))))


(defun psgml-update-all-options-menus ()
  (psgml-update-options-menu "File Options" psgml-file-options
			    'psgml-save-options)
  (psgml-update-options-menu "User Options" psgml-user-options)
  nil)

(defun psgml-compute-insert-dtd-items ()
  (loop for e in psgml-custom-dtd collect
	(vector (first e)
		(` (psgml-doctype-insert (, (cadr e)) '(, (cddr e))))
		t)))

(defun psgml-compute-custom-markup-items ()
  (loop for e in psgml-custom-markup collect
	(vector (first e)
		(` (psgml-insert-markup  (, (cadr e))))
		t)))

(defun psgml-build-custom-menus ()
  "Build custom parts of Markup and DTD menus."
  (let ((button3 (lookup-key (current-local-map) [button3])))
    (unless (or (null button3)
		(numberp button3))
      (local-set-key [button3] button3))
    (when psgml-custom-dtd
      (easy-menu-change '("DTD") "Insert DTD"
			(psgml-compute-insert-dtd-items)))
    (when psgml-custom-markup
      (easy-menu-change '("Markup") "Custom markup"
			(psgml-compute-custom-markup-items))))
  nil)


;;;; Post command hook

(defvar psgml-auto-activate-dtd-tried nil)
(make-variable-buffer-local 'psgml-auto-activate-dtd-tried)

(defvar psgml-buffer-parse-state nil
  "If the buffers DTD has been activated this contains the parser state.
The parser state has been created with `psgml-make-pstate' and contains
the information about the DTD and the parse tree.  This parse state is
actually only the state that persists between commands.")
(make-variable-buffer-local 'psgml-buffer-parse-state)

(eval-and-compile			; Interface to psgml-parse
  (loop for fun in '(psgml-need-dtd psgml-update-display
				   psgml-fontify-buffer
				   psgml-subst-expand
				   psgml-declaration)
	do (autoload fun "psgml-parse")))


(defun psgml-command-post ()
  (when (or (memq major-mode '(psgml-mode pxml-mode))
	    (fboundp 'make-local-hook))
    ;; Explanation if make-local-hook is defined then this is called
    ;; from a local hook and need not check major-mode.
    (when (and (null psgml-buffer-parse-state)
	       psgml-auto-activate-dtd
	       (null psgml-auto-activate-dtd-tried)
	       (not (zerop (buffer-size)))
	       (looking-at ".*<"))
      (setq psgml-auto-activate-dtd-tried t)
      (ignore-errors
	(psgml-need-dtd)
	(psgml-fontify-buffer 0)))
    (when psgml-buffer-parse-state
      (psgml-update-display))))


;;;; SGML mode: major mode definition

;;; This section is mostly from sgml-mode by James Clark.

;;;###autoload
(defun psgml-mode ()
  "Major mode for editing SGML.\\<psgml-mode-map>
Makes > display the matching <.  Makes / display matching /.
Use \\[psgml-validate] to validate your document with an SGML parser.

You can find information with:
\\[psgml-show-context]  Show the nesting of elements at cursor position.
\\[psgml-list-valid-tags]  Show the tags valid at cursor position.

Insert tags with completion of contextually valid tags with \\[psgml-insert-tag].
End the current element with \\[psgml-insert-end-tag].  Insert an element (i.e.
both start and end tag) with \\[psgml-insert-element].  Or tag a region with
\\[psgml-tag-region].

To tag a region with the mouse, use transient mark mode or secondary selection.

Structure editing:
\\[psgml-backward-element]  Moves backwards over the previous element.
\\[psgml-forward-element]  Moves forward over the nex element.
\\[psgml-down-element]  Move forward and down one level in the element structure.
\\[psgml-backward-up-element]  Move backward out of this element level.
\\[psgml-beginning-of-element]  Move to after the start tag of the current element.
\\[psgml-end-of-element]  Move to before the end tag of the current element.
\\[psgml-kill-element]  Kill the element following the cursor.

Finding interesting positions
\\[psgml-next-data-field]  Move forward to next point where data is allowed.
\\[psgml-next-trouble-spot]  Move forward to next point where something is
	amiss with the structure.

Folding and unfolding
\\[psgml-fold-element]  Fold the lines comprising the current element, leaving
	the first line visible.
\\[psgml-fold-subelement]  Fold the elements in the content of the current element.
	Leaving the first line of every element visible.
\\[psgml-unfold-line]  Show hidden lines in current line.

User options:

psgml-omittag  Set this to reflect OMITTAG in the SGML declaration.
psgml-shortag  Set this to reflect SHORTTAG in the SGML declaration.
psgml-namecase-general  Set this to reflect NAMECASE GENERAL in the SGML declaration.
psgml-auto-insert-required-elements  If non-nil, automatically insert required
	elements in the content of an inserted element.
psgml-balanced-tag-edit  If non-nil, always insert start-end tag pairs.
psgml-omittag-transparent  If non-nil, will show legal tags inside elements
	with omitable start tags and legal tags beyond omitable end tags.
psgml-leave-point-after-insert  If non-nil, the point will remain after
	inserted tag(s).
psgml-warn-about-undefined-elements  If non-nil, print a warning when a tag
	for a undefined element is found.
psgml-max-menu-size  Max number of entries in Tags and Entities menus before
	they are split into several panes.
psgml-always-quote-attributes  If non-nil, quote all attribute values
	inserted after finishing edit attributes.
psgml-minimize-attributes  Determines minimization of attributes inserted by
	edit-attributes.
psgml-normalize-trims  If non-nil, psgml-normalize will trim off white space
	from end of element when adding end tag.
psgml-indent-step  How much to increament indent for every element level.
psgml-indent-data  If non-nil, indent in data/mixed context also.
psgml-set-face     If non-nil, psgml will set the face of parsed markup.
psgml-markup-faces The faces used when the above variable is non-nil.
psgml-public-map  Mapping from public identifiers to file names.
psgml-offer-save  If non-nil, ask about saving modified buffers before
		\\[psgml-validate] is run.

All bindings:
\\{psgml-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq psgml-pxml-p nil)
  (setq local-abbrev-table psgml-mode-abbrev-table)
  (use-local-map psgml-mode-map)
  (setq mode-name "PSGML")
  (setq major-mode 'psgml-mode)

  ;; A start or end tag by itself on a line separates a paragraph.
  ;; This is desirable because SGML discards a newline that appears
  ;; immediately after a start tag or immediately before an end tag.

  (set (make-local-variable 'paragraph-separate)
	"^[ \t\n]*$\\|\
^[ \t]*</?\\([_A-Za-z]\\([-:._A-Za-z0-9= \t\n]\\|\
\"[^\"]*\"\\|'[^']*'\\)*\\)?>$")
  (set (make-local-variable 'paragraph-start)
       paragraph-separate)

  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'psgml-comment-indent)
  (make-local-variable 'comment-start-skip)
  ;; This will allow existing comments within declarations to be
  ;; recognized.  [Does not work well with auto-fill, Lst/940205]
  ;;(setq comment-start-skip "--[ \t]*")
  (setq comment-start-skip "<!--[ \t]*")
  ;; Added for psgml:
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'psgml-indent-line)
  (make-local-variable 'mode-line-format)
  ;; Modify mode-line-format with susbt (sugested by wing)
  (setq mode-line-format
	(subst '("" mode-name psgml-active-dtd-indicator) 'mode-name
	       mode-line-format))
  (make-local-variable 'psgml-default-dtd-file)
  (when (setq psgml-default-dtd-file (psgml-default-dtd-file))
    (unless (file-exists-p psgml-default-dtd-file)
      (setq psgml-default-dtd-file nil)))
  (cond ((fboundp 'make-local-hook)
	 ;; emacs>= 19.29
	 (make-local-hook 'post-command-hook)
	 (add-hook 'post-command-hook 'psgml-command-post 'append 'local)
	 (unless psgml-running-lucid
	   ;; XEmacs 20.4 doesn't handle local activate-menubar-hook
	   ;; it tries to call the function `t' when using the menubar
	   (make-local-hook 'activate-menubar-hook))
	 (add-hook 'activate-menubar-hook 'psgml-update-all-options-menus
		   nil 'local))
	(t
	 ;; emacs< 19.29
	 (add-hook 'post-command-hook 'psgml-command-post 'append)
	 (add-hook 'menu-bar-update-hook 'psgml-update-all-options-menus)
	 ))
  (run-hooks 'text-mode-hook 'psgml-mode-hook)
  (easy-menu-add psgml-main-menu)
  (easy-menu-add psgml-modify-menu)
  (easy-menu-add psgml-move-menu)
  (easy-menu-add psgml-markup-menu)
  (easy-menu-add psgml-view-menu)
  (easy-menu-add psgml-dtd-menu)
  (psgml-build-custom-menus))



;;;###autoload
(define-derived-mode pxml-mode psgml-mode "PXML"
  (setq psgml-pxml-p t)
  ;; XML-friendly settings
  (setq psgml-omittag nil)
  (setq psgml-shorttag nil)
  (setq psgml-namecase-general nil)
  (setq psgml-minimize-attributes nil)
  (setq psgml-always-quote-attributes t)
  (setq psgml-validate-command "nsgmls -wxml -s %s %s")
  (unless psgml-declaration
    (make-local-variable 'psgml-declaration)
    (setq psgml-declaration psgml-pxml-declaration)))

(defun psgml-default-dtd-file ()
  (and (buffer-file-name)
       (let ((base (file-name-nondirectory (buffer-file-name))))
	 (concat
	  (cond ((string-match "\\.[^.]+$" base)
		 (substring base 0 (match-beginning 0)))
		(t
		 base))
	  ".ced"))))

(defun psgml-comment-indent ()
  (if (and (looking-at "--")
	   (not (and (eq (char-after (1- (point))) ?!)
		     (eq (char-after (- (point) 2)) ?<))))
      (progn
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))
    0))

(defconst psgml-start-tag-regex
  "<[_A-Za-z]\\([-:.A-Za-z0-9= \n\t]\\|\"[^\"]*\"\\|'[^']*'\\)*"
  "Regular expression that matches a non-empty start tag.
Any terminating > or / is not matched.")

(defvar psgml-mode-markup-syntax-table nil
  "Syntax table used for scanning SGML markup.")

(if psgml-mode-markup-syntax-table
    ()
  (setq psgml-mode-markup-syntax-table (make-syntax-table))
  (modify-syntax-entry ?< "(>" psgml-mode-markup-syntax-table)
  (modify-syntax-entry ?> ")<" psgml-mode-markup-syntax-table)
  (modify-syntax-entry ?- "_ 1234" psgml-mode-markup-syntax-table)
  (modify-syntax-entry ?\' "\"" psgml-mode-markup-syntax-table))

(defconst psgml-angle-distance 4000
  "*If non-nil, is the maximum distance to search for matching <.")

(defun psgml-close-angle (arg)
  "Insert > and display matching <."
  (interactive "p")
  (insert-char ?> arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos))
	(save-excursion
	  (save-restriction
	    (if psgml-angle-distance
		(narrow-to-region (max (point-min)
				       (- (point) psgml-angle-distance))
				  oldpos))
	    ;; See if it's the end of a marked section.
	    (and (> (- (point) (point-min)) 3)
		 (eq (char-after (- (point) 2)) ?\])
		 (eq (char-after (- (point) 3)) ?\])
		 (re-search-backward "<!\\[\\(-?[A-Za-z0-9. \t\n&;]\\|\
--\\([^-]\\|-[^-]\\)*--\\)*\\["
				     (point-min)
				     t)
		 (let ((msspos (point)))
		   (if (and (search-forward "]]>" oldpos t)
			    (eq (point) oldpos))
		       (setq blinkpos msspos))))
	    ;; This handles cases where the > ends one of the following:
	    ;; markup declaration starting with <! (possibly including a
	    ;; declaration subset); start tag; end tag; SGML declaration.
	    (if blinkpos
		()
	      (goto-char oldpos)
	      (condition-case ()
		  (let ((oldtable (syntax-table))
			(parse-sexp-ignore-comments t))
		    (unwind-protect
			(progn
			  (set-syntax-table psgml-mode-markup-syntax-table)
			  (setq blinkpos (scan-sexps oldpos -1)))
		      (set-syntax-table oldtable)))
		(error nil))
	      (and blinkpos
		   (goto-char blinkpos)
		   (or
		    ;; Check that it's a valid delimiter in context.
		    (not (looking-at
			  "<\\(\\?\\|/?[A-Za-z>]\\|!\\([[A-Za-z]\\|--\\)\\)"))
		    ;; Check that it's not a net-enabling start tag
		    ;; nor an unclosed start-tag.
		    (looking-at (concat psgml-start-tag-regex "[/<]"))
		    ;; Nor an unclosed end-tag.
		    (looking-at "</[A-Za-z][-:.A-Za-z0-9]*[ \t]*<"))
		   (setq blinkpos nil)))
	    (if blinkpos
		()
	      ;; See if it's the end of a processing instruction.
	      (goto-char oldpos)
	      (if (search-backward "<?" (point-min) t)
		  (let ((pipos (point)))
		    (if (and (search-forward ">" oldpos t)
			     (eq (point) oldpos))
			(setq blinkpos pipos))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring blinkpos
					     (progn (end-of-line)
						    (point)))))))))))

;;; I doubt that null end tags are used much for large elements,
;;; so use a small distance here.
(defconst psgml-slash-distance 1000
  "*If non-nil, is the maximum distance to search for matching /.")

(defun psgml-slash (arg)
  "Insert / and display any previous matching /.
Two /s are treated as matching if the first / ends a net-enabling
start tag, and the second / is the corresponding null end tag."
  (interactive "p")
  (insert-char ?/ arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos)
	    (level 0))
	(save-excursion
	  (save-restriction
	    (if psgml-slash-distance
		(narrow-to-region (max (point-min)
				       (- (point) psgml-slash-distance))
				  oldpos))
	    (if (and (re-search-backward psgml-start-tag-regex (point-min) t)
		     (eq (match-end 0) (1- oldpos)))
		()
	      (goto-char (1- oldpos))
	      (while (and (not blinkpos)
			  (search-backward "/" (point-min) t))
		(let ((tagend (save-excursion
				(if (re-search-backward psgml-start-tag-regex
							(point-min) t)
				    (match-end 0)
				  nil))))
		  (if (eq tagend (point))
		      (if (eq level 0)
			  (setq blinkpos (point))
			(setq level (1- level)))
		    (setq level (1+ level)))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring (progn
					       (beginning-of-line)
					       (point))
					     (1+ blinkpos))))))))))

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun psgml-default-validate-command ()
  (cond
   ((consp psgml-validate-command)
    (let ((validate-subst
	   (list
	    (cons ?b (and (buffer-file-name)
			  (file-name-nondirectory (buffer-file-name))))
	    (cons ?s (psgml-declaration))
	    (cons ?v psgml-declaration)
	    (cons ?d psgml-doctype))))
      (loop for template in psgml-validate-command
	    thereis
	    (psgml-subst-expand template validate-subst))))
   (t
    (apply 'format psgml-validate-command
	   (if psgml-validate-files
	       (funcall psgml-validate-files)
	     (list (or psgml-declaration "")
		   (let ((name (buffer-file-name)))
		     (if name
			 (file-name-nondirectory name)
		       ""))))))))

(defun psgml-validate (command)
  "Validate an SGML document.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the SGML document that caused it."
  (interactive
   (list (read-from-minibuffer "Validate command: "
			       (psgml-default-validate-command)
			       nil nil 'psgml-validate-command-history)))
  (if psgml-offer-save
      (save-some-buffers nil nil))
  (compile-internal command "No more errors" "SGML validation"
		    nil
		    psgml-validate-error-regexps))



;;;; Autoloads and hooks

(autoload 'psgml-doctype-insert "psgml-edit"
	  nil
	  nil nil)
(autoload 'psgml-indent-line "psgml-edit" nil)
(autoload 'psgml-element-endable-p "psgml-edit" nil)

;;; Generated by psgml-build-autoloads

(autoload 'psgml-load-dtd "psgml-parse" "Load a saved DTD from FILE." t)
(autoload 'psgml-show-or-clear-log "psgml-parse" "Show the *SGML LOG* buffer if it is not showing, or clear and
remove it if it is showing." t)
(autoload 'psgml-parse-prolog "psgml-parse" "Parse the document prolog to learn the DTD." t)
(autoload 'psgml-beginning-of-element "psgml-edit" "Move to after the start-tag of the current element.
If the start-tag is implied, move to the start of the element." t)
(autoload 'psgml-end-of-element "psgml-edit" "Move to before the end-tag of the current element." t)
(autoload 'psgml-backward-up-element "psgml-edit" "Move backward out of this element level.
That is move to before the start-tag or where a start-tag is implied." t)
(autoload 'psgml-up-element "psgml-edit" "Move forward out of this element level.
That is move to after the end-tag or where an end-tag is implied." t)
(autoload 'psgml-forward-element "psgml-edit" "Move forward over next element." t)
(autoload 'psgml-backward-element "psgml-edit" "Move backward over previous element at this level.
With implied tags this is ambigous." t)
(autoload 'psgml-down-element "psgml-edit" "Move forward and down one level in the element structure." t)
(autoload 'psgml-kill-element "psgml-edit" "Kill the element following the cursor." t)
(autoload 'psgml-transpose-element "psgml-edit" "Interchange element before point with element after point, leave point after." t)
(autoload 'psgml-mark-element "psgml-edit" "Set mark after next element." t)
(autoload 'psgml-mark-current-element "psgml-edit" "Set mark at end of current element, and leave point before current element." t)
(autoload 'psgml-change-element-name "psgml-edit" "Replace the name of the current element with a new name.
Eventual attributes of the current element will be translated if
possible." t)
(autoload 'psgml-untag-element "psgml-edit" "Remove tags from current element." t)
(autoload 'psgml-kill-markup "psgml-edit" "Kill next tag, markup declaration or process instruction." t)
(autoload 'psgml-fold-region "psgml-edit" "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides." t)
(autoload 'psgml-fold-element "psgml-edit" "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature." t)
(autoload 'psgml-fold-subelement "psgml-edit" "Fold all elements current elements content, leaving the first lines visible.
This uses the selective display feature." t)
(autoload 'psgml-unfold-line "psgml-edit" "Show hidden lines in current line." t)
(autoload 'psgml-unfold-element "psgml-edit" "Show all hidden lines in current element." t)
(autoload 'psgml-expand-element "psgml-edit" "As psgml-fold-subelement, but unfold first." t)
(autoload 'psgml-unfold-all "psgml-edit" "Show all hidden lines in buffer." t)
(autoload 'psgml-next-data-field "psgml-edit" "Move forward to next point where data is allowed." t)
(autoload 'psgml-next-trouble-spot "psgml-edit" "Move forward to next point where something is amiss with the structure." t)
(autoload 'psgml-list-valid-tags "psgml-edit" "Display a list of the contextually valid tags." t)
(autoload 'psgml-show-context "psgml-edit" "Display where the cursor is in the element hierarchy." t)
(autoload 'psgml-what-element "psgml-edit" "Display what element is under the cursor." t)
(autoload 'psgml-insert-tag "psgml-edit" "Insert a tag, reading tag name in minibuffer with completion.
If the variable psgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If psgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If psgml-leave-point-after-insert is nil the point is left
after the first tag inserted." t)
(autoload 'psgml-insert-element "psgml-edit" "Reads element name from minibuffer and inserts start and end tags." t)
(autoload 'psgml-tag-region "psgml-edit" "Reads element name from minibuffer and inserts start and end tags." t)
(autoload 'psgml-insert-end-tag "psgml-edit" "Insert end-tag for the current open element." t)
(autoload 'psgml-insert-attribute "psgml-edit" "Read attribute name and value from minibuffer and insert attribute spec." t)
(autoload 'psgml-split-element "psgml-edit" "Split the current element at point.
If repeated, the containing element will be split before the beginning
of then current element." t)
(autoload 'psgml-custom-dtd "psgml-edit" "Insert a DTD declaration from the psgml-custom-dtd alist." t)
(autoload 'psgml-custom-markup "psgml-edit" "Insert markup from the psgml-custom-markup alist." t)
(autoload 'psgml-tags-menu "psgml-edit" "Pop up a menu with valid tags and insert the choosen tag.
If the variable psgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If psgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If psgml-leave-point-after-insert is nil the point is left
after the first tag inserted." t)
(autoload 'psgml-element-menu "psgml-edit" "Pop up a menu with valid elements and insert choice.
If psgml-leave-point-after-insert is nil the point is left after the first
tag inserted." t)
(autoload 'psgml-start-tag-menu "psgml-edit" "Pop up a menu with valid start-tags and insert choice." t)
(autoload 'psgml-end-tag-menu "psgml-edit" "Pop up a menu with valid end-tags and insert choice." t)
(autoload 'psgml-tag-region-menu "psgml-edit" "Pop up a menu with valid elements and tag current region with the choice." t)
(autoload 'psgml-entities-menu "psgml-edit" nil t)
(autoload 'psgml-attrib-menu "psgml-edit" "Pop up a menu of the attributes of the current element
\(or the element whith start-tag before point)." t)
(autoload 'psgml-fill-element "psgml-edit" "Fill bigest enclosing element with mixed content.
If current element has pure element content, recursively fill the
subelements." t)
(autoload 'psgml-edit-attributes "psgml-edit" "Edit attributes of current element.
Editing is done in a separate window." t)
(autoload 'psgml-edit-attrib-finish "psgml-edit" "Finish editing and insert attribute values in original buffer." t)
(autoload 'psgml-edit-attrib-default "psgml-edit" "Set current attribute value to default." t)
(autoload 'psgml-edit-attrib-clear "psgml-edit" "Kill the value of current attribute." t)
(autoload 'psgml-edit-attrib-field-start "psgml-edit" "Go to the start of the attribute value field." t)
(autoload 'psgml-edit-attrib-field-end "psgml-edit" "Go to the end of the attribute value field." t)
(autoload 'psgml-edit-attrib-next "psgml-edit" "Move to next attribute value." t)
(autoload 'psgml-hide-tags "psgml-edit" "Hide all tags in buffer." t)
(autoload 'psgml-show-tags "psgml-edit" "Show hidden tags in buffer." t)
(autoload 'psgml-hide-attributes "psgml-edit" "Hide all attribute specifications in the buffer." t)
(autoload 'psgml-show-attributes "psgml-edit" "Show all attribute specifications in the buffer." t)
(autoload 'psgml-expand-all-shortrefs "psgml-edit" "Expand all short references in the buffer.
Short references to text entities are expanded to the replacement text
of the entity other short references are expanded into general entity
references.  If argument, TO-ENTITY, is non-nil, or if called
interactive with numeric prefix argument, all short references are
replaced by generaly entity references." t)
(autoload 'psgml-normalize "psgml-edit" "Normalize buffer by filling in omitted tags and expanding empty tags.
Argument TO-ENTITY controls how short references are expanded as with
`psgml-expand-all-shortrefs'.  An optional argument ELEMENT can be the
element to normalize insted of the whole buffer, if used no short
references will be expanded." t)
(autoload 'psgml-normalize-element "psgml-edit" nil t)
(autoload 'psgml-make-character-reference "psgml-edit" "Convert character after point into a character reference.
If called with a numeric argument, convert a character reference back
to a normal character.  If called from a program, set optional
argument INVERT to non-nil." t)
(autoload 'psgml-expand-entity-reference "psgml-edit" "Insert the text of the entity referenced at point." t)
(autoload 'psgml-complete "psgml-edit" "Complete the word/tag/entity before point.
If it is a tag (starts with < or </) complete with valid tags.
If it is an entity (starts with &) complete with declared entities.
If it is a markup declaration (starts with <!) complete with markup
declaration names.
If it is something else complete with ispell-complete-word." t)
(autoload 'psgml-file-options-menu "psgml-edit" nil t)
(autoload 'psgml-user-options-menu "psgml-edit" nil t)
(autoload 'psgml-add-element-to-element "psgml-edit" "Add an element of type GI to the current element.
The element will be added at the last legal position if FIRST is `nil',
otherwise it will be added at the first legal position." t)
(autoload 'psgml-save-dtd "psgml-dtd" "Save the parsed dtd on FILE." t)
(autoload 'psgml-list-elements "psgml-info" "List the elements and their attributes in the current DTD." t)
(autoload 'psgml-list-attributes "psgml-info" "List the attributes and in which elements they occur." t)
(autoload 'psgml-list-terminals "psgml-info" "List the elements that can have data in their content." t)
(autoload 'psgml-list-content-elements "psgml-info" "List all element types and the element types that can occur in its content." t)
(autoload 'psgml-list-occur-in-elements "psgml-info" "List all element types and where it can occur." t)
(autoload 'psgml-describe-entity "psgml-info" "Describe the properties of an entity as declared in the current DTD." t)
(autoload 'psgml-describe-element-type "psgml-info" "Describe the properties of an element type as declared in the current DTD." t)
(autoload 'psgml-general-dtd-info "psgml-info" "Display information about the current DTD." t)
(autoload 'psgml-charent-to-display-char "psgml-charent" "Replace character entities with their display character equivalents" t)
(autoload 'psgml-display-char-to-charent "psgml-charent" "Replace displayable characters with their character entity equivalents" t)


;;;; Last provisions

(provide 'psgml)
(provide 'psgml-mode)

(cond
 (psgml-running-lucid
  (require 'psgml-lucid))
 (t
  (require 'psgml-other)))

;;; psgml.el ends here
