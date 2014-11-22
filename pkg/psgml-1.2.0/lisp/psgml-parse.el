;;;; psgml-parse.el --- Parser for SGML-editing mode with parsing support
;; $Id: psgml-parse.el,v 2.67 1999/10/19 16:20:26 lenst Exp $

;; Copyright (C) 1994, 1995, 1996, 1997, 1998 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Acknowledgment:
;;   The catalog and XML parsing code was contributed by
;;      David Megginson <david@megginson.com>

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


;;;; Commentary:

;; Part of major mode for editing the SGML document-markup language.


;;;; Code:

(require 'psgml)

;;; Interface to psgml-dtd
(eval-and-compile
  (autoload 'psgml-do-usemap-element  "psgml-dtd")
  (autoload 'psgml-write-dtd	     "psgml-dtd")
  (autoload 'psgml-check-dtd-subset   "psgml-dtd") )


;;;; Advise to do-auto-fill

(defvar psgml-auto-fill-inhibit-function nil
  "If non-nil, it should be a function of no arguments.
The functions is evaluated before the standard auto-fill function,
do-auto-fill, tries to fill a line. If the function returns a true
value the auto-fill is inhibited.")

;;(defadvice do-auto-fill (around disable-auto-fill-hook activate)
;;  (or (and psgml-auto-fill-inhibit-function
;;	   (funcall psgml-auto-fill-inhibit-function))
;;      ad-do-it))


;;;; Variables

;;; Hooks

(defvar psgml-open-element-hook nil
  "The hook run by `psgml-open-element'.
Theses functions are called with two arguments, the first argument is
the opened element and the second argument is the attribute specification
list.  It is probably best not to refer to the content or the end-tag of
the element.")

(defvar psgml-close-element-hook nil
  "The hook run by `psgml-close-element'.
These functions are invoked with `psgml-current-tree' bound to the
element just parsed.")

(defvar psgml-doctype-parsed-hook nil
  "This hook is caled after the doctype has been parsed.
It can be used to load any additional information into the DTD structure.")

(defvar psgml-sysid-resolve-functions nil
  "This variable should contain a list of functions.
Each function should take one argument, the system identifier of an entity.
If the function can handle that identifier, it should insert the text
of the entity into the current buffer at point and return t.  If the
system identifier is not handled the function should return nil.")


;;; Internal variables

(defconst psgml-pcdata-token (intern "#PCDATA"))

(defvar psgml-computed-map nil
  "Internal representation of entity search map.")

(defvar psgml-used-entity-map nil
  "The value of `psgml-current-entity-map' used to compute the map in
`psgml-compute-map'.")

(defvar psgml-last-element nil
  "Used to keep information about position in element structure between
commands.")

(defconst psgml-users-of-last-element
  '(psgml-beginning-of-element
    psgml-end-of-element
    psgml-up-element
    psgml-backward-up-element
    psgml-backward-element
    psgml-forward-element
    psgml-down-element
    psgml-show-context
    psgml-next-data-field
    )
  "List of commands that set the psgml-last-element variable.")

(defvar psgml-parser-syntax nil
  "Syntax table used during parsing.")

(defvar psgml-ecat-assoc nil
  "Assoc list caching parsed ecats.")

(defvar psgml-catalog-assoc nil
  "Assoc list caching parsed catalogs.")


;;; Variables dynamically bound to affect parsing

(defvar psgml-throw-on-warning nil
  "Set to a symbol other than nil to make psgml-log-warning throw to that symbol.")

(defvar psgml-throw-on-error nil
  "Set to a symbol other than nil to make psgml-error throw to that symbol.")

(defvar psgml-show-warnings nil
  "Set to t to show warnings.")

(defvar psgml-close-element-trap nil
  "Can be nil for no trap, an element or t for any element.
Tested by psgml-close-element to see if the parse should be ended.")

(defvar psgml-goal 0
  "Point in buffer to parse up to.")

(defvar psgml-shortref-handler (function psgml-handle-shortref)
  "Function called by parser to handle a short reference.
Called with the entity as argument.  The start and end of the
short reference is `psgml-markup-start' and point.")

(defvar psgml-data-function nil
  "Function called with parsed data.")

(defvar psgml-entity-function nil
  "Function called with entity referenced at current point in parse.")

(defvar psgml-pi-function nil
  "Function called with parsed process instruction.")

(defvar psgml-signal-data-function nil
  "Called when some data characters are conceptually parsed,
e.g. a data entity reference.")

(defvar psgml-throw-on-element-change nil
  "Throw tag.")

;;; Global variables active during parsing

(defvar psgml-parsing-dtd nil
  "This variable is bound to `t' while parsing a DTD (subset).")

(defvar psgml-rs-ignore-pos nil
  "Set to position of last parsing start in current buffer.")
(make-variable-buffer-local 'psgml-rs-ignore-pos)

(defvar psgml-dtd-info nil
  "Holds the `psgml-dtd' structure describing the current DTD.")

(defvar psgml-current-namecase-general t
  "Value of `psgml-namecase-general' in main buffer. Valid during parsing.")

(defvar psgml-current-omittag nil
  "Value of `psgml-omittag' in main buffer. Valid during parsing.")

(defvar psgml-current-shorttag nil
  "Value of `psgml-shorttag' in main buffer. Valid during parsing.")

(defvar psgml-current-localcat nil
  "Value of `psgml-local-catalogs' in main buffer. Valid during parsing.")

(defvar psgml-current-local-ecat nil
  "Value of `psgml-local-ecat-files' in main buffer. Valid during parsing.")

(defvar psgml-current-top-buffer nil
  "The buffer of the document entity, the main buffer.
Valid during parsing. This is used to find current directory for
catalogs.")

(defvar psgml-current-state nil
  "Current state in content model or model type if CDATA, RCDATA or ANY.")

(defvar psgml-current-shortmap nil
  "The current active short reference map.")

(defvar psgml-current-tree nil
  "Current parse tree node, identifies open element.")

(defvar psgml-previous-tree nil
  "Previous tree node in current tree.
This is nil if no previous node.")

(defvar psgml-markup-type nil
"Contains the type of markup parsed last.
The value is a symbol:
nil	- pcdata or space
CDATA	- CDATA or RCDATA
comment	- comment declaration
doctype	- doctype declaration
end-tag
ignored	- ignored marked section
ms-end	- marked section start, if not ignored
ms-start - marked section end, if not ignored
pi	- processing instruction
psgml	- SGML declaration
start-tag
entity  - general entity reference
param   - parameter reference
shortref- short reference
mdecl   - markup declaration
")

(defvar psgml-top-tree nil
  "Root node of parse tree during parsing.")

(defvar psgml-markup-tree nil
  "Tree node of markup parsed.
In case markup closed element this is different from psgml-current-tree.
Only valid after `psgml-parse-to'.")

(defvar psgml-markup-start nil
  "Start point of markup beeing parsed.")

(defvar psgml-conref-flag nil
  "This variable is set by `psgml-parse-attribute-specification-list'
if a CONREF attribute is parsed.")

(defvar psgml-no-elements nil
  "Number of declared elements.")

;;; Vars used in *param* buffers

(defvar psgml-previous-buffer nil)

(defvar psgml-current-eref nil
  "This is the entity reference used to enter current entity.
If this is nil, then current entity is main buffer.")

(defvar psgml-current-file nil
  "This is the file name of the current entity")

(defvar psgml-scratch-buffer nil
  "The global value of this variable is the first scratch buffer for
entities. The entity buffers can have a buffer local value for this variable
to point to the next scratch buffer.")

(defvar psgml-last-entity-buffer nil)

;;; For loading DTD

(eval-and-compile
  (defconst psgml-max-single-octet-number 250
    "Octets greater than this is the first of a two octet coding."))

(defvar psgml-read-token-vector nil)	; Vector of symbols used to decode
					; token numbers.
(defvar psgml-read-nodes nil)		; Vector of nodes used when reading
					; a finite automaton.

;; Buffer local variables

(defvar psgml-loaded-dtd nil
  "File name corresponding to current DTD.")
(make-variable-buffer-local 'psgml-loaded-dtd)

(defvar psgml-current-element-name nil
  "Name of current element for mode line display.")
(make-variable-buffer-local 'psgml-current-element-name)

;;;; Build parser syntax table

(setq psgml-parser-syntax (make-syntax-table))

(let ((i 0))
  (while (< i 256)
    (modify-syntax-entry i " " psgml-parser-syntax)
    (setq i (1+ i))))

(mapconcat (function (lambda (c)
	     (modify-syntax-entry c "w" psgml-parser-syntax)))
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtsuvwxyz" "")
(mapconcat (function (lambda (c)
		       (modify-syntax-entry c "_" psgml-parser-syntax)))
	   "-.0123456789" "")


;;(progn (set-syntax-table psgml-parser-syntax) (describe-syntax))

(defconst pxml-parser-syntax
  (let ((tab (make-syntax-table)))
    (let ((i 0))
      (while (< i 128)
	(modify-syntax-entry i " " tab)
	(setq i (1+ i))))
    (mapconcat (function (lambda (c)
			   (modify-syntax-entry c "w" tab)))
	       "_:ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtsuvwxyz" "")
    (mapconcat (function (lambda (c)
			   (modify-syntax-entry c "_" tab)))
	       "-.0123456789·" "")
    tab))

;;(progn (set-syntax-table pxml-parser-syntax) (describe-syntax))

(defmacro psgml-with-parser-syntax (&rest body)
  (` (let ((normal-syntax-table (syntax-table)))
       (set-syntax-table (if psgml-pxml-p pxml-parser-syntax psgml-parser-syntax))
       (unwind-protect
	   (progn (,@ body))
	 (set-syntax-table normal-syntax-table)))))


;;;; State machine

;; From the parsers POV a state is a mapping from tokens (in sgml it
;; is primitive state tokens) to states.  The pairs of the mapping is
;; called moves.

;; DFAs are always represented by the start state, which is a
;; normal state.  Normal states contain moves of two types:
;; 1. moves for required tokens, 2. moves for optional tokens.
;; By design these are keept in two different sets.
;; [Alt: they could perhaps have been keept in one set but
;; marked in different ways.]

;; The and-model groups creates too big state machines, therefor
;; there is a datastruture called and-node.

;; An and-node is a specification for a dfa that has not been computed.
;; It contains a set of dfas that all have to be traversed before going
;; to the next state.  The and-nodes are only stored in moves and are
;; not seen by the parser.  When a move is taken the and-node is converted
;; to an and-state.

;; An and-state keeps track of which dfas still need to be
;; traversed and the state of the current dfa.

;; move = <token, node>

;; node = normal-state | and-node

;; and-node = <dfas, next>
;; where: dfas is a set of normal-state
;;        next is a normal-state

;; State = normal-state | and-state
;; The parser only knows about the state type.

;; normal-state = <opts, reqs>
;; where: opts is a set of moves for optional tokens
;;	  reqs is a set of moves for required tokens

;; and-state = <substate, dfas, next>
;; where: substate is a normal-state
;;        dfas is a set of states
;;        next is the next state

;; The and-state is only used during the parsing.
;; Primitiv functions to get data from parse state need
;; to know both normal-state and and-state.


;;; Representations:

;;move: (token . node)

(defmacro psgml-make-move (token node)
  (` (cons (, token) (, node))))

(defmacro psgml-move-token (x)
  (` (car (, x))))

(defmacro psgml-move-dest (x)
  (` (cdr (, x))))

;; set of moves: list of moves

(defmacro psgml-add-move-to-set (token node set)
  (`(cons (cons (, token) (, node)) (, set))))

(defmacro psgml-moves-lookup (token set)
  (` (assq (, token) (, set))))

;; normal-state: ('normal-state opts . reqs)

(defsubst psgml-make-state ()
  (cons 'normal-state (cons nil nil)))

(defmacro psgml-normal-state-p (s)
  (` (eq (car (, s)) 'normal-state)))

(defmacro psgml-state-opts (s)
  (` (cadr (, s))))

(defmacro psgml-state-reqs (s)
  (` (cddr (, s))))

(defmacro psgml-state-final-p (s)
  (`(null (psgml-state-reqs (, s)))))

;; adding moves
;; *** Should these functions check for ambiguity?
;; What if adding a optional move for a token that has a
;;  required move?
;; What about the other way?

(defsubst psgml-add-opt-move (s token dest)
  (or (psgml-moves-lookup token (psgml-state-opts s))
      (setf (psgml-state-opts s)
	    (psgml-add-move-to-set token dest (psgml-state-opts s)))))

(defsubst psgml-add-req-move (s token dest)
  (or (psgml-moves-lookup token (psgml-state-reqs s))
      (setf (psgml-state-reqs s)
	    (psgml-add-move-to-set token dest (psgml-state-reqs s)))))

(defsubst psgml-make-primitive-content-token (token)
  (let ((s1 (psgml-make-state))
	(s2 (psgml-make-state)))
    (psgml-add-req-move s1 token s2)
    s1))

;;and-state: (state next . dfas)

(defsubst psgml-make-and-state (state dfas next)
  (cons state (cons next dfas)))

(defsubst psgml-step-and-state (state and-state)
  (cons state (cdr and-state)))

(defsubst psgml-and-state-substate (s)
  (car s))

(defsubst psgml-and-state-dfas (s)
  (cddr s))

(defsubst psgml-and-state-next (s)
  (cadr s))


;;and-node:  (next . dfas)

(defsubst psgml-make-and-node (dfas next)
  (cons next dfas))

(defmacro psgml-and-node-next (n)
  (` (car (, n))))

(defmacro psgml-and-node-dfas (n)
  (` (cdr (, n))))


;;; Using states

(defsubst psgml-final (state)
  (if (psgml-normal-state-p state)
      (psgml-state-final-p state)
    (psgml-final-and state)))

(defun psgml-final-and (state)
  (and (psgml-final (psgml-and-state-substate state))
       (loop for s in (psgml-and-state-dfas state)
	     always (psgml-state-final-p s))
       (psgml-state-final-p (psgml-and-state-next state))))


;; get-move: State x Token --> State|nil

(defsubst psgml-get-move (state token)
  "Return a new state or nil, after traversing TOKEN from STATE."
  (cond
   ((symbolp state) nil)                ;if EMPTY slips thru...
   ((psgml-normal-state-p state)
    (let ((c (or (psgml-moves-lookup token (psgml-state-opts state))
		 (psgml-moves-lookup token (psgml-state-reqs state)))))
      (if c
	  (let ((dest (psgml-move-dest c)))
	    (if (psgml-normal-state-p dest)
		dest
	      ;; dest is a and-node
	      (psgml-next-sub-and (psgml-and-node-dfas dest)
				 token
				 (psgml-and-node-next dest)))))))
   (t					;state is a and-state
    (psgml-get-and-move state token))))

(defun psgml-get-and-move (state token)
  ;; state is a and-state
  (let ((m (psgml-get-move (psgml-and-state-substate state) token)))
    (cond (m (cons m (cdr state)))
	  ((psgml-final (psgml-and-state-substate state))
	   (psgml-next-sub-and (psgml-and-state-dfas state)
			      token
			      (psgml-and-state-next state))))))

(defun psgml-next-sub-and (dfas token next)
  "Compute the next state, choosing from DFAS and moving by TOKEN.
If this is not possible, but all DFAS are final, move by TOKEN in NEXT."
  (let ((allfinal t)
	(l dfas)
	(res nil)
	s1 s2)
    (while (and l (not res))
      (setq s1 (car l)
	    allfinal (and allfinal (psgml-state-final-p s1))
	    s2 (psgml-get-move s1 token)
	    res (and s2 (psgml-make-and-state s2 (remq s1 dfas) next))
	    l (cdr l)))
    (cond (res)
	  (allfinal (psgml-get-move next token)))))

(defsubst psgml-tokens-of-moves (moves)
  (mapcar (function (lambda (m) (psgml-move-token m)))
	  moves))

(defun psgml-required-tokens (state)
  (if (psgml-normal-state-p state)
      (psgml-tokens-of-moves (psgml-state-reqs state))
    (or (psgml-required-tokens (psgml-and-state-substate state))
	(loop for s in (psgml-and-state-dfas state)
	      nconc (psgml-tokens-of-moves (psgml-state-reqs s)))
	(psgml-tokens-of-moves (psgml-state-reqs (psgml-and-state-next state))))))

(defun psgml-optional-tokens (state)
  (if (psgml-normal-state-p state)
      (psgml-tokens-of-moves (psgml-state-opts state))
    (nconc
     (psgml-optional-tokens (psgml-and-state-substate state))
     (if (psgml-final (psgml-and-state-substate state))
	 (loop for s in (psgml-and-state-dfas state)
	       nconc (psgml-tokens-of-moves (psgml-state-opts s))))
     (if (loop for s in (psgml-and-state-dfas state)
	       always (psgml-state-final-p s))
	 (psgml-tokens-of-moves
	  (psgml-state-opts (psgml-and-state-next state)))))))


;;;; Attribute Types

;;; Basic Types
;; name = string	attribute names are lisp strings
;; attval = string	attribute values are lisp strings

;;; Attribute Declaration Type
;; attdecl = <name, declared-value, default-value>

;; This is the result of the ATTLIST declarations in the DTD.
;; All attribute declarations for an element is the elements
;; attlist.

;;; Attribute Declaration Operations
;; psgml-make-attdecl: name declared-value default-value -> attdecl
;; psgml-attdecl-name: attdecl -> name
;; psgml-attdecl-declared-value: attdecl -> declared-value
;; psgml-attdecl-default-value: attdecl -> default-value

;;; Attribute Declaration List Type
;; attlist = attdecl*

;;; Attribute Declaration List Operations
;; psgml-lookup-attdecl: name x attlist -> attdecl

;;; Declared Value Type
;; declared-value = (token-group | notation | simpel)
;; token-group = nametoken+
;; notation = nametoken+
;; simple = symbol		lisp symbol corresponding to SGML type

;;; Declared Value Operations
;; psgml-declared-value-token-group: declared-value -> list of symbols
;; psgml-declared-value-notation: declared-value -> list of symbols
;; (empty list if not token-group/notation)

;;; Default Value Type
;; default-value = (required | implied | conref | specified )
;; implied, conref = constant symbol
;; specified = (fixed | normal)
;; fixed, normal = attval

;;; Default Value Operations
;; psgml-default-value-attval: default-value -> (attval | nil)
;; psgml-default-value-type-p: type x default-value -> cond

;;; Attribute Specification Type
;; attspec = <name, attval>

;; This is the result of parsing an attribute specification.

;; psgml-make-attspec: name x attval -> attspec
;; psgml-attspec-name: attspec -> name
;; psgml-attspec-attval: attspec -> attval


;;; Attribute Specification List Type
;; asl = attspec*

;; aka. attribute value list


;;; Code

;;; attdecl representation = (name declared-value default-value)

(defun psgml-make-attdecl (name dcl-value default-value)
  (list name dcl-value default-value))

(defun psgml-attdecl-name (attdecl)
  (car attdecl))

(defun psgml-attdecl-declared-value (attdecl)
  "The declared value of ATTDECL.
It may be a symbol or (name-token-group (NAME1 ... NAMEn))
or (notation  (NOT1 ... NOTn))"
  (cadr attdecl))

(defun psgml-attdecl-default-value (attdecl)
  "The default value of ATTDECL.
The default value is either a symbol (REQUIRED | IMPLIED | CURRENT |
CONREF) or a list with first element nil or symbol `FIXED' and second
element the value."
  (caddr attdecl))


;;; attlist representation = (attspec*)

(defun psgml-lookup-attdecl (name attlist)
  "Return the attribute declaration for NAME in ATTLIST."
  (assoc name attlist))

(defun psgml-attribute-with-declared-value (attlist declared-value)
  "Find the first attribute in ATTLIST that has DECLARED-VALUE."
  (let ((found nil))
    (while (and attlist (not found))
      (when (equal declared-value
		   (psgml-attdecl-declared-value (car attlist)))
	(setq found (car attlist)))
      (setq attlist (cdr attlist)))
    found))


;;; declared-value representation
;; token-group = (name-token (symbol+))
;; notation = (notation (symbol+))
;; simple = symbol		lisp symbol correspoinding to PSGML type

(defun psgml-make-declared-value (type &optional names)
  "Make a declared-value of TYPE.
TYPE should be a symbol.  If TYPE is name-token-group or notation
NAMES should be a list of symbols."
  (if (consp names)
      (list type names)
    type))

(defun psgml-declared-value-token-group (declared-value)
  "Return the name token group for the DECLARED-VALUE.
This applies to name token groups.  For other declared values nil is
returned."
  (and (consp declared-value)
       (eq 'name-token-group (car declared-value))
       (cadr declared-value)))

(defun psgml-declared-value-notation (declared-value)
  "Return the list of notation names for the DECLARED-VALUE.
This applies to notation declared value.  For other declared values
nil is returned."
  (and (consp declared-value)
       (eq 'NOTATION (car declared-value))
       (cadr declared-value)))

;;; default-value representation = symbol | ((nil | 'fixed) attval)

(defun psgml-make-default-value (type &optional attval)
  (if attval
      (list type attval)
    type))

(defun psgml-default-value-attval (default-value)
  "Return the actual default value of the declared DEFAULT-VALUE.
The actual value is a string. Return nil if no actual value."
  (and (consp default-value)
       (cadr default-value)))

(defun psgml-default-value-type-p (type default-value)
  "Return true if DEFAULT-VALUE is of TYPE.
Where TYPE is a symbol, one of REQUIRED, IMPLIED, CONREF, or FIXED."
  (or (eq type default-value)
      (and (consp default-value)
	   (eq type (car default-value)))))


;;; attspec representation = (symbol . string)

(defun psgml-make-attspec (name attval)
  "Create an attspec from NAME and ATTVAL.
Special case, if ATTVAL is nil this is an implied attribute."
  (cons name attval))

;; psgml-attspec-name: attspec -> name
(defun psgml-attspec-name (attspec)
  (car attspec))

;; psgml-attspec-attval: attspec -> attval
(defun psgml-attspec-attval (attspec)
  "Return the value of attribute specification ATTSPEC.
If ATTSPEC is nil, nil is returned."
  (cdr attspec))

;;; asl representaion = (attspec*)

(defun psgml-lookup-attspec (name asl)
  (assoc name asl))


;;;; Element content types

;; The content of an element is defined as
;;	 (125 declared content | 126 content model),
;; 125  declared content = "CDATA" | "RCDATA" | "EMPTY"
;; 126  content model    = (127 model group | "ANY"),
;;			 (65 ps+, 138 exceptions)?

;; I represent a model group with the first state of a corresponding finite
;; automaton (this is a cons).  Exceptions are handled separately.
;; The other content types are represented by symbols.

(defsubst psgml-model-group-p (model)
  (consp model))

(defconst psgml-cdata 'CDATA)
(defconst psgml-rcdata 'RCDATA)
(defconst psgml-empty 'EMPTY)
(defconst psgml-any 'ANY)


;;;; External identifier
;; extid = (pubid? sysid? dir)
;; Representation as (pubid  sysid . dir)
;; where pubid = nil | string
;;       sysid = nil | string
;;       dir   = string

(defun psgml-make-extid (pubid sysid &optional pubid-ok)
  (and psgml-pxml-p (not pubid-ok) pubid (not sysid)
    (psgml-error "XML requires a system ID after a public ID."))
  (cons pubid (cons sysid default-directory)))

(defun psgml-extid-pubid (extid)
  (car extid))

(defun psgml-extid-sysid (extid)
  (if (consp (cdr extid))
      (cadr extid)
    (cdr extid)))

(defun psgml-extid-dir (extid)
  "Directory where EXTID was declared"
  (if (consp (cdr extid))
      (cddr extid)
    nil))

(defun psgml-extid-expand (file extid)
  "Expand file name FILE in the context of EXTID."
  (let ((psgml-system-path (cons (psgml-extid-dir extid)
				psgml-system-path)))
    (or (psgml-extid-expand-2 file psgml-system-path)
	(expand-file-name file (psgml-extid-dir extid)))))

(defun psgml-extid-expand-2 (file directories)
  (cond ((null directories) nil)
	(t
	 (let ((f (expand-file-name file (car directories))))
	   (if (file-exists-p f)
	       f
	     (psgml-extid-expand-2 file (cdr directories)))))))



;;;; DTD

;; DTD = (doctype, eltypes, parameters, entities, shortmaps,
;;	 notations, dependencies, merged)
;; DTDsubset ~=~ DTD, but doctype is unused
;;
;; doctype = name
;; eltypes = oblist
;; parameters = entity*
;; entities = entity*
;; shortmaps = (name, shortmap)*
;; dependencies = file*
;; merged = Compiled-DTD?  where  Compiled-DTD = (file, DTD)

(defstruct (psgml-dtd
	    (:type vector)
	    (:constructor psgml-make-dtd  (doctype)))
  doctype				; STRING, name of doctype
  (eltypes				; OBLIST, element types defined
   (psgml-make-eltype-table))
  (parameters				; ALIST
   (psgml-make-entity-table))
  (entities				; ALIST
   (psgml-make-entity-table))
  (shortmaps				; ALIST
   (psgml-make-shortref-table))
  (notations				; ??
   nil)
  (dependencies				; LIST
   nil)
  (merged				; (file . DTD)
   nil)
  (undef-entities			; LIST of entity names
   nil))


;;;; Element type objects

;; An element type object contains the information about an element type
;; obtained from parsing the DTD.

;; An element type object is represented by a symbol in a special oblist.
;; A table of element type objects is represented by a oblist.


;;; Element type objects

(defun psgml-eltype-name (et)
  (symbol-name et))

(define-compiler-macro psgml-eltype-name (et)
  (`(symbol-name (, et))))

(defun psgml-eltype-defined (et)
  (fboundp et))

(defun psgml-eltype-token (et)
  "Return a token for the element type"
  et)

(define-compiler-macro psgml-eltype-token (et)
  et)

(defun psgml-token-eltype (token)
  "Return the element type corresponding to TOKEN."
  token)

(define-compiler-macro psgml-token-eltype (token)
  token)

(defmacro psgml-prop-fields (&rest names)
  (cons
   'progn
   (loop for n in names collect
	 (`(defmacro (, (intern (format "psgml-eltype-%s" n))) (et)
	     (list 'get et ''(, n)))))))

(psgml-prop-fields
 ;;flags			; optional tags and mixed
					; (perhaps in value field)
 ;;model					; Content type
					; (perhaps in function field)
 attlist				; List of defined attributes
 includes				; List of included elements
 excludes				; List of excluded elements
 shortmap				; Associated shortref map
					; nil if none and 'empty if #empty
 )

(defmacro psgml-eltype-flags (et)
  (` (symbol-value (, et))))

(defun psgml-eltype-model (et)
  (if (fboundp et)
      (symbol-function et)
    psgml-any))

(defsetf psgml-eltype-model fset)


(defun psgml-eltype-stag-optional (et)
  (oddp (psgml-eltype-flags et)))

(defun psgml-eltype-etag-optional (et)
  (/= 0 (logand 2 (psgml-eltype-flags et))))

(defun psgml-eltype-mixed (et)
  (< 3 (psgml-eltype-flags et)))
(define-compiler-macro psgml-eltype-mixed (et)
  (`(< 3 (psgml-eltype-flags (, et)))))

(defsetf psgml-eltype-stag-optional (et) (f)
  (list 'psgml-set-eltype-flag et 1 f))
(defsetf psgml-eltype-etag-optional (et) (f)
  (list 'psgml-set-eltype-flag et 2 f))
(defsetf psgml-eltype-mixed (et) (f)
  (list 'psgml-set-eltype-flag et 4 f))

(defun psgml-set-eltype-flag (et mask f)
  (setf (psgml-eltype-flags et)
	(logior (logand (if (boundp et)
			    (psgml-eltype-flags et)
			  0)
			(lognot mask))
	       (if f mask 0))))

(defun psgml-maybe-put (sym prop val)
  (when val (put sym prop val)))

(defsetf psgml-eltype-includes (et) (l)
  (list 'psgml-maybe-put et ''includes l))

(defsetf psgml-eltype-excludes (et) (l)
  (list 'psgml-maybe-put et ''excludes l))

(defmacro psgml-eltype-appdata (et prop)
  "Get application data from element type ET with name PROP.
PROP should be a symbol, reserved names are: flags, model, attlist,
includes, excludes, conref-regexp, mixed, stag-optional, etag-optional."
  (` (get (, et) (, prop))))

(defun psgml-eltype-all-miscdata (et)
  (loop for p on (symbol-plist et) by (function cddr)
	unless (memq (car p) '(model flags includes excludes))
	nconc (list (car p) (cadr p))))

(defun psgml-eltype-set-all-miscdata (et miscdata)
  (setf (symbol-plist et)
	(nconc (symbol-plist et) miscdata)))

(defun psgml-make-eltype (name)
  (let ((et (make-symbol name)))
    (setf (psgml-eltype-flags et) 0)
    et))


;;; Element type tables

(defun psgml-make-eltype-table ()
  "Make an empty table of element types."
  (make-vector 73 0))

(defun psgml-eltype-table-empty (eltype-table)
  (loop for x across eltype-table always (eq x 0)))

(defun psgml-merge-eltypes (eltypes1 eltypes2)
  "Return the merge of two element type tables ELTYPES1 and ELTYPES2.
This may change ELTYPES1, ELTYPES2 is unchanged. Returns the new table."
  (if (psgml-eltype-table-empty eltypes1)
      eltypes2
    (progn
      (mapatoms
       (function (lambda (sym)
		   (let ((et (intern (symbol-name sym) eltypes1)))
		     (unless (fboundp et) ; not yet defined by <!element
		       (when (fboundp sym)
			 (fset et (symbol-function sym)))
		       (when (boundp sym)
			 (set et (symbol-value sym))))
		     (setf (symbol-plist et)
			   (nconc (symbol-plist et)
				  (copy-list (symbol-plist sym)))))))
       eltypes2)
      eltypes1)))

(defun psgml-lookup-eltype (name &optional dtd)
  "Lookup the element defintion for NAME (string)."
  (intern name (psgml-dtd-eltypes (or dtd psgml-dtd-info))))

(defun psgml-eltype-completion-table (eltypes)
  "Make a completion table from a list, ELTYPES, of element types."
  (loop for et in eltypes as name = (psgml-eltype-name et)
	if (boundp et)
	collect (cons name name)))

(defun psgml-read-element-type (prompt dtd &optional default)
  "Read an element type name.
PROMPT is displayed as a prompt and DTD should be the dtd to get the
element types from. Optional argument DEFAULT (string) will be used as
a default for the element type name."
  (let ((name
	 (let ((completion-ignore-case psgml-namecase-general))
	   (completing-read prompt
			    (psgml-dtd-eltypes dtd)
			    (function fboundp)
			    t
			    nil
			    nil))))
    (when (equal name "")
      (setq name (or default (error "Aborted"))))
    (psgml-lookup-eltype name dtd)))

(defun psgml-map-eltypes (fn dtd &optional collect all)
  (let ((*res* nil))
    (mapatoms
     (cond ((and collect all)
	    (function (lambda (a) (push (funcall fn a) *res*))))
	   (collect
	    (function (lambda (a) (when (boundp a)
				    (push (funcall fn a) *res*)))))
	   (all
	    (function (lambda (a) (funcall fn a))))
	   (t
	    (function (lambda (a) (when (boundp a) (funcall fn a))))))
     (psgml-dtd-eltypes dtd))
    (nreverse *res*)))

;;;; Load a saved dtd

;;; Wing addition
(defmacro psgml-char-int (ch)
  (if (fboundp 'char-int)
      (` (char-int (, ch)))
    ch))

(defsubst psgml-read-octet ()
  ;; Wing change
  (prog1 (psgml-char-int (following-char))
    (forward-char)))

(defsubst psgml-read-peek ()
  (psgml-char-int (following-char)))

(defsubst psgml-read-number ()
  "Read a number.
A number is 1: an octet [0--psgml-max-singel-octet-number]
or 2: two octets (n,m) interpreted as  (n-t-1)*256+m+t."
  (if (> (psgml-read-peek) psgml-max-single-octet-number)
      (+ (* (- (psgml-read-octet) (eval-when-compile
				   (1+ psgml-max-single-octet-number)))
	    256)
	 (psgml-read-octet)
	 psgml-max-single-octet-number)
    (psgml-read-octet)))


(defun psgml-read-sexp ()
  (prog1
      (let ((standard-input (current-buffer)))
	(read))
    (skip-chars-forward " \t")
    (forward-char 1)))

(defsubst psgml-read-token ()
  (aref psgml-read-token-vector (psgml-read-number)))

(defsubst psgml-read-node-ref ()
  (aref psgml-read-nodes (psgml-read-octet)))

(defun psgml-read-model-seq ()
  (loop repeat (psgml-read-number) collect (psgml-read-model)))

(defun psgml-read-token-seq ()
  (loop repeat (psgml-read-number) collect (psgml-read-token)))

(defun psgml-read-moves ()
  (loop repeat (psgml-read-number)
	collect (psgml-make-move (psgml-read-token) (psgml-read-node-ref))))

(defun psgml-read-model ()
  (let* ((n (psgml-read-number))
	 (psgml-read-nodes (make-vector n nil)))
    (loop for i below n do (aset psgml-read-nodes i (psgml-make-state)))
    (loop for e across psgml-read-nodes do
	  (cond ((eq 255 (psgml-read-peek))	; a and-node
		 (psgml-read-octet)		; skip
		 (setf (psgml-and-node-next e) (psgml-read-node-ref))
		 (setf (psgml-and-node-dfas e) (psgml-read-model-seq)))
		(t			; a normal-state
		 (setf (psgml-state-opts e) (psgml-read-moves))
		 (setf (psgml-state-reqs e) (psgml-read-moves)))))
    (aref psgml-read-nodes 0)))

(defun psgml-read-content ()
  (let ((c (psgml-read-octet)))
    (cond ((eq c 0) psgml-cdata)
	  ((eq c 1) psgml-rcdata)
	  ((eq c 2) psgml-empty)
	  ((eq c 3) psgml-any)
	  ((eq c 4) nil)
	  ((eq c 128)
	   (psgml-read-model)))))

(defun psgml-read-decode-flag (flag mask)
  (not (zerop (logand flag mask))))

(defun psgml-read-element (et)
  (psgml-eltype-set-all-miscdata et (psgml-read-sexp))
  (let ((flags (psgml-read-octet)))
    (unless (= flags 128)
      (setf (psgml-eltype-flags et) flags
	    (psgml-eltype-model et) (psgml-read-content)
	    (psgml-eltype-includes et) (psgml-read-token-seq)
	    (psgml-eltype-excludes et) (psgml-read-token-seq)))))

(defun psgml-read-dtd ()
  "Decode the saved DTD in current buffer, return the DTD."
  (let ((gc-cons-threshold (max gc-cons-threshold 500000))
	(file-version (psgml-read-sexp))
	dtd)
    (cond
     ((equal file-version '(psgml-saved-dtd-version 7))
      (setq dtd (psgml-bdtd-read-dtd)))
     ;; Something else
     (t
      (error "Unknown file format for saved DTD: %s" file-version)))
    dtd))

(defun psgml-load-dtd (file)
  "Load a saved DTD from FILE."
  (interactive
   (let ((tem (expand-file-name
	       (or psgml-default-dtd-file
		   (psgml-default-dtd-file)))))
     (list (read-file-name "Load DTD from: "
			   (file-name-directory tem)
			   tem
			   t
			   (file-name-nondirectory tem)))))
  (setq psgml-loaded-dtd nil)		; Allow reloading of DTD
  ;; Search for 'file' on the psgml-system-path [ndw]
  (let ((real-file (car (mapcan (function
				 (lambda (dir)
				   (let ((f (expand-file-name file dir)))
				     (if (file-exists-p f)
					 (list f)))))
				(cons "." psgml-system-path)))))
    (or real-file
	(error "Saved DTD file %s not found" file))
    (let ((cb (current-buffer))
	  (tem nil)
	  (dtd nil)
	  (l (buffer-list))
	  (find-file-type		; Allways binary
	   (function (lambda (fname) 1))))
      ;; Search loaded buffer for a already loaded DTD
      (while (and l (null tem))
	(set-buffer (car l))
	(if (equal psgml-loaded-dtd real-file)
	    (setq tem (current-buffer)))
	(setq l (cdr l)))
      (cond
       (tem				; loaded DTD found
	(setq dtd (psgml-pstate-dtd psgml-buffer-parse-state)))
       (t				; load DTD from file
	(set-buffer cb)
	(psgml-push-to-entity real-file)
	(message "Loading DTD from %s..." real-file)
	(setq dtd (psgml-read-dtd))
	(message "Loading DTD from %s...done" real-file)
	(psgml-pop-entity)))
      (set-buffer cb)
      (psgml-set-initial-state dtd)
      (setq psgml-default-dtd-file file)
      (setq psgml-loaded-dtd real-file))))

;;;; Binary coded DTD module
;;; Works on the binary coded compiled DTD (bdtd)

;;; bdtd-load: cfile dtdfile ents -> bdtd
;;; bdtd-merge: bdtd dtd -> dtd?
;;; bdtd-read-dtd: bdtd -> dtd

;;; Implement by letting bdtd be implicitly the current buffer and
;;; dtd implicit in psgml-dtd-info.

(defun psgml-bdtd-load (cfile dtdfile ents)
  "Load the compiled dtd from CFILE into the current buffer.
If this file does not exists, is of an old version or out of date, a
new compiled dtd will be creted from file DTDFILE and parameter entity
settings in ENTS."
  ;;(Assume the current buffer is a scratch buffer and is empty)
  (psgml-debug "Trying to load compiled DTD from %s..." cfile)
  (or (and (file-readable-p cfile)
	   (let ((find-file-type	; Allways binary
		  (function (lambda (fname) 1))))
	     ;; fifth arg to insert-file-contents is not available in early
	     ;; v19.
	     (insert-file-contents cfile nil nil nil))
	   (equal '(psgml-saved-dtd-version 7) (psgml-read-sexp))
	   (or (psgml-up-to-date-p cfile (psgml-read-sexp))
	       (if (eq 'ask psgml-recompile-out-of-date-cdtd)
		   (not (y-or-n-p
			 "Compiled DTD is out of date, recompile? "))
		 (not psgml-recompile-out-of-date-cdtd))))
      (psgml-compile-dtd dtdfile cfile ents)))

(defun psgml-up-to-date-p (file dependencies)
  "Check if FILE is newer than all files in the list DEPENDENCIES.
If DEPENDENCIES contains the symbol `t', FILE is not considered newer."
  (if (memq t dependencies)
      nil
    (loop for f in dependencies
	  always (file-newer-than-file-p file f))))

(defun psgml-compile-dtd (dtd-file to-file ents)
  "Construct a binary code compiled dtd from DTD-FILE and write it to TO-FILE.
The dtd will be constructed with the parameter entities set according
to ENTS. The bdtd will be left in the current buffer.  The current
buffer is assumend to be empty to start with."
  (psgml-log-message "Recompiling DTD file %s..." dtd-file)
  (let* ((psgml-dtd-info (psgml-make-dtd nil))
	 (parameters (psgml-dtd-parameters psgml-dtd-info))
	 (psgml-parsing-dtd t))
    (push dtd-file
	  (psgml-dtd-dependencies psgml-dtd-info))
    (loop for (name . val) in ents
	  do (psgml-entity-declare name parameters 'text val))
    (psgml-push-to-entity dtd-file)
    (psgml-check-dtd-subset)
    (psgml-pop-entity)
    (erase-buffer)
    (psgml-write-dtd psgml-dtd-info to-file)
    t))

(defun psgml-check-entities (params1 params2)
  "Check that PARAMS1 is compatible with PARAMS2."
  (block check-entities
    (psgml-map-entities
     (function (lambda (entity)
		 (let ((other
			(psgml-lookup-entity (psgml-entity-name entity)
					    params2)))
		   (unless (or (null other)
			       (equal entity other))
		     (psgml-log-message
		      "Parameter %s in complied DTD has wrong value;\
 is '%s' should be '%s'"
		      (psgml-entity-name entity)
		      (psgml-entity-text other)
		      (psgml-entity-text entity))
		     (return-from check-entities nil)))))
     params1)
    t))

(defun psgml-bdtd-merge ()
  "Merge the binary coded dtd in the current buffer with the current dtd.
The current dtd is the variable psgml-dtd-info.  Return t if mereged
was successfull or nil if failed."
  (goto-char (point-min))
  (psgml-read-sexp)			; skip filev
  (let ((dependencies (psgml-read-sexp))
	(parameters (psgml-read-sexp))
	(gc-cons-threshold (max gc-cons-threshold 500000))
	temp)
    ;; Check comaptibility of parameters
    (and (psgml-check-entities (psgml-dtd-parameters psgml-dtd-info)
			      parameters)
	 (progn
	   ;; Do the merger
	   (psgml-message "Reading compiled DTD...")
	   (psgml-merge-entity-tables (psgml-dtd-parameters psgml-dtd-info)
				     parameters)
	   (setf (psgml-dtd-dependencies psgml-dtd-info)
		 (nconc (psgml-dtd-dependencies psgml-dtd-info)
			dependencies))
	   ;; Doctype
	   (setq temp (psgml-read-sexp))
	   (when (and temp (null (psgml-dtd-doctype psgml-dtd-info)))
	     (setf (psgml-dtd-doctype psgml-dtd-info) temp))

	   ;; Element type names -- read and create token vector
	   (setq temp (psgml-read-number)) ; # eltypes
	   (setq psgml-read-token-vector (make-vector (1+ temp) nil))
	   (aset psgml-read-token-vector 0 psgml-pcdata-token)
	   (loop for i from 1 to temp do
		 (aset psgml-read-token-vector i
		       (psgml-lookup-eltype (psgml-read-sexp))))
	   ;; Element type descriptions
	   (loop for i from 1 to (psgml-read-number) do
		 (psgml-read-element (aref psgml-read-token-vector i)))
	   (psgml-merge-entity-tables (psgml-dtd-entities psgml-dtd-info)
				     (psgml-read-sexp))
	   (psgml-merge-shortmaps (psgml-dtd-shortmaps psgml-dtd-info)
				 (psgml-read-sexp))
	   (setf (psgml-dtd-notations psgml-dtd-info) (psgml-read-sexp))
	   t))))

(defun psgml-bdtd-read-dtd ()
  "Create and return a dtd from the binary coded dtd in the current buffer."
  (let ((psgml-dtd-info (psgml-make-dtd nil)))
    (psgml-bdtd-merge)
    psgml-dtd-info))

;;;; Set markup type

(defsubst psgml-set-markup-type (type)
  "Set the type of the markup parsed to TYPE.
The markup starts at position given by variable psgml-markup-start and
ends at point."
  (when (and psgml-set-face
	     (null psgml-current-eref))
    (psgml-set-face-for psgml-markup-start (point) type))
  (setq psgml-markup-type type))


;;;; Parsing delimiters

(eval-and-compile
  (defconst psgml-delimiters
    '("AND"   "&"
      "COM"   "--"
      "CRO"   "&#"
      "DSC"   "]"
      "DSO"   "["
      "DTGC"  "]"
      "DTGO"  "["
      "ERO"   "&"
      "ETAGO" "</"
      "GRPC"  ")"
      "GRPO"  "("
      "LIT"   "\""
      "LITA"  "'"
      "MDC"   ">"
      "MDO"   "<!"
      "MINUS" "-"
      "MSC"   "]]"
      "NESTC" "/"
      "NET"   "/"
      "OPT"   "?"
      "OR"    "|"
      "PERO"  "%"
      "PIC"   ">"
      "PIO"   "<?"
      "PLUS"  "+"
      "REFC"  ";"
      "REP"   "*"
      "RNI"   "#"
      "SEQ"   ","
      "STAGO" "<"
      "TAGC"  ">"
      "VI"    "="
      ;; Some combinations
      "MS-START" "<!["			; MDO DSO
      "MS-END"   "]]>"			; MSC MDC
      ;; XML stuff
      "XML-ECOM"   "-->"		; end an XML comment
      "XML-PIC"    "?>"			; end an XML processing instruction
      "XML-SCOM"   "<!--"		; start an XML comment
      "XML-TAGCE"  "/>"			; end an XML empty element
      ;; Pseudo
      "NULL"  ""
      )))

(eval-and-compile
(defun psgml-get-delim-string (drole)
  (car (cdr (member drole psgml-delimiters)))))

(defmacro psgml-delim (drole)
  (if (stringp drole)
      (psgml-get-delim-string (upcase drole))
    `(psgml-get-delim-string ,drole)))


(defmacro psgml-is-delim (delim &optional context move offset)
  "Macro for matching delimiters.
Syntax: DELIM &optional CONTEXT MOVE
where DELIM is the delimiter name (string or symbol),
CONTEXT the contextual constraint, and
MOVE is `nil', `move' or `check'.

Test if the text following point in current buffer matches the SGML
delimiter DELIM.  Also check the characters after the delimiter for
CONTEXT.  Applicable values for CONTEXT is
`gi' -- name start or TAGC if SHORTTAG YES,
`com' -- if COM or MDC,
`nmstart' -- name start character,
`stagc' -- TAGC if SHORTTAG YES,
`digit' -- any Digit,
string -- delimiter with that name,
list -- any of the contextual constraints in the list."

  (or offset (setq offset 0))
  (setq delim (upcase (format "%s" delim)))
  (let ((ds (psgml-get-delim-string delim)))
    (assert ds)
    (cond ((eq context 'gi)
	   (setq context '(nmstart stagc)))
	  ((eq context 'com)
	   (setq context '("COM" "MDC")))
	  ((null context)
	   (setq context '(t)))
	  ((not (listp context))
	   (setq context (list context))))
    (`(if (and				; This and checks that characters
					; of the delimiter
	   (,@(loop for i from 0 below (length ds) collect
		    (` (eq (, (aref ds i))
			   (psgml-following-char (, (+ i offset)))))))
	   (or
	    (,@(loop
		for c in context collect ; context check
		(cond
		 ((eq c 'nmstart)	; name start character
		  (`(psgml-startnm-char
		     (or (psgml-following-char (, (length ds))) 0))))
		 ((eq c 'stagc)
		  (`(and psgml-current-shorttag
			 (psgml-is-delim "TAGC" nil nil (, (length ds))))))
		 ((eq c 'digit)
		  (`(memq (psgml-following-char (, (length ds)))
			  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
		 ((stringp c)
		  (`(psgml-is-delim (, c) nil nil (, (length ds)))))
		 ((eq c t))
		 (t (error "Context type: %s" c))))
	       )))

	  (progn			; Do operations if delimiter found
	    (,@ (if move (`((forward-char (, (length ds)))))))
	    (,@ (if (not (eq move 'check))
		    '(t))))
	(,@ (if (eq move 'check)
		(`((psgml-delimiter-parse-error (, delim))))))))))

(defmacro psgml-following-char (n)
  (cond ((zerop n)  '(following-char))
	((= n 1)    '(char-after (1+ (point))))
	(t          (` (char-after (+ (, n) (point)))))))

(defun psgml-delimiter-parse-error (delim)
  (psgml-parse-error "Delimiter %s (%s) expected"
		    delim (psgml-get-delim-string delim)))

(defmacro psgml-parse-delim (delim &optional context)
  (`(psgml-is-delim (, delim) (, context) move)))

(defmacro psgml-check-delim (delim &optional context)
  (`(psgml-is-delim (, delim) (, context) check)))

(defmacro psgml-skip-upto (delim)
  "Skip until the delimiter or first char of one of the delimiters.
If DELIM is a string/symbol this is should be a delimiter role.
Characters are skipped until the delimiter is recognized.
If DELIM is a list of delimiters, skip until a character that is first
in any of them."
  (cond
   ((consp delim)
    (list 'skip-chars-forward
	  (concat "^"
		  (loop for d in delim
			concat (let ((ds (member (upcase (format "%s" d))
						 psgml-delimiters)))
				 (assert ds)
				 (let ((s (substring (cadr ds) 0 1)))
				   (if (member s '("-" "\\"))
				       (concat "\\" s)
				     s)))))))
   (t
    (let ((ds (psgml-get-delim-string (upcase (format "%s" delim)))))
      (if (= 1 (length ds))
	  (list 'skip-chars-forward (concat "^" ds))
	(`(and (search-forward (, ds) nil t)
	       (backward-char (, (length ds))))))))))


;;(macroexpand '(psgml-is-delim mdo))
;;(macroexpand '(psgml-parse-delim mdo))
;;(macroexpand '(psgml-check-delim mdo))


;;;; General lexical functions
;;; Naming conventions
;;; psgml-parse-xx  try to parse xx, return nil if can't else return
;;;		   some propriate non-nil value.
;;;                Except: for name/nametoken parsing, return 0 if can't.
;;; psgml-check-xx  require xx, report error if can't parse.  Return
;;;                aproporiate value.

(defmacro psgml-parse-char (char)
  (` (cond ((eq (, char) (following-char))
	    (forward-char 1)
	    t))))

(defmacro psgml-parse-chars (char1 char2 &optional char3)
  "Parse two or three chars; return nil if can't"
  (if (null char3)
      (` (cond ((and (eq (, char1) (following-char))
		 (eq (, char2) (char-after (1+ (point)))))
	    (forward-char 2)
	    t)))
    (` (cond ((and (eq (, char1) (following-char))
		 (eq (, char2) (char-after (1+ (point))))
		 (eq (, char3) (char-after (1+ (1+ (point))))))
	    (forward-char 3)
	    t)))))

(defun psgml-check-char (char)
  (cond ((not (psgml-parse-char char))
	 (psgml-parse-error "Expecting %c" char))))

(defun psgml-parse-RE ()
  (or (psgml-parse-char ?\n)
      (psgml-parse-char ?\r)))

(defmacro psgml-startnm-char (c)
  (` (eq ?w (char-syntax (, c)))))

(defsubst psgml-startnm-char-next ()
  (and (not (eobp))
       (psgml-startnm-char (following-char))))

(defun psgml-name-char (c)
  (and c
       (or (psgml-startnm-char c)
	   (eq ?_ (char-syntax c)))))

(defun psgml-is-end-tag ()
  (psgml-is-delim "ETAGO" gi))

(defsubst psgml-is-enabled-net ()
  (and (psgml-is-delim "NET")
       psgml-current-shorttag
       (psgml-tree-net-enabled psgml-current-tree)))

(defun psgml-is-start-tag ()
  (psgml-is-delim "STAGO" gi))

(defsubst psgml-parse-s (&optional shortmap)
  (if shortmap
      (or (/= 0 (skip-chars-forward " "))
	  (/= 0 (skip-chars-forward "\t"))
	  (psgml-parse-char ?\n)
	  (psgml-parse-char ?\r))
    (/= 0 (skip-chars-forward " \t\n\r"))))

(defsubst psgml-parse-processing-instruction (&optional in-declaration)
  (if (psgml-parse-delim "PIO")
      (psgml-do-processing-instruction in-declaration)))

(defun psgml-parse-set-appflag (flagsym)
  (loop for name = (psgml-parse-name)
	while name
	for et = (psgml-lookup-eltype name)
	do (setf (psgml-eltype-appdata et flagsym) t)
	(message "Defining element %s as %s" name flagsym)
	(psgml-skip-cs)))

(defun psgml-do-processing-instruction (in-declaration)
  (let ((start (point)))
    (when (and (eq ?P (following-char))
	       (looking-at "PSGML +\\(\\sw+\\) *"))
      (let* ((command (downcase (match-string 1)))
	     (flag-command (assoc command
				  '(("nofill"      . nofill)
				    ("breakafter"  . break-after-stag)
				    ("breakbefore" . break-before-stag)
				    ("structure"   . structure)))))
	(goto-char (match-end 0))
	(cond (flag-command
	       (psgml-parse-set-appflag (cdr flag-command)))
	      (t
	       (psgml-log-warning "Unknown processing instruction for PSGML: %s"
				 command)))))
    (if psgml-pxml-p
	(psgml-skip-upto "XML-PIC")
      (psgml-skip-upto "PIC"))
    (when psgml-pi-function
      (funcall psgml-pi-function
	       (buffer-substring-no-properties start (point)))))
  (if psgml-pxml-p
      (psgml-check-delim "XML-PIC")
    (psgml-check-delim "PIC"))
  (unless in-declaration
    (psgml-set-markup-type 'pi))
  t)


(defun psgml-general-case (string)
  (if psgml-current-namecase-general
      (upcase string)
    string))

(defmacro psgml-entity-case (string)   string)


;;[lenst/1998-03-09 19:52:08]  Perhaps not the right place
(defun psgml-general-insert-case (text)
  (if psgml-namecase-general
      (case psgml-general-insert-case
	(upper (upcase text))
	(lower (downcase text))
	(t text))
    text))

(defun psgml-entity-insert-case  (text)
  (if psgml-namecase-entity
      (case psgml-entity-insert-case
	(upper (upcase text))
	(lower (downcase text))
	(t text))
    text))


(defun psgml-parse-name (&optional entity-name)
  (if (psgml-startnm-char-next)
      (let ((name (buffer-substring-no-properties
		   (point)
		   (progn (skip-syntax-forward "w_")
			  (point)))))
	(if entity-name
	    (psgml-entity-case name)
	  (psgml-general-case name)))))

(define-compiler-macro psgml-parse-name (&whole form &optional entity-name)
  (cond
   ((memq entity-name '(nil t))
    (` (if (psgml-startnm-char-next)
	   ((, (if entity-name 'psgml-entity-case 'psgml-general-case))
	    (buffer-substring-no-properties (point)
					    (progn (skip-syntax-forward "w_")
						   (point)))))))
   (t
    form)))


(defun psgml-check-name (&optional entity-name)
  (or (psgml-parse-name entity-name)
      (psgml-parse-error "Name expected")))

(define-compiler-macro psgml-check-name (&optional entity-name)
  (`(or (psgml-parse-name (, entity-name))
	(psgml-parse-error "Name expected"))))


(defun psgml-parse-nametoken (&optional entity-name)
  "Parses a name token and returns a string or nil if no nametoken."
  (if (psgml-name-char (following-char))
      (let ((name (buffer-substring-no-properties
		   (point)
		   (progn (skip-syntax-forward "w_")
			  (point)))))
	(if entity-name
	    (psgml-entity-case name)
	  (psgml-general-case name)))))

(defun psgml-check-nametoken ()
  (or (psgml-parse-nametoken)
      (psgml-parse-error "Name token expected")))

(defsubst psgml-parse-general-entity-ref ()
  (if (psgml-parse-delim "ERO" nmstart)
      (psgml-do-general-entity-ref)))

(defun psgml-do-general-entity-ref ()
  (psgml-do-entity-ref
   (prog1 (psgml-parse-name t)
     (or (psgml-parse-delim "REFC")
	 (psgml-parse-RE))
     (psgml-set-markup-type 'entity)))
  t)

(defun psgml-do-entity-ref (name)
  (let ((entity
	 (psgml-lookup-entity name
			     (psgml-dtd-entities psgml-dtd-info))))
    (cond ((and (null entity)
		psgml-warn-about-undefined-entities)
	   (psgml-log-warning
	    "Undefined entity %s" name))
	  ((psgml-entity-data-p entity)
	   (when psgml-pxml-p
	     (psgml-error
	      "XML forbids data-entity references in data or DTD (%s)."
	      name))
	   (when psgml-signal-data-function
	     (funcall psgml-signal-data-function))
	   (cond
	    (psgml-entity-function
	     (funcall psgml-entity-function entity))
	    (psgml-data-function
	     (psgml-push-to-entity entity psgml-markup-start)
	     (funcall psgml-data-function (buffer-string))
	     (psgml-pop-entity))))
	  (t
	   (psgml-push-to-entity entity psgml-markup-start)))))

(defsubst psgml-parse-parameter-entity-ref ()
  "Parse and push to a parameter entity, return nil if no ref here."
  ;;(setq psgml-markup-start (point))
  (if (psgml-parse-delim "PERO" nmstart)
      (psgml-do-parameter-entity-ref)))

(defun psgml-do-parameter-entity-ref ()
  (let* ((name (psgml-parse-name t))
	 (ent (psgml-lookup-entity name
				  (psgml-dtd-parameters psgml-dtd-info))))
	(or (psgml-parse-delim "REFC")
	    (psgml-parse-char ?\n))
	;;(psgml-set-markup-type 'param)
	(cond (ent
	       (psgml-push-to-entity ent psgml-markup-start 'param))
	      (t
	       (psgml-parse-warning
		"Undefined parameter entity %s" name)))
	t))

(defun psgml-parse-comment ()
  (if (psgml-parse-delim "COM")
      (progn
	(if psgml-pxml-p
	    (psgml-parse-warning "XML forbids nested comments."))
	(psgml-skip-upto "COM")
	(psgml-check-delim "COM")
	t)))

(defun psgml-parse-pxml-comment ()
  (if (psgml-parse-delim "XML-SCOM")
      (progn (psgml-skip-upto "XML-ECOM")
	     (psgml-check-delim "XML-ECOM")
	     (psgml-set-markup-type 'comment)
	     t)))

(defun psgml-skip-cs ()
  "Skip over the separator used in the catalog.
Return true if not at the end of the buffer."
  (while (or (psgml-parse-s)
	     (psgml-parse-comment)))
  (not (eobp)))

(defsubst psgml-skip-ps ()
  "Move point forward stopping before a char that isn't a parameter separator."
  (while
      (or (psgml-parse-s)
	  (if (eobp) (psgml-pop-entity))
	  (psgml-parse-parameter-entity-ref)
	  (psgml-parse-comment))))

(defsubst psgml-parse-ds ()
;71  ds   = 5 s | EE | 60+ parameter entity reference
;         | 91 comment declaration
;         | 44 processing instruction
;         | 93 marked section declaration ***
  (or (and (eobp) (psgml-pop-entity))	;EE
      (psgml-parse-s)			;5 s
      ;;(psgml-parse-comment-declaration)	;91 comment declaration
      (psgml-parse-parameter-entity-ref)
      (psgml-parse-processing-instruction 'in-declaration)))

(defun psgml-skip-ds ()
  (while (psgml-parse-ds)))

(defmacro psgml-parse-rni (&optional name)
  "Parse a RNI (#) return nil if none; with optional NAME,
a RNI must be followed by NAME."
  (cond
   (name
    (` (if (psgml-parse-delim "RNI")
	   (psgml-check-token (, name)))))
   (t '(psgml-parse-delim "RNI"))))

(defun psgml-check-token (name)
  (or (equal (psgml-check-case (psgml-check-name)) name)
      (psgml-parse-error "Reserved name not expected (expecting %s)"
			name)))

(defun psgml-check-case (name)
  "Check that NAME is in upper case.
If psgml-namecase-general is nil, then signal an error if the argument
is not already in upper case."
  ;; FIXME: Perhaps only warn and upcase
  (or psgml-current-namecase-general
      (equal name (upcase name))
      (psgml-parse-error "Uppercase keyword expected."))
  name)

(defun psgml-parse-literal ()
  "Parse a literal and return a string, if no literal return nil."
  (let (lita start value)
    (cond ((or (psgml-parse-delim "LIT")
	       (setq lita (psgml-parse-delim "LITA")))
	   (setq start (point))
	   (if lita
	       (psgml-skip-upto "LITA")
	     (psgml-skip-upto "LIT"))
	   (setq value (buffer-substring-no-properties start (point)))
	   (if lita
	       (psgml-check-delim "LITA")
	     (psgml-check-delim "LIT"))
	   value))))

(defun psgml-check-literal ()
  (or (psgml-parse-literal)
      (psgml-parse-error "A litteral expected")))

(defun psgml-parse-minimum-literal ()
  "Parse a quoted SGML string and return it, if no string return nil."
  (cond
   ((memq (following-char) '(?\" ?\'))
    (let* ((qchar (following-char))
	   (blanks " \t\r\n")
	   (qskip (format "^%s%c" blanks qchar))
	   (start (point))
	   (value			; accumulates the literal value
	    "")
	   (spaced ""))
      (forward-char 1)
      (skip-chars-forward blanks)
      (while (not (psgml-parse-char qchar))
	(cond ((eobp)
	       (goto-char start)
	       (psgml-parse-error "Unterminated literal"))
	      ((psgml-parse-s)
	       (setq spaced " "))
	      (t
	       (setq value
		     (concat value spaced
			     (buffer-substring-no-properties
			      (point)
			      (progn (skip-chars-forward qskip)
				     (point))))
		     spaced ""))))
      value))))

(defun psgml-check-minimum-literal ()
  (or (psgml-parse-minimum-literal)
      (psgml-parse-error "A minimum literal expected")))

(defun psgml-parse-external (&optional pubid-ok)
  "Leaves nil if no external id, or (pubid . sysid)"
  (psgml-skip-ps)
  (let* ((p (point))
	 (token (psgml-parse-nametoken)))
    (cond
     (token
      (psgml-skip-ps)
      (cond ((member (psgml-check-case token) '("PUBLIC" "SYSTEM"))
	     (let* ((pubid		; the public id
		     (if (string-equal token "PUBLIC")
			 (or (psgml-parse-minimum-literal)
			     (psgml-parse-error "Public identifier expected"))))
		    (sysid		; the system id
		     (progn (psgml-skip-ps)
			    (psgml-parse-literal))))
	       (psgml-make-extid pubid sysid pubid-ok)))
	    (t
	     (goto-char p)
	     nil))))))

(defun psgml-skip-tag ()
  (when (psgml-parse-char ?<)
    (psgml-parse-char ?/)
    (unless (search-forward-regexp
	       "\\([^\"'<>/]\\|\"[^\"]*\"\\|'[^']*'\\)*"
	       nil t)
      (psgml-error "Invalid tag"))
    (or (psgml-parse-char ?>)
	(psgml-parse-char ?/))))


;;;; Entity Manager

(defstruct (psgml-entity
	    (:type list)
	    (:constructor psgml-make-entity (name type text &optional notation)))
  name					; Name of entity (string)
  type					; Type of entity CDATA NDATA PI SDATA
  text					; string or external
  notation                              ; Notation of external entity or nil
  ;; Last cdr is undefined flag
  )

(defun psgml-entity-data-p (entity)
  "True if ENTITY is a data entity, that is not a text entity."
  (not (eq (psgml-entity-type entity) 'text)))

(defun psgml-entity-marked-undefined-p (entity)
  (cddddr entity))

(defsetf psgml-entity-marked-undefined-p (entity) (val)
  `(setf (cddddr ,entity) ,val))



;;; Entity tables
;; Represented by a cons-cell whose car is the default entity (or nil)
;; and whose cdr is as an association list.

(defun psgml-make-entity-table ()
  (list nil))

(defun psgml-lookup-entity (name entity-table)
  (or (assoc name (cdr entity-table))
      (car entity-table)))

(defun psgml-entity-declare (name entity-table type text &optional notation)
  "Declare an entity with name NAME in table ENTITY-TABLE.
TYPE should be the type of the entity (text|CDATA|NDATA|SDATA...).
TEXT is the text of the entity, a string or an external identifier.
NOTATION is the notation of an external entity, if present.
If NAME is nil, this defines the default entity."
  (cond
   (name
    (unless (psgml-lookup-entity name entity-table)
      (psgml-debug "Declare entity %s %s as %S" name type text)
      (nconc entity-table
	     (list (psgml-make-entity name type text notation)))))
   (t
    (unless (car entity-table)
      (psgml-debug "Declare default entity %s as %S" type text)
      (setcar entity-table (psgml-make-entity name type text))))))

(defun psgml-entity-completion-table (entity-table)
  "Make a completion table from the ENTITY-TABLE."
  (cdr entity-table))

(defun psgml-map-entities (fn entity-table &optional collect)
  (if collect
      (mapcar fn (cdr entity-table))
    (loop for e in (cdr entity-table) do (funcall fn e))))

(defun psgml-merge-entity-tables (tab1 tab2)
  "Merge entity table TAB2 into TAB1.  TAB1 is modified."
  (nconc tab1 (cdr tab2))
  (setcar tab1 (or (car tab1) (car tab2))))


(defun psgml-entity-insert-text (entity &optional ptype)
  "Insert the text of ENTITY.
PTYPE can be 'param if this is a parameter entity."
  (let ((text (psgml-entity-text entity)))
    (cond
     ((stringp text)
      (insert text))
     (t
      (psgml-insert-external-entity text
				   (or ptype
				       (psgml-entity-type entity))
				   (psgml-entity-name entity))))))

;;;; External identifyer resolve

(defun psgml-cache-catalog (file cache-var parser-fun
				&optional default-dir)
  "Return parsed catalog.
FILE is the file containing the catalog.  Maintains a cache of parsed
catalog files in variable CACHE-VAR. The parsing is done by function
PARSER-FUN that should parse the current buffer and return the parsed
repreaentation of the catalog."
  (setq file (expand-file-name file default-dir))
  (and
   (file-readable-p file)
   (let ((c (assoc file (symbol-value cache-var)))
	 (modtime (elt (file-attributes (file-truename file)) 5)))
     (if (and c (equal (second c) modtime))
	 (cddr c)
       (when c (set cache-var (delq c (symbol-value cache-var))))
       (let (new)
	 (message "Loading %s ..." file)
	 (save-excursion
	   (psgml-push-to-entity file)
	   (setq default-directory (file-name-directory file))
	   (setq new (funcall parser-fun)))
	 (push (cons file (cons modtime new)) (symbol-value cache-var))
	 (message "Loading %s ... done" file)
	 new)))))

(defun psgml-main-directory ()
  "Directory of the document entity."
  (let ((cb (current-buffer)))
    (set-buffer psgml-current-top-buffer)
    (prog1 default-directory
      (set-buffer cb))))

(defun psgml-trace-lookup (&rest args)
  "Log a message like `psgml-log-message', but only if `psgml-trace-entity-lookup' is set."
  (when psgml-trace-entity-lookup
    (apply (function psgml-log-message) args)))


(defun psgml-catalog-lookup (files pubid type name)
  "Look up the public identifier/entity name in catalogs.
The result is a file name or nil. FILES is a list of catalogs to use.
PUBID is the public identifier \(if any). TYPE is the entity type and
NAME is the entity name."
  (cond ((eq type 'param)
	 (setq name (format "%%%s" name)
	       type 'entity))
	((eq type 'dtd)
	 (setq type 'doctype)))
  ;;(psgml-trace-lookup "  [pubid='%s' type=%s name='%s']" pubid type name)
  (let ((remaining files)
	(file nil))
    (while (and remaining (null file))
      (let ((additional nil)		; Extra catalogs to search
	    (cat (psgml-cache-catalog (car remaining) 'psgml-catalog-assoc
				     (function psgml-parse-catalog-buffer)
				     (psgml-main-directory))))
	(psgml-trace-lookup "  catalog: %s %s"
			   (expand-file-name (car remaining)
					     (psgml-main-directory))
			   (if (null cat) "empty/non existent" "exists"))
	(when pubid
	  ;; Giv PUBLIC entries priority over ENTITY and DOCTYPE
	  (loop for (key cname cfile) in cat
		while (not file) do
		(when (and (eq 'public key)
			   (string= pubid cname))
		  (when (file-readable-p cfile) (setq file cfile))
		  (psgml-trace-lookup "  >> %s [by pubid]%s"
				     cfile (if file "" " !unreadable")))))
	(loop for (key cname cfile) in cat
	      while (not file) do
	      (when (eq 'catalog key)
		(push cfile additional))
	      (when (and (eq type key)
			 (or (null cname)
			     (string= name cname)))
		(when (file-readable-p cfile) (setq file cfile))
		(psgml-trace-lookup "  >> %s [by %s %s]%s"
				   cfile key cname
				   (if file "" " !unreadable"))))
	(setq remaining
	      (nconc (nreverse additional) (cdr remaining)))))
    file))


(defun psgml-path-lookup (extid type name)
  (let* ((pubid (psgml-extid-pubid extid))
	 (sysid (psgml-extid-sysid extid))
	 (subst (list '(?% ?%))))
    (when pubid
      (nconc subst (list (cons ?p (psgml-transliterate-file pubid)))
	     (psgml-pubid-parts pubid))
      (setq pubid (psgml-canonize-pubid pubid)))
    (when sysid (nconc subst (list (cons ?s sysid))))
    (when name  (nconc subst (list (cons ?n name))))
    (when type  (nconc subst (list (cons ?y (cond ((eq type 'dtd) "dtd")
						  ((eq type 'text) "text")
						  ((eq type 'param) "parm")
						  (t "psgml"))))))
    (psgml-debug "Ext. file subst. = %S" subst)
    (loop for cand in psgml-public-map
	  thereis
	  (and (setq cand (psgml-subst-expand cand subst))
	       (file-readable-p
		(setq cand
		      (psgml-extid-expand (substitute-in-file-name cand)
					 extid)))
	       (not (file-directory-p cand))
	       cand))))

(defun psgml-external-file (extid &optional type name)
  "Return file name for entity with external identifier EXTID.
Optional argument TYPE should be the type of entity and NAME should be
the entity name."
  ;; extid is (pubid . sysid)
  (let ((pubid (psgml-extid-pubid extid)))
    (when pubid (setq pubid (psgml-canonize-pubid pubid)))
    (psgml-trace-lookup "Start looking for %s entity %s public %s system %s"
		       (or type "-")
		       (or name "?")
		       pubid
		       (psgml-extid-sysid extid))
    (or (if (and psgml-system-identifiers-are-preferred
		 (psgml-extid-sysid extid))
	    (or (psgml-lookup-sysid-as-file extid)
		(psgml-path-lookup  ;Try the path also, but only using sysid
		 (psgml-make-extid nil (psgml-extid-sysid extid))
		 nil nil)))
	(psgml-catalog-lookup psgml-current-localcat pubid type name)
	(psgml-catalog-lookup psgml-catalog-files pubid type name)
	(if (not psgml-system-identifiers-are-preferred)
	    (psgml-lookup-sysid-as-file extid))
	(psgml-path-lookup extid type name))))

(defun psgml-lookup-sysid-as-file (extid)
  (let ((sysid (psgml-extid-sysid extid)))
    (and sysid
	 (loop for pat in psgml-public-map
	       never (string-match "%[Ss]" pat))
	 (file-readable-p (setq sysid (psgml-extid-expand sysid extid)))
	 sysid)))

(defun psgml-insert-external-entity (extid &optional type name)
  "Insert the contents of an external entity at point.
EXTID is the external identifier of the entity. Optional arguments TYPE
is the entity type and NAME is the entity name, used to find the entity.
Returns nil if entity is not found."
  (let* ((pubid (psgml-extid-pubid extid))
	 (sysid (psgml-extid-sysid extid)))
    (or (if sysid
	    (loop for fn in psgml-sysid-resolve-functions
		  thereis (funcall fn sysid)))
	(let ((file (psgml-external-file extid type name)))
	  (and file (insert-file-contents file)))
	(progn
	  (psgml-log-warning "External entity %s not found" name)
	  (when pubid
	    (psgml-log-warning "  Public identifier %s" pubid))
	  (when sysid
	    (psgml-log-warning "  System identfier %s" sysid))
	  nil))))


;; Parse a buffer full of catalogue entries.
(defun psgml-parse-catalog-buffer ()
  "Parse all entries in a catalogue."
  (let ((psgml-pxml-p nil))
    (psgml-trace-lookup "  (Parsing catalog)")
    (loop
     while (psgml-skip-cs)
     for type = (downcase (psgml-check-cat-literal))
     for class = (cdr (assoc type '(("public" . public) ("dtddecl" . public)
				    ("entity" . name)   ("linktype" . name)
				    ("doctype" . name)  ("sgmldecl" . noname)
				    ("document" . noname)
				    ("catalog"  . noname))))
     when (not (null class))
     collect
     (let* ((name
	     (cond ((eq class 'public)
		    (psgml-skip-cs)
		    (psgml-canonize-pubid (psgml-check-minimum-literal)))
		   ((string= type "doctype")
		    (psgml-general-case (psgml-check-cat-literal)))
		   ((eq class 'name)
		    (psgml-entity-case (psgml-check-cat-literal)))))
	    (file
	     (expand-file-name (psgml-check-cat-literal))))
       (list (intern type) name file)))))


(defun psgml-check-cat-literal ()
  "Read the next catalog token.
Skips any leading spaces/comments."
  (psgml-skip-cs)
  (or (psgml-parse-literal)
      (buffer-substring-no-properties
       (point)
       (progn (skip-chars-forward "^ \r\n\t")
	      (point)))))

(defconst psgml-formal-pubid-regexp
  (concat
   "^\\(+//\\|-//\\|\\)"		; Registered indicator  [1]
   "\\(\\([^/]\\|/[^/]\\)+\\)"		; Owner                 [2]
   "//"
   "\\([^ ]+\\)"			; Text class            [4]
   " "
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Text description      [5]
   "//"
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Language              [7]
   "\\(//"				;		        [9]
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Version	        [10]
   "\\)?"))

(defun psgml-pubid-parts (pubid)
  (nconc
   (if (string-match psgml-formal-pubid-regexp pubid)
       (nconc
	(list
	 (cons ?o (psgml-transliterate-file (psgml-matched-string pubid 2)))
	 (cons ?c (downcase (psgml-matched-string pubid 4)))
	 (cons ?d (psgml-transliterate-file (psgml-matched-string pubid 5)))
	 ;; t alias for d  (%T used by sgmls)
	 (cons ?t (psgml-transliterate-file (psgml-matched-string pubid 5)))
	 (cons ?l (downcase (psgml-matched-string pubid 7))))
	(if (match-beginning 9)
	    (list (cons ?v (psgml-transliterate-file
			    (psgml-matched-string pubid 10)))))))))

(defun psgml-canonize-pubid (pubid)
  (if (string-match psgml-formal-pubid-regexp pubid)
      (concat
       (psgml-matched-string pubid 1)	; registered indicator
       (psgml-matched-string pubid 2)	; Owner
       "//"
       (upcase (psgml-matched-string pubid 4)) ; class
       " "
       (psgml-matched-string pubid 5)	; Text description
       "//"
       (upcase (psgml-matched-string pubid 7)) ; Language
       "//"
       (if (match-beginning 9)
	   (psgml-matched-string pubid 10) ""))))

(defun psgml-transliterate-file (string)
  (mapconcat (function (lambda (c)
			 (char-to-string
			  (or (cdr-safe (assq c psgml-public-transliterations))
			      c))))
	     string ""))

(defun psgml-subst-expand-char (c parts)
  (cdr-safe (assq (downcase c) parts)))

(defun psgml-subst-expand (s parts)
  (loop for i from 0 to (1- (length s))
	as c = (aref s i)
	concat (if (eq c ?%)
		   (or (psgml-subst-expand-char (aref s (incf i)) parts)
		       (return nil))
		 (char-to-string (aref s i)))))

(defun psgml-matched-string (string n &optional regexp noerror)
  (let ((res (if regexp
		 (or (string-match regexp string)
		     noerror
		     (error "String match fail")))))
    (if (or (null regexp)
	    (numberp res))
	(substring string (match-beginning n)
		   (match-end n)))))

;;;; Files for SGML declaration and DOCTYPE declaration

(defun psgml-declaration ()
  (or psgml-declaration
      (if psgml-doctype
	  (psgml-in-file-eval psgml-doctype
			     '(psgml-declaration)))
      (if psgml-parent-document
	  (psgml-in-file-eval (car psgml-parent-document)
			     '(psgml-declaration)))
      ;; *** check for sgmldecl comment
      (psgml-external-file nil 'psgmldecl)
      )
  )

(defun psgml-in-file-eval (file expr)
  (let ((cb (current-buffer)))
    (set-buffer (find-file-noselect file))
    (prog1 (eval expr)
      (set-buffer cb))))


;;;; Entity references and positions

(defstruct (psgml-eref
	    (:constructor psgml-make-eref (entity start end))
	    (:type list))
  entity
  start					; type: epos
  end)

(defun psgml-make-epos (eref pos)
  (cons eref pos))

(defun psgml-epos-eref (epos)
  (if (consp epos)
      (car epos)))

(defun psgml-epos-pos (epos)
  "The buffer position of EPOS withing its entity."
  (if (consp epos)
      (cdr epos)
    epos))

(defun psgml-bpos-p (epos)
  "True if EPOS is a position in the main buffer."
  (numberp epos))

(defun psgml-strict-epos-p (epos)
  "True if EPOS is a position in an entity other then the main buffer."
  (consp epos))

(defun psgml-epos (pos)
  "Convert a buffer position POS into an epos."
  (if psgml-current-eref
      (psgml-make-epos psgml-current-eref pos)
    pos))

(defun psgml-epos-before (epos)
  "The last position in buffer not after EPOS.
If EPOS is a buffer position this is the same. If EPOS is in an entity
this is the buffer position before the entity reference."
  (while (consp epos)
    (setq epos (psgml-eref-start (psgml-epos-eref epos))))
  epos)

(defun psgml-epos-after (epos)
  "The first position in buffer after EPOS.
If EPOS is in an other entity, buffer position is after
entity reference leading to EPOS."
  (while (consp epos)
    (setq epos (psgml-eref-end (psgml-epos-eref epos))))
  epos)

(defun psgml-epos-promote (epos)
  "Convert position in entity structure EPOS to a buffer position.
If EPOS is in an entity, the buffer position will be the position
before the entity reference if EPOS is first character in entity
text. Otherwise buffer position will be after entity reference."
  (while (and (consp epos)
	      (= (cdr epos) 1))
    (setq epos (psgml-eref-start (car epos))))
  (psgml-epos-after epos))


;;;; DTD repository
;;compiled-dtd: extid -> Compiled-DTD?
;;extid-cdtd-name: extid -> file?
;;up-to-date-p: (file, dependencies) -> cond

;; Emacs Catalogues:
;; Syntax:
;;  ecat ::= (cs | ecat-entry)*
;;  cs ::= (s | comment)
;;  ecat-entry ::= (pub-entry | file-entry)
;;  pub-entry ::= ("PUBLIC", minimal literal, ent-spec?, cat literal)
;;  pub-entry ::= ("FILE", literal, ent-spec?, cat literal)
;;  ent-spec ::= ("[", (name, literal)*, "]")

;; Parsed ecat = (eent*)
;; eent = (type ...)
;;      = ('public pubid cfile . ents)
;;      = ('file file cfile . ents)

(defun psgml-load-ecat (file)
  "Return ecat for FILE."
  (psgml-cache-catalog
   file 'psgml-ecat-assoc
   (function
    (lambda ()
      (let ((psgml-pxml-p nil)
	    new type ents from to name val)
	(while (progn (psgml-skip-cs)
		      (setq type (psgml-parse-name)))
	  (setq type (intern (downcase type)))
	  (setq ents nil from nil)
	  (psgml-skip-cs)
	  (cond
	   ((eq type 'public)
	    (setq from (psgml-canonize-pubid (psgml-check-minimum-literal))))
	   ((eq type 'file)
	    (setq from (expand-file-name (psgml-check-cat-literal)))))
	  (cond
	   ((null from)
	    (error "Syntax error in ECAT: %s" file))
	   (t
	    (psgml-skip-cs)
	    (when (psgml-parse-char ?\[)
	      (while (progn (psgml-skip-cs)
			    (setq name (psgml-parse-name t)))
		(psgml-skip-cs)
		(setq val (psgml-check-literal))
		(push (cons name val) ents))
	      (psgml-check-char ?\])
	      (psgml-skip-cs))
	    (setq to (expand-file-name (psgml-check-cat-literal)))
	    (push (cons type (cons from (cons to ents)))
		  new))))
	(nreverse new))))))

(defun psgml-ecat-lookup (files pubid file)
  "Return (file . ents) or nil."
  (let ((params (psgml-dtd-parameters psgml-dtd-info)))
    (loop
     for f in files
     do (psgml-debug "Search ECAT %s" f)
     thereis
     (loop
      for (type name cfile . ents) in (psgml-load-ecat f)
      thereis
      (if (and (cond ((eq type 'public) (equal name pubid))
		     ((eq type 'file)   (equal name file)))
	       (loop for (name . val) in ents
		     for entity = (psgml-lookup-entity name params)
		     always (and entity
				 (equal val (psgml-entity-text entity)))))
	  (cons cfile ents))))))

;;(let ((psgml-dtd-info (psgml-make-dtd nil)))
;;  (psgml-ecat-lookup psgml-ecat-files
;;		    "-//lenst//DTD My DTD//EN//"
;;		    "/home/u5/lenst/src/psgml/bar.dtd"))


;;;; Merge compiled dtd

(defun psgml-try-merge-compiled-dtd (pubid file)
  (when pubid (setq pubid (psgml-canonize-pubid pubid)))
  (when file (setq file (expand-file-name file)))
  (psgml-debug "Find compiled dtd for %s %s" pubid file)
  (let ((ce (or (psgml-ecat-lookup psgml-current-local-ecat pubid file)
		(psgml-ecat-lookup psgml-ecat-files pubid file))))
    (and ce
	 (let ((cfile (car ce))
	       (ents  (cdr ce)))
	   (psgml-debug "Found %s" cfile)
	   (if (psgml-use-special-case)
	       (psgml-try-merge-special-case pubid file cfile ents)
	     (and (psgml-bdtd-load cfile file ents)
		  (psgml-bdtd-merge)))))))

(defun psgml-use-special-case ()
  (and (null (psgml-dtd-merged psgml-dtd-info))
       (psgml-eltype-table-empty (psgml-dtd-eltypes psgml-dtd-info))
       (eq 'dtd (psgml-entity-type (psgml-eref-entity psgml-current-eref)))))

(defun psgml-try-merge-special-case (pubid file cfile ents)
  (let (cdtd)
    (psgml-debug "Merging special case")
    ;; Look for a compiled dtd in som other buffer
    (let ((cb (current-buffer)))
      (loop for b in (buffer-list)
	    until
	    (progn (set-buffer b)
		   (and psgml-buffer-parse-state
			(let ((m (psgml-dtd-merged
				  (psgml-pstate-dtd psgml-buffer-parse-state))))
			  (and m
			       (string-equal cfile (car m))
			       (setq cdtd (cdr m)))))))
      (set-buffer cb))
    ;; Load a new compiled dtd
    (unless cdtd
      (and (psgml-bdtd-load cfile file ents)
	   (setq cdtd (psgml-bdtd-read-dtd))))
    ;; Do the merger
    (cond
     ((and cdtd
	   (psgml-check-entities (psgml-dtd-parameters psgml-dtd-info)
				(psgml-dtd-parameters cdtd)))
      (setf (psgml-dtd-eltypes psgml-dtd-info)
	    (psgml-dtd-eltypes cdtd))
      (psgml-merge-entity-tables (psgml-dtd-entities psgml-dtd-info)
				(psgml-dtd-entities cdtd))
      (psgml-merge-entity-tables (psgml-dtd-parameters psgml-dtd-info)
				(psgml-dtd-parameters cdtd))
      (psgml-merge-shortmaps (psgml-dtd-shortmaps psgml-dtd-info)
			    (psgml-dtd-shortmaps cdtd))
      (setf (psgml-dtd-dependencies psgml-dtd-info)
	    (nconc (psgml-dtd-dependencies psgml-dtd-info)
		   (psgml-dtd-dependencies cdtd)))
      (setf (psgml-dtd-merged psgml-dtd-info) (cons cfile cdtd))))))


;;;; Pushing and poping entities

(defun psgml-push-to-entity (entity &optional ref-start type)
  "Set current buffer to a buffer containing the entity ENTITY.
ENTITY can also be a file name.  Optional argument REF-START should be
the start point of the entity reference.  Optional argument TYPE,
overrides the entity type in entity look up."
  (psgml-debug "Push to %s"
	      (cond ((stringp entity)
		     (format "string '%s'" entity))
		    (t
		     (psgml-entity-name entity))))
  (when ref-start
    ;; don't consider a RS shortref here again
    (setq psgml-rs-ignore-pos ref-start))
  (unless (and psgml-scratch-buffer
	       (buffer-name psgml-scratch-buffer))
    (setq psgml-scratch-buffer (generate-new-buffer " *entity*")))
  (let ((cb (current-buffer))
	(dd default-directory)
	(syntax-table (syntax-table))
	(pxml-p psgml-pxml-p)
	;;*** should eref be argument ?
	(eref (psgml-make-eref (if (stringp entity)
				  (psgml-make-entity entity nil nil)
				entity)
			      (psgml-epos (or ref-start (point)))
			      (psgml-epos (point)))))
    (set-buffer psgml-scratch-buffer)
    ;; For MULE to not misinterpret binary data set the mc-flag
    ;; (reported by Jeffrey Friedl <jfriedl@nff.ncl.omron.co.jp>)
    (set 'mc-flag nil)
    (when (eq psgml-scratch-buffer (default-value 'psgml-scratch-buffer))
      (make-local-variable 'psgml-scratch-buffer)
      (setq psgml-scratch-buffer nil))
    (when after-change-functions		;***
      (message "OOPS: after-change-functions not NIL in scratch buffer %s: %s"
	       (current-buffer)
	       after-change-functions)
      (setq before-change-functions nil
	    after-change-functions nil))
    (setq psgml-last-entity-buffer (current-buffer))
    (erase-buffer)
    (setq default-directory dd)
    (make-local-variable 'psgml-current-file)
    (make-local-variable 'psgml-current-eref)
    (setq psgml-current-eref eref)
    (set-syntax-table syntax-table)
    (make-local-variable 'psgml-previous-buffer)
    (setq psgml-previous-buffer cb)
    (setq psgml-pxml-p pxml-p)
    (setq psgml-rs-ignore-pos		; don't interpret beginning of buffer
					; as #RS if internal entity.
	  (if (or (stringp entity)
		  (stringp (psgml-entity-text entity)))
	      (point)
	    0))
    (when psgml-buffer-parse-state
      (psgml-debug "-- pstate set in scratch buffer")
      (setq psgml-buffer-parse-state nil))
    (cond
     ((stringp entity)			; a file name
      ;;(save-excursion ) test remove [lenst/1998-06-19 12:49:47]
      (insert-file-contents entity)
      (setq psgml-current-file entity)
      ;; (goto-char (point-min)) ??
      (setq default-directory (file-name-directory entity)))
     ((consp (psgml-entity-text entity)) ; external id?
      (let* ((extid (psgml-entity-text entity))
	     (file
	      (psgml-external-file extid
				  (or type (psgml-entity-type entity))
				  (psgml-entity-name entity))))
	(when psgml-parsing-dtd
	  (push (or file t)
		(psgml-dtd-dependencies psgml-dtd-info)))
	(psgml-debug "Push to %s = %s" extid file)
	(cond
	 ((and file psgml-parsing-dtd
	       (psgml-try-merge-compiled-dtd (psgml-extid-pubid extid)
					    file))
	  (goto-char (point-max)))
	 (file
	  ;; fifth arg not available in early v19
	  ;;(erase-buffer) already erase the buffer
	  (insert-file-contents file nil nil nil)
	  (setq psgml-current-file file)
	  (setq default-directory (file-name-directory file))
	  (goto-char (point-min)))
	 (t ;; No file for entity
	  (save-excursion
	    (let* ((pubid (psgml-extid-pubid extid))
		   (sysid (psgml-extid-sysid extid)))
	      (or (if sysid		; try the sysid hooks
		      (loop for fn in psgml-sysid-resolve-functions
			    thereis (funcall fn sysid)))
		  (progn
		    ;; Mark entity as not found
		    (setf (psgml-entity-marked-undefined-p entity) t)
		    (psgml-log-warning "External entity %s not found"
				      (psgml-entity-name entity))
		    (when pubid
		      (psgml-log-warning "  Public identifier %s" pubid))
		    (when sysid
		      (psgml-log-warning "  System identfier %s" sysid))
		    nil))))))))
     (t ;; internal entity
      (save-excursion
	(insert (psgml-entity-text entity)))))))

(defun psgml-pop-entity ()
  (cond ((and (boundp 'psgml-previous-buffer)
	      (bufferp psgml-previous-buffer))
	 (psgml-debug "Exit entity")
	 (setq psgml-last-entity-buffer psgml-previous-buffer)
	 (set-buffer psgml-previous-buffer)
	 t)))

(defun psgml-goto-epos (epos)
  "Goto a position in an entity given by EPOS."
  (assert epos)
  (cond ((psgml-bpos-p epos)
	 (goto-char epos))
	(t
	 (let ((eref (psgml-epos-eref epos)))
	   (psgml-cleanup-entities)
	   (psgml-goto-epos (psgml-eref-end eref))
	   (psgml-push-to-entity (psgml-eref-entity eref)
				(psgml-epos-pos (psgml-eref-start eref))))
	 (goto-char (psgml-epos-pos epos)))))

(defun psgml-pop-all-entities ()
  (while (psgml-pop-entity)))

(defun psgml-cleanup-entities ()
  (let ((cb (current-buffer))
	(n 0))
    (while (and psgml-scratch-buffer (buffer-name psgml-scratch-buffer))
      (set-buffer psgml-scratch-buffer)
      (assert (not (eq psgml-scratch-buffer
		       (default-value 'psgml-scratch-buffer))))
      (incf n))
    (while (> n 10)
      (set-buffer (prog1 psgml-previous-buffer
		    (kill-buffer (current-buffer))))
      (decf n))
    (set-buffer cb)))

(defun psgml-any-open-param/file ()
  "Return true if there currently is a parameter or file open."
  (and (boundp 'psgml-previous-buffer)
       psgml-previous-buffer))


;;;; Parse tree

(defstruct (psgml-tree
	    (:type vector)
	    (:constructor psgml-make-tree
			  (eltype stag-epos stag-len  parent level
				  excludes includes pstate net-enabled
				  conref &optional shortmap pshortmap asl)))
  eltype				; element object
  ;;start				; start point in buffer
  ;;end					; end point in buffer
  stag-epos				; start-tag entity position
  etag-epos				; end-tag entity position
  stag-len				; length of start-tag
  etag-len				; length of end-tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  next					; next sibling tree
  content				; child trees
  net-enabled				; if NET enabled (t this element,
					;  other non-nil, some parent)
  conref				; if conref attribute used
  shortmap				; shortmap at start of element
  pshortmap				; parents shortmap
  asl					; attribute specification list
)


(defun psgml-tree-end (tree)
  "Buffer position after end of TREE."
  (let ((epos (psgml-tree-etag-epos tree))
	(len (psgml-tree-etag-len tree)))
    (cond ((psgml-bpos-p epos)
	   (+ epos len))
	  ((zerop len)
	   (psgml-epos-promote epos))
	  (t
	   (psgml-epos-after epos)))))


;;;; (text) Element view of parse tree

(defmacro psgml-alias-fields (orig dest &rest fields)
  (let ((macs nil))
    (while fields
      (push
       (` (defmacro (, (intern (format "%s-%s" dest (car fields)))) (element)
	    (, (format "Return %s field of ELEMENT." (car fields)))
	    (list
	     '(, (intern (format "%s-%s" orig (car fields))))
	     element)))
       macs)
      (setq fields (cdr fields)))
    (cons 'progn macs)))

(psgml-alias-fields psgml-tree psgml-element
  eltype				; element object
  ;;  start					; start point in buffer
  stag-epos
  etag-epos
  stag-len				; length of start-tag
  etag-len				; length of end-tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  net-enabled				; if NET enabled
  )

(defun psgml-element-model (element)
  "Declared content or content model of ELEMENT."
  (psgml-eltype-model (psgml-tree-eltype element)))

(defun psgml-element-name (element)
  "Return name (symbol) of ELEMENT."
  (psgml-tree-eltype element))

(defun psgml-element-gi (element)
  "Return general identifier (string) of ELEMENT."
  (psgml-eltype-name (psgml-tree-eltype element)))

(defun psgml-element-appdata (element prop)
  "Return the application data named PROP associated with the type of ELEMENT."
  (psgml-eltype-appdata (psgml-tree-eltype element) prop))

(defmacro psgml-element-stag-optional (element)
  "True if start-tag of ELEMENT is omissible."
  (`(psgml-eltype-stag-optional (psgml-tree-eltype (, element)))))

(defun psgml-element-etag-optional (element)
  "True if end-tag of ELEMENT is omissible."
  (psgml-eltype-etag-optional (psgml-tree-eltype element)))

(define-compiler-macro psgml-element-etag-optional (element)
  "True if end-tag of ELEMENT is omissible."
  (`(psgml-eltype-etag-optional (psgml-tree-eltype (, element)))))

(defun psgml-element-attlist (element)
  "Return the attribute specification list of ELEMENT."
  (psgml-eltype-attlist (psgml-tree-eltype element)))

(defun psgml-element-mixed (element)
  "True if ELEMENT has mixed content."
  (psgml-eltype-mixed (psgml-tree-eltype element)))

(define-compiler-macro psgml-element-mixed (element)
  (`(psgml-eltype-mixed (psgml-tree-eltype (, element)))))

(defun psgml-element-start (element)
  "Position before start of ELEMENT."
  (psgml-epos-promote (psgml-tree-stag-epos element)))

(defun psgml-element-stag-end (element)
  "Position after start-tag of ELEMENT."
  (let ((epos (psgml-tree-stag-epos element))
	(len (psgml-tree-stag-len element)))
    (cond ((psgml-bpos-p epos)
	   (+ epos len))
	  ((zerop len)
	   (psgml-epos-promote epos))
	  (t
	   (psgml-epos-after epos)))))

(defun psgml-element-empty (element)
  "True if ELEMENT is empty."
  (or (psgml-tree-conref element)
      (and (not psgml-pxml-p)
	   (eq psgml-empty (psgml-element-model element)))))

(defun psgml-check-empty (name)
  "True if element with NAME is empty."
  (let ((eltype (if (symbolp name) name (psgml-lookup-eltype name))))
    (eq psgml-empty (psgml-eltype-model eltype))))

(defun psgml-element-data-p (element)
  "True if ELEMENT can have data characters in its content."
  (or (psgml-element-mixed element)
      (eq psgml-cdata (psgml-element-model element))
      (eq psgml-rcdata (psgml-element-model element))))

(defun psgml-element-context-string (element)
  "Return string describing context of ELEMENT."
  (if (eq element psgml-top-tree)
      ""
    (format "in %s %s"
	    (psgml-element-gi element)
	    (psgml-element-context-string (psgml-tree-parent element)))))


;;;; Display and Mode-line

(defun psgml-update-display ()
  (when (not (eq this-command 'keyboard-quit))
    ;; Don't let point be inside an invisible region
    (when (and (get-text-property (point) 'invisible)
	       (eq (get-text-property (point) 'invisible)
		   (get-text-property (1- (point)) 'invisible)))
      (setq psgml-last-element nil)	; May not be valid after point moved
      (if (memq this-command '(backward-char previous-line backward-word))
	  (goto-char (or (previous-single-property-change (point) 'invisible)
			 (point-min)))
	(goto-char (or (next-single-property-change (point) 'invisible)
		       (point-max)))))
    (when (and (not executing-macro)
	       (or psgml-live-element-indicator
		   psgml-set-face)
	       (not (null psgml-buffer-parse-state))
	       (sit-for 0))
      (let ((deactivate-mark nil))
	(psgml-need-dtd)
	(let ((start
	       (save-excursion (psgml-find-start-point (point))
			       (psgml-pop-all-entities)
			       (point)))
	      (eol-pos
	       (save-excursion (end-of-line 1) (point))))
	  (let ((quiet (< (- (point) start) 500)))
	    ;;(message "Should parse %s to %s => %s" start (point) quiet)
	    (when (if quiet
		      t
		    (setq psgml-current-element-name "?")
		    (sit-for 1))

	      ;; Find current element
	      (cond ((and (memq this-command psgml-users-of-last-element)
			  psgml-last-element)
		     (setq psgml-current-element-name
			   (psgml-element-gi psgml-last-element)))
		    (psgml-live-element-indicator
		     (save-excursion
		       (condition-case nil
			   (psgml-parse-to
			    (point) (function input-pending-p) quiet)
			 (error
			  (setq psgml-current-element-name "*error*")))
		       (unless (input-pending-p)
			 (setq psgml-current-element-name
			       (psgml-element-gi psgml-current-tree))))))
	      ;; Set face on current line
	      (when (and psgml-set-face (not (input-pending-p)))
		(save-excursion
		  (condition-case nil
		      (psgml-parse-to
		       eol-pos (function input-pending-p) quiet)
		    (error nil)))))))
	;; Set face in rest of buffer
	(psgml-fontify-buffer 6)		;FIXME: make option for delay
	))))

(defun psgml-fontify-buffer (delay)
  (and
   psgml-set-face
   (null (psgml-tree-etag-epos
	  (psgml-pstate-top-tree psgml-buffer-parse-state)))
   (sit-for delay)
   (condition-case nil
       (save-excursion
	 (message "Fontifying...")
	 (psgml-parse-until-end-of nil nil
				  (function input-pending-p)
				  t)
	 (message "Fontifying...done"))
     (error nil))))

(defun psgml-set-active-dtd-indicator (name)
  (set (make-local-variable 'psgml-active-dtd-indicator)
       (list (format " [%s" name)
	     '(psgml-live-element-indicator ("/" psgml-current-element-name))
	     "]"))
  (force-mode-line-update))

;;;; Parser state

(defstruct (psgml-pstate
	    (:constructor psgml-make-pstate (dtd top-tree)))
  dtd
  top-tree)

;(defsubst psgml-excludes ()
;  (psgml-tree-excludes psgml-current-tree))

;(defsubst psgml-includes ()
;  (psgml-tree-includes psgml-current-tree))

(defsubst psgml-current-mixed-p ()
  (psgml-element-mixed psgml-current-tree))

(defun psgml-set-initial-state (dtd)
  "Set initial state of parsing"
  (make-local-variable 'before-change-function)
  (setq before-change-function 'psgml-note-change-at)
  (make-local-variable 'after-change-function)
  (setq after-change-function 'psgml-set-face-after-change)
  (psgml-set-active-dtd-indicator (psgml-dtd-doctype dtd))
  (let ((top-type			; Fake element type for the top
					; node of the parse tree
	 (psgml-make-eltype "#DOC")	; was "Document (no element)"
	 ))
    (setf (psgml-eltype-model top-type)
	  (psgml-make-primitive-content-token
	   (psgml-eltype-token
	    (psgml-lookup-eltype (psgml-dtd-doctype dtd) dtd))))
    (setq psgml-buffer-parse-state
	  (psgml-make-pstate dtd
			    (psgml-make-tree top-type
					    0 0 nil 0 nil nil nil nil nil)))))

(defun psgml-set-parse-state (tree where)
  "Set parse state from TREE, either from start of TREE if WHERE is start
or from after TREE if WHERE is after."
  (setq psgml-current-tree tree
	psgml-markup-tree tree
	psgml-rs-ignore-pos 0 )
  (let ((empty
	 (psgml-element-empty tree)))
    (cond ((and (eq where 'start)
		(not empty))
	   (setq psgml-current-state (psgml-element-model psgml-current-tree)
		 psgml-current-shortmap (psgml-tree-shortmap psgml-current-tree)
		 psgml-previous-tree nil)
	   (setq psgml-markup-type
		 (if (and (not (zerop (psgml-tree-stag-len tree)))
			  (psgml-bpos-p (psgml-tree-stag-epos tree)))
		     'start-tag)
		 psgml-markup-start (psgml-element-start psgml-current-tree))
	   (psgml-goto-epos (psgml-tree-stag-epos psgml-current-tree))
	   (forward-char (psgml-tree-stag-len psgml-current-tree)))
	  (t
	   (setq psgml-current-state (psgml-tree-pstate psgml-current-tree)
		 psgml-current-shortmap (psgml-tree-pshortmap psgml-current-tree)
		 psgml-previous-tree psgml-current-tree)
	   (psgml-goto-epos (psgml-tree-etag-epos psgml-current-tree))
	   (forward-char (psgml-tree-etag-len psgml-current-tree))
	   (setq psgml-markup-type (if empty 'start-tag 'end-tag)
		 psgml-markup-start (- (point)
				      (psgml-tree-etag-len psgml-current-tree)))
	   (setq psgml-current-tree (psgml-tree-parent psgml-current-tree))))
    (assert psgml-current-state)))

(defsubst psgml-final-p (state)
  ;; Test if a state/model can be ended
  (or (not (psgml-model-group-p state))
      (psgml-final state)))

;(defun psgml-current-element-contains-data ()
;  "Retrun true if the current open element is either mixed or is (r)cdata."
;  (or (eq psgml-cdata psgml-current-state)
;      (eq psgml-rcdata psgml-current-state)
;      (psgml-current-mixed-p)))

;(defun psgml-current-element-content-class ()
;  "Return a string describing the type of content in the current element.
;The type can be CDATA, RCDATA, ANY, #PCDATA or none."
;  (cond ((eq psgml-cdata psgml-current-state)
;	 "CDATA")
;	((eq psgml-rcdata psgml-current-state)
;	 "RCDATA")
;	((eq psgml-any psgml-current-state)
;	 "ANY")
;	((psgml-current-mixed-p)
;	 "#PCDATA")
;	(t "")))

(defun psgml-promoted-epos (start end)
  "Return an entity position for start of region START END.
If region is empty, choose return an epos as high in the
entity hierarchy as possible."
;; This does not work if the entity is entered by a shortref that
;; only is active in the current element.
  (let ((epos (psgml-epos start)))
    (when (= start end)
      (while (and (psgml-strict-epos-p epos)
		  (= 1 (psgml-epos-pos epos)))
	(setq epos (psgml-eref-start (psgml-epos-eref epos)))))
    epos))

(defun psgml-open-element (eltype conref before-tag after-tag &optional asl)
  (unless (psgml-eltype-defined eltype)
    (setf (psgml-eltype-mixed eltype) t)
    (setf (psgml-eltype-etag-optional eltype) t)
    (when psgml-warn-about-undefined-elements
      (psgml-log-warning
       "Start-tag of undefined element %s; assume O O ANY"
       (psgml-eltype-name eltype))))
  (let* ((emap (psgml-eltype-shortmap eltype))
	 (newmap (if emap
		     (if (eq 'empty emap)
			 nil
		       (psgml-lookup-shortref-map
			(psgml-dtd-shortmaps psgml-dtd-info)
			emap))
		   psgml-current-shortmap))
	 (nt (psgml-make-tree
	      eltype
	      (psgml-promoted-epos before-tag after-tag) ; stag-epos
	      (- after-tag before-tag)	; stag-len
	      psgml-current-tree		; parent
	      (1+ (psgml-tree-level psgml-current-tree)) ; level
	      (append (psgml-eltype-excludes eltype)
		      (psgml-tree-excludes psgml-current-tree))
	      (append (psgml-eltype-includes eltype)
		      (psgml-tree-includes psgml-current-tree))
	      psgml-current-state
	      (if (psgml-tree-net-enabled psgml-current-tree) 1)
	      conref
	      newmap
	      psgml-current-shortmap
	      asl)))
;; (let ((u (psgml-tree-content psgml-current-tree)))
;;      (cond ((and u (> before-tag (psgml-element-start u)))
;;	     (while (and (psgml-tree-next u)
;;			 (> before-tag
;;			    (psgml-element-start (psgml-tree-next u))))
;;	       (setq u (psgml-tree-next u)))
;;	     (setf (psgml-tree-next u) nt))
;;	    (t
;;	     (setf (psgml-tree-content psgml-current-tree) nt))))
    ;; Install new node in tree
    (cond (psgml-previous-tree
	   (psgml-debug "Open element %s: after %s"
		       eltype (psgml-tree-eltype psgml-previous-tree))
	   (setf (psgml-tree-next psgml-previous-tree) nt))
	  (t
	   (psgml-debug "Open element %s: first in %s"
		       eltype (psgml-tree-eltype psgml-current-tree))
	   (setf (psgml-tree-content psgml-current-tree) nt)))
    ;; Prune tree
    ;; *** all the way up?  tree-end = nil?
    (setf (psgml-tree-next psgml-current-tree) nil)
    ;; Set new state
    (setq psgml-current-state (psgml-eltype-model eltype)
	  psgml-current-shortmap newmap
	  psgml-current-tree nt
	  psgml-previous-tree nil)
    (assert psgml-current-state)
    (setq psgml-markup-tree psgml-current-tree)
    (run-hook-with-args 'psgml-open-element-hook psgml-current-tree asl)
    (when (psgml-element-empty psgml-current-tree)
      (psgml-close-element after-tag after-tag))))

(defun psgml-fake-open-element (tree el &optional state)
  (psgml-make-tree
   el 0 0
   tree
   0
   (append (psgml-eltype-excludes el) (psgml-tree-excludes tree))
   (append (psgml-eltype-includes el) (psgml-tree-includes tree))
   state
   nil
   nil))

(defun psgml-close-element (before-tag after-tag)
  (when (or (eq psgml-close-element-trap t)
	    (eq psgml-close-element-trap psgml-current-tree))
    (setq psgml-goal (point)))
  (when psgml-throw-on-element-change
    (throw psgml-throw-on-element-change 'end))
  (psgml-debug "Close element %s" (psgml-tree-eltype psgml-current-tree))
  (setf (psgml-tree-etag-epos psgml-current-tree)
	;;(psgml-promoted-epos before-tag after-tag)
	(psgml-epos before-tag))
  (setf (psgml-tree-etag-len psgml-current-tree) (- after-tag before-tag))
  (run-hooks 'psgml-close-element-hook)
  (setq psgml-markup-tree psgml-current-tree)
  (cond ((eq psgml-current-tree psgml-top-tree)
	 (unless (eobp)
	   (psgml-error "Parse ended")))
	(t
	 (setq psgml-previous-tree psgml-current-tree
	       psgml-current-state (psgml-tree-pstate psgml-current-tree)
	       psgml-current-shortmap (psgml-tree-pshortmap psgml-current-tree)
	       psgml-current-tree (psgml-tree-parent psgml-current-tree))
	 (assert psgml-current-state))))

(defun psgml-fake-close-element (tree)
  (psgml-tree-parent tree))

(defun psgml-note-change-at (at &optional end)
  ;; Inform the cache that there have been some changes after AT
  (when psgml-buffer-parse-state
    (psgml-debug "psgml-note-change-at %s" at)
    (let ((u (psgml-pstate-top-tree psgml-buffer-parse-state)))
      (when u
	;;(message "%d" at)
	(while
	    (cond
	     ((and (psgml-tree-next u)	; Change clearly in next element
		   (> at (psgml-element-stag-end (psgml-tree-next u))))
	      (setq u (psgml-tree-next u)))
	     (t				;
	      (setf (psgml-tree-next u) nil) ; Forget next element
	      (cond
	       ;; If change after this element and it is ended by an end
	       ;; tag no pruning is done.  If the end of the element is
	       ;; implied changing the tag that implied it may change
	       ;; the extent of the element.
	       ((and (psgml-tree-etag-epos u)
		     (> at (psgml-tree-end u))
		     (or (> (psgml-tree-etag-len u) 0)
			 (psgml-element-empty u)))
		nil)
	       (t
		(setf (psgml-tree-etag-epos u) nil)
		(cond;; Enter into content if change is clearly in it
		 ((and (psgml-tree-content u)
		       (> at (psgml-element-stag-end (psgml-tree-content u))))
		  (setq u (psgml-tree-content u)))
		 ;; Check if element has no start tag,
		 ;; then it must be pruned as a change could create
		 ;; a valid start tag for the element.
		 ((and (zerop (psgml-tree-stag-len u))
		       (> at (psgml-element-start u)))
		  ;; restart from to with new position
		  ;; this can't loop forever as
		  ;; position allways gets smaller
		  (setq at (psgml-element-start u)
			u psgml-top-tree))
		 (t
		  (setf (psgml-tree-content u) nil))))))))))))

(defun psgml-list-implications (token type)
  "Return a list of the tags implied by a token TOKEN.
TOKEN is a token, and the list elements are either tokens or `t'.
Where the latter represents end-tags."
  (let ((state psgml-current-state)
	(tree psgml-current-tree)
	(temp nil)
	(imps nil))
    (while				; Until token accepted
	(cond
	 ;; Test if accepted in state
	 ((or (eq state psgml-any)
	      (and (psgml-model-group-p state)
		   (not (memq token (psgml-tree-excludes tree)))
		   (or (memq token (psgml-tree-includes tree))
		       (psgml-get-move state token))))
	  nil)
	 ;; Test if end tag implied
	 ((or (eq state psgml-empty)
	      (and (psgml-final-p state)
		   (not (eq tree psgml-top-tree))))
	  (unless (eq state psgml-empty)	; not realy implied
	    (push t imps))
	  (setq state (psgml-tree-pstate tree)
		tree (psgml-fake-close-element tree))
	  t)
	 ;; Test if start-tag can be implied
	 ((and (setq temp (psgml-required-tokens state))
	       (null (cdr temp)))
	  (setq temp (car temp)
		tree (psgml-fake-open-element tree temp
					     (psgml-get-move state temp))
		state (psgml-element-model tree))
	  (push temp imps)
	  t)
	 ;; No implictions and not accepted
	 (t
	  (psgml-log-warning "Out of context %s" type)
	  (setq imps nil))))
    ;; Return the implications in correct order
    (nreverse imps)))


(defun psgml-eltypes-in-state (tree state)
  "Return list of element types (eltype) valid in STATE and TREE."
  (let* ((req				; Required tokens
	  (if (psgml-model-group-p state)
	      (psgml-required-tokens state)))
	 (elems				; Normally valid tokens
	  (if (psgml-model-group-p state)
	      (nconc req
		     (delq psgml-pcdata-token (psgml-optional-tokens state))))))
    ;; Modify for exceptions
    (loop for et in (psgml-tree-includes tree) ;*** Tokens or eltypes?
	  unless (memq et elems) do (push et elems))
    (loop for et in (psgml-tree-excludes tree)
	  do (setq elems (delq et elems)))
    ;; Check for omitable start-tags
    (when (and psgml-omittag-transparent
	       (not (psgml-final-p state))
	       req
	       (null (cdr req)))
      (let ((et (psgml-token-eltype (car req))))
	(when (psgml-eltype-stag-optional et)
	  (setq elems
		(nconc elems		; *** possibility of duplicates
		       (psgml-eltypes-in-state
			(psgml-fake-open-element tree et)
			(psgml-eltype-model et)))))))
    elems))

(defun psgml-current-list-of-valid-eltypes ()
  "Returns a list of contextually valid element types (eltype)."
  (let ((elems (psgml-eltypes-in-state psgml-current-tree psgml-current-state))
	(tree psgml-current-tree)
	(state psgml-current-state))
    (when psgml-omittag-transparent
      (while (and tree
		  (psgml-final-p state)
		  (psgml-element-etag-optional tree))
	(setq state (psgml-tree-pstate tree)
	      tree (psgml-tree-parent tree))
	(loop for e in (psgml-eltypes-in-state tree state) do
	      (when (not (memq e elems))
		(setq elems (nconc elems (list e)))))))
    ;; FIXME: Filter out elements that are undefined?
    (sort elems (function string-lessp))))

(defun psgml-current-list-of-endable-eltypes ()
  "Return a list of the element types endable in current state."
  (let* ((elems nil)
	 (tree psgml-current-tree)
	 (state psgml-current-state))
    (while
	(and (psgml-final-p state)
	     (not (eq tree psgml-top-tree))
	     (progn
	       (setq elems
		     (nconc elems (list (psgml-tree-eltype tree))))
	       psgml-omittag)
	     (psgml-eltype-etag-optional (psgml-tree-eltype tree)))
      (setq state (psgml-tree-pstate tree)
	    tree (psgml-tree-parent tree)))
    elems))

;;;; Logging of warnings

(defconst psgml-log-buffer-name "*SGML LOG*")

(defvar psgml-log-last-size 0)

(defun psgml-display-log ()
  (let ((buf (get-buffer psgml-log-buffer-name)))
    (when buf
      (display-buffer buf)
      (setq psgml-log-last-size (save-excursion (set-buffer buf)
					       (point-max))))))

(defun psgml-log-warning (format &rest things)
  (when psgml-throw-on-warning
    (apply 'message format things)
    (throw psgml-throw-on-warning t))
  (when (or psgml-show-warnings psgml-parsing-dtd)
    (apply 'psgml-message format things)
    (apply 'psgml-log-message format things)))

(defun psgml-log-message (format &rest things)
  (let ((mess (apply 'format format things))
	(buf (get-buffer-create psgml-log-buffer-name))
	(cb (current-buffer)))
    (set-buffer buf)
    (goto-char (point-max))
    (insert mess "\n")
    (when (get-buffer-window buf)
      (setq psgml-log-last-size  (point-max)))
    (set-buffer cb)))

(defun psgml-error (format &rest things)
  (when psgml-throw-on-error
    (throw psgml-throw-on-error nil))
  (psgml-log-entity-stack)
  (apply 'psgml-log-warning format things)
  (apply 'error format things))

(defun psgml-log-entity-stack ()
  (save-excursion
    (loop
     do (psgml-log-message
	 "%s line %s col %s %s"
	 (or psgml-current-file (buffer-file-name) "-")
	 (count-lines (point-min) (point))
	 (current-column)
	 (let ((entity (if psgml-current-eref
			   (psgml-eref-entity psgml-current-eref))))
	   (if (and entity (psgml-entity-type entity))
	       (format "entity %s" (psgml-entity-name entity))
	     "")))
     while (and (boundp 'psgml-previous-buffer) psgml-previous-buffer)
     do (set-buffer psgml-previous-buffer))))

(defun psgml-parse-warning (format &rest things)
  (psgml-log-entity-stack)
  (apply 'psgml-log-warning format things))

(defun psgml-parse-error (format &rest things)
  (apply 'psgml-error
	 (concat format "; at: %s")
	 (append things (list (buffer-substring-no-properties
			       (point)
			       (min (point-max) (+ (point) 12)))))))

(defun psgml-message (format &rest things)
  (let ((buf (get-buffer psgml-log-buffer-name)))
    (when (and buf
	       (> (save-excursion (set-buffer buf)
				  (point-max))
		  psgml-log-last-size))
      (psgml-display-log)))
  (apply 'message format things))

(defun psgml-reset-log ()
  (let ((buf (get-buffer psgml-log-buffer-name)))
    (when buf
      (setq psgml-log-last-size
	    (save-excursion (set-buffer buf)
			    (point-max))))))

(defun psgml-clear-log ()
  (let ((b (get-buffer psgml-log-buffer-name)))
    (when b
      (delete-windows-on b)
      (kill-buffer b)
      (setq psgml-log-last-size 0))))

(defun psgml-show-or-clear-log ()
  "Show the *SGML LOG* buffer if it is not showing, or clear and
remove it if it is showing."
  (interactive)
  (cond ((and (get-buffer psgml-log-buffer-name)
	      (null (get-buffer-window psgml-log-buffer-name)))
	 (psgml-display-log))
	(t
	 (psgml-clear-log))))



;;; This has noting to do with warnings...

(defvar psgml-lazy-time 0)

(defun psgml-lazy-message (&rest args)
  (unless (= psgml-lazy-time (second (current-time)))
    (apply 'message args)
    (setq psgml-lazy-time (second (current-time)))))

;;;; Shortref maps

(eval-and-compile
  (defconst psgml-shortref-list
    '(
      "\t"				;&#TAB
      "\n"				;&#RE;
      "\001"				;&#RS;
      "\001B"
      "\001\n"
      "\001B\n"
      "B\n"
      " "				;&#SPACE;
      "BB"
      "\""				;&#34;
      "#"
      "%"
      "'"
      "("
      ")"
      "*"
      "+"
      ","
      "-"
      "--"
      ":"
      ";"
      "="
      "@"
      "["
      "]"
      "^"
      "_"
      "{"
      "|"
      "}"
      "~")))

(eval-and-compile
  (defun psgml-shortref-index (string)
    (let ((pos (member string psgml-shortref-list))
	  (len (length psgml-shortref-list)))
      (and pos (- len (length pos))) )))

(defun psgml-make-shortmap (pairs)
  "Create a shortreference map from PAIRS.
Where PAIRS is a list of (delim . ename)."
  (let ((map
	 (make-vector (1+ (length psgml-shortref-list))
		      nil))
	index)
    (loop for p in pairs
	  for delim = (car p)
	  for name = (cdr p)
	  do
	  (setq index (psgml-shortref-index delim))
	  (cond ((null index)
		 (psgml-log-warning
		  "Illegal short reference delimiter '%s'" delim))
		(t
		 (aset map index name))))
    ;; Compute a suitable string for skip-chars-forward that
    ;; can be used to skip over pcdata
    (aset map
	  (eval-when-compile (length psgml-shortref-list))
	  (if (some (function
		     (lambda (r) (aref map (psgml-shortref-index r))))
		    '("\001B\n" "B\n" " " "BB"))
	      "^<]/& \n\t\"#%'()*+,\\-:;=@[]\\^_{|}~"
	    "^<]/&\n\t\"#%'()*+,\\-:;=@[]\\^_{|}~"))
    map))

(defun psgml-shortmap-skipstring (map)
  (if (bolp)
      ""
      (aref map (eval-when-compile (length psgml-shortref-list)))))


(defconst psgml-shortref-oneassq
  (loop for d in psgml-shortref-list
	for c = (aref d 0)
	when (and (= 1 (length d))
		  (/= 1 c) (/= 10 c))
	collect (cons c (psgml-shortref-index d))))

(defun psgml-parse-B ()
  (/= 0 (skip-chars-forward " \t")))

(defun psgml-deref-shortmap (map &optional nobol)
  "Identify shortref delimiter at point and return entity name.
Also move point.  Return nil, either if no shortref or undefined."

  (macrolet
      ((delim (x) (` (aref map (, (psgml-shortref-index x))))))
    (let ((i (if nobol 1 0)))
      (while (numberp i)
	(setq i
	      (cond
	       ((and (bolp) (zerop i)) ; Either "\001" "\001B"
					; "\001\n" "\001B\n"
		(cond ((psgml-parse-B)	; "\001B"
		       (if (eolp)
			   (delim "\001B\n")
			 (delim "\001B")))
		      ((psgml-parse-RE) (delim "\001\n"))
		      ((delim "\001"))
		      (t 1)))
	       ((cond ((psgml-parse-char ?\t) (setq i (delim "\t")) t)
		      ((psgml-parse-char ? )  (setq i (delim " "))  t))
		(cond ((psgml-parse-B) (setq i (delim "BB"))))
		(cond ((psgml-parse-char ?\n)
		       (delim "B\n"))
		      (t i)))
	       ((psgml-parse-RE) (delim "\n"))
	       ((psgml-parse-chars ?- ?-) (delim "--"))
	       ;; The other one character delimiters
	       ((setq i (assq (following-char) psgml-shortref-oneassq))
		(when i (forward-char 1))
		(aref map (cdr i))))))
      i)))

;;; Table of shortref maps

(defun psgml-make-shortref-table ()
  (list nil))

(defun psgml-add-shortref-map (table name map)
  (nconc table (list (cons name map))))

(defun psgml-lookup-shortref-map (table name)
  (cdr (assoc name (cdr table))))

(defun psgml-lookup-shortref-name (table map)
  (car (rassq map (cdr table))))

(defun psgml-merge-shortmaps (tab1 tab2)
  "Merge tables of short reference maps TAB2 into TAB1, modifying TAB1."
  (nconc tab1 (cdr tab2)))

;;;; Parse markup declarations

(defun psgml-skip-until-dsc ()
  (while (progn
	   (if psgml-pxml-p
	       (psgml-skip-upto ("DSO" "DSC" "LITA" "LIT" "XML-SCOM" "COM"))
	     (psgml-skip-upto ("DSO" "DSC" "LITA" "LIT" "COM")))
	   (not (psgml-parse-delim "DSC")))
    (cond ((psgml-parse-literal))
	  ((psgml-parse-delim "DSO")
	   (psgml-skip-until-dsc))
	  ((and psgml-pxml-p (psgml-parse-pxml-comment)))
	  ((and (not psgml-pxml-p) (psgml-parse-comment)))
	  (t (forward-char 1)))))

(defun psgml-skip-upto-mdc ()
  "Move point forward until end of current markup declaration.
Assumes starts with point inside a markup declaration."
  (while (progn
	   (psgml-skip-upto ("DSO" "MDC" "LIT" "LITA" "COM"))
	   (not (psgml-is-delim "MDC")))
    (cond ((psgml-parse-delim "DSO")
	   (psgml-skip-until-dsc))
	  ((psgml-parse-literal))
	  ((psgml-parse-comment))
	  (t (forward-char 1)))))

(defun psgml-do-psgml-declaration ()
  (psgml-skip-upto-mdc)
  (setq psgml-markup-type 'psgml))

(defun psgml-do-doctype ()
  (cond
   (psgml-dtd-info			; Has doctype already been defined
    (psgml-skip-upto-mdc))
   (t
    (let (psgml-markup-start)
      (message "Parsing doctype...")
      (psgml-setup-doctype (psgml-check-name)
			  (psgml-parse-external))
      (message "Parsing doctype...done"))))
  (setq psgml-markup-type 'doctype))

(defun psgml-check-end-of-entity (type)
  (unless (eobp)
    (psgml-parse-error "Illegal character '%c' in %s"
		      (following-char)
		      type)))

(defun psgml-setup-doctype (docname external)
  (let ((psgml-parsing-dtd t))
    (setq psgml-no-elements 0)
    (setq psgml-dtd-info (psgml-make-dtd docname))
    ;;(setq psgml-dtd-shortmaps nil)
    (psgml-skip-ps)
    (cond
     ((psgml-parse-delim "DSO")
      (let ((original-buffer (current-buffer)))
	(psgml-check-dtd-subset)
	(if (eq (current-buffer) original-buffer)
	    (psgml-check-delim "DSC")
	  (psgml-parse-error "Illegal character '%c' in doctype declaration"
			    (following-char))))))
    (cond (external
	   (psgml-push-to-entity (psgml-make-entity docname 'dtd external))
	   (psgml-check-dtd-subset)
	   (psgml-check-end-of-entity "DTD subset")
	   (psgml-pop-entity)))
;;;    (loop for map in psgml-dtd-shortmaps do
;;;	  (psgml-add-shortref-map
;;;	   (psgml-dtd-shortmaps psgml-dtd-info)
;;;	   (car map)
;;;	   (psgml-make-shortmap (cdr map))))
    (psgml-set-initial-state psgml-dtd-info)
    (run-hooks 'psgml-doctype-parsed-hook)))

(defun psgml-do-data (type &optional marked-section)
  "Move point forward until there is an end-tag open after point."
  (let ((start (point))
	(done nil)
	(eref psgml-current-eref)
	psgml-signal-data-function)
    (while (not done)
      ;; FIXME: a lot of hardcoded knowledge about concrete delimiters
      (cond (marked-section
	     (skip-chars-forward (if (eq type psgml-cdata) "^]" "^&]"))
	     (when psgml-data-function
	       (funcall psgml-data-function (buffer-substring-no-properties
					    start (point))))
	     (setq done (psgml-parse-delim "MS-END")))
	    (t
	     (skip-chars-forward (if (eq type psgml-cdata) "^</" "^</&"))
	     (when psgml-data-function
	       (funcall psgml-data-function
			(buffer-substring-no-properties start (point))))
	     (setq done (or (psgml-is-delim "ETAGO" gi)
			    (psgml-is-enabled-net)))))
      (setq start (point))
      (cond
       (done)
       ((eobp)
	(when (eq eref psgml-current-eref)
	  (psgml-error "Unterminated %s %s"
		      type (if marked-section "marked section")))
	(psgml-pop-entity)
	(setq start (point)))
       ((null psgml-data-function)
	(forward-char 1))
       ((psgml-parse-general-entity-ref)
	(setq start (point)))
       (t
	(forward-char 1))))))


(defun psgml-do-marked-section ()
  (let ((status nil))
    (while (progn (psgml-skip-ps)
		  (not (psgml-parse-char ?\[)))
      (push (psgml-check-name)
	    status))
    (cond
     ((member "IGNORE" status)
      (psgml-skip-marked-section)
      (psgml-set-markup-type 'ignored))
     ((or (member "CDATA" status)
	  (member "RCDATA" status))
      (when psgml-signal-data-function
	(funcall psgml-signal-data-function))
      (let ((type (if (member "CDATA" status) psgml-cdata psgml-rcdata)))
	(psgml-do-data type t)
      (psgml-set-markup-type type)))
     (t
      (psgml-set-markup-type 'ms-start)))))

(defun psgml-skip-marked-section ()
  (while (progn
	   (psgml-skip-upto ("MS-START" "MS-END"))
	   (when (eobp) (psgml-error "Marked section unterminated"))
	   (not (psgml-parse-delim "MS-END")))
    (cond ((psgml-parse-delim "MS-START")
	   ;;(search-forward "[")
	   (psgml-skip-marked-section))
	  (t (forward-char 1)))))

(defun psgml-do-usemap ()
  (let (mapname)
    ;;(setq psgml-markup-type 'usemap)
    (unless (psgml-parse-rni "EMPTY")
      (setq mapname (psgml-check-name)))
    (psgml-skip-ps)
    (cond
     ((psgml-is-delim "MDC")
      (psgml-debug "USEMAP %s" (if mapname mapname "#EMPTY"))
      (cond (psgml-dtd-info
	     (setq psgml-current-shortmap
		   (if mapname
		       (or (psgml-lookup-shortref-map
			    (psgml-dtd-shortmaps psgml-dtd-info)
			    mapname)
			   (psgml-error "Undefined shortref map %s" mapname)))))
	    ;; If in prolog
	    (t
	     (psgml-log-warning
	      "USEMAP without associated element type in prolog"))))
     (t
      ;; Should be handled by psgml-dtd
      (psgml-do-usemap-element mapname)))))

(defconst psgml-markup-declaration-table
  '(("SGML"     . psgml-do-psgml-declaration)
    ("DOCTYPE"  . psgml-do-doctype)
    ("ELEMENT"  . psgml-declare-element)
    ("ENTITY"   . psgml-declare-entity)
    ("USEMAP"   . psgml-do-usemap)
    ("SHORTREF" . psgml-declare-shortref)
    ("NOTATION" . psgml-declare-notation)
    ("ATTLIST"  . psgml-declare-attlist)
    ("USELINK"  . psgml-skip-upto-mdc)
    ("LINKTYPE" . psgml-skip-upto-mdc)
    ("LINK"     . psgml-skip-upto-mdc)
    ("IDLINK"   . psgml-skip-upto-mdc)))


(defun psgml-parse-markup-declaration (option)
  "Parse a markup declartion.
OPTION can be `prolog' if parsing the prolog or `dtd' if parsing the
dtd or `ignore' if the declaration is to be ignored."
  (cond
   ((and psgml-pxml-p (psgml-parse-pxml-comment)))
   ((psgml-parse-delim "MDO" (nmstart "COM" "MDC"))
    (cond
     ((psgml-startnm-char-next)
      (setq psgml-markup-type nil)
      (let* ((tok (psgml-parse-nametoken))
	     (rut (assoc (psgml-check-case tok) psgml-markup-declaration-table)))
	(when (and (not (memq option '(prolog ignore)))
		   (member tok '("SGML" "DOCTYPE")))
	  (psgml-error "%s declaration is only valid in prolog" tok))
	(when (and (not (memq option '(dtd ignore)))
		   (member tok '("ELEMENT" "ENTITY" "ATTLIST" "NOTATION"
				 "SHORTREF")))
	  (psgml-error "%s declaration is only valid in doctype" tok))
	(cond ((eq option 'ignore)
	       (psgml-skip-upto-mdc))
	      (rut (psgml-skip-ps)
		   (funcall (cdr rut)))
	      (t (psgml-parse-error
		  "Illegal markup declaration %s" tok)))))
     (t
      (setq psgml-markup-type 'comment)))
    (psgml-skip-ps)
    (psgml-check-delim "MDC")
    (unless (eq option 'ignore)		; Set the markup type given
      (when psgml-markup-type
	(psgml-set-markup-type psgml-markup-type)))
    t)
   ((psgml-parse-delim "MS-START")
    (psgml-do-marked-section))))

;;;; Parsing attribute values

(defun psgml-parse-attribute-specification-list (&optional eltype)
  "Parse an attribute specification list.
Optional argument ELTYPE, is used to resolve omitted name=.
Returns a list of attspec (attribute specification)."
  (setq psgml-conref-flag nil)
  (let ((attlist (if eltype (psgml-eltype-attlist eltype)))
	name val asl attdecl)
    (while (setq name (progn (psgml-parse-s)
			     (psgml-parse-nametoken)))
      (psgml-parse-s)
      (cond ((psgml-parse-delim "VI")
	     (psgml-parse-s)
	     (setq val (psgml-parse-attribute-value-specification 'warn))
	     (if (null val)
		 (setq attdecl nil)
	       (when eltype
		 (or (setq attdecl (psgml-lookup-attdecl name attlist))
		     (psgml-log-warning
		      "Attribute %s not declared for element %s"
		      name (psgml-eltype-name eltype))))))
	    ((null eltype)
	     (psgml-parse-error "Expecting a ="))
	    ((progn
	       (unless psgml-current-shorttag
		 (psgml-log-warning
		  "Must have attribute name when SHORTTAG NO"))
	       (setq attdecl
		     (psgml-find-attdecl-for-value (setq val name)
						  eltype))))
	    (t
	     (psgml-log-warning
	      "%s is not in any name group for element %s."
	      val
	      (psgml-eltype-name eltype))))
      ;; FIXME: What happens when eltype is nil ??
      (cond
       (attdecl
	(push (psgml-make-attspec (psgml-attdecl-name attdecl) val)
	      asl)
	(when (psgml-default-value-type-p 'CONREF
					 (psgml-attdecl-default-value attdecl))
	  (setq psgml-conref-flag t)))
       (t                               ; No attdecl, record attribute any way
	(push (psgml-make-attspec name val) asl))))
    asl))

(defun psgml-check-attribute-value-specification ()
  (or (psgml-parse-literal)
      (prog1 (psgml-parse-nametoken t)	; Not really a nametoken, but an
	(when psgml-pxml-p		; undelimited literal
	  (psgml-parse-warning "XML forbids undelimited literals.")))
      (psgml-parse-error "Expecting an attribute value: literal or token")))

(defun psgml-parse-attribute-value-specification (&optional warn)
  (or (psgml-parse-literal)
      (psgml-parse-nametoken t)		; Not really a nametoken, but an
					; undelimited literal
      (if warn
	  (progn
	    (psgml-log-warning "Expecting an attribute value: literal or token")
	    nil))))


(defun psgml-find-attdecl-for-value (value eltype)
  "Find the attribute declaration of ELTYPE that has VALUE in its name group.
VALUE is a string.  Returns nil or an attdecl."
  (let ((al (psgml-eltype-attlist eltype))
	dv)
    (while (and al
		(or (atom (setq dv (psgml-attdecl-declared-value (car al))))
		    (not (member value
				 (psgml-declared-value-token-group dv)))))
      (setq al (cdr al)))
    (if al (car al))))


;;;; Parser driver

;; The parser maintains a partial parse tree during the parse.  This tree
;; can be inspected to find information, and also be used to restart the
;; parse.  The parser also has a postition in the current content model.
;; (Called a state.)  The parser is used for several things:
;; 1) To find the state the parser would be in at a point in the buffer.
;;    (Point in emacs sense, I.e. between chararacters).
;; 2) Identify the element containing a character.
;; 3) Find end of an element.
;; 4) Find the next element.
;; 5) To find the previous element.

;; These tasks are done by a combination of parsing and traversing
;; the partial parse tree.  The primitive parse operation is to parse
;; until a goal point in the buffer has been passed.  In addition to
;; this it is possible to "trap" closing of elements.  Either for a
;; specific element or for any element.  When the trap is sprung the
;; parse is ended.  This is used to extend the parse tree.  When the
;; trap is used the parser is usually called with the end of the
;; buffer as the goal point.

(defun psgml-need-dtd ()
  "Make sure that an eventual DTD is parsed or loaded."
  (psgml-pop-all-entities)
  (psgml-cleanup-entities)
  (when (null psgml-buffer-parse-state)	; first parse in this buffer
    ;;(psgml-set-initial-state)		; fall back DTD
    (add-hook 'pre-command-hook 'psgml-reset-log)
    (make-local-variable 'psgml-auto-fill-inhibit-function)
    (setq psgml-auto-fill-inhibit-function (function psgml-in-prolog-p))
    (if psgml-default-dtd-file
	(psgml-load-dtd psgml-default-dtd-file)
      (psgml-load-doctype)))
  (psgml-debug "Need dtd getting state from %s" (buffer-name))
  (setq psgml-dtd-info (psgml-pstate-dtd psgml-buffer-parse-state)
	psgml-top-tree (psgml-pstate-top-tree psgml-buffer-parse-state))
  (psgml-set-global))


(defun psgml-load-doctype ()
  (cond
   ;; Case of doctype in another file
   ((or psgml-parent-document psgml-doctype)
    (let ((dtd
	   (save-excursion		; get DTD from parent document
	     (set-buffer (find-file-noselect
			  (if (consp psgml-parent-document)
			      (car psgml-parent-document)
			    (or psgml-doctype psgml-parent-document))))
	     (psgml-need-dtd)
	     (psgml-pstate-dtd psgml-buffer-parse-state))))
      (psgml-set-initial-state dtd)
      (when (consp psgml-parent-document) ; modify DTD for child documents
	(psgml-modify-dtd (cdr psgml-parent-document)))))

   ;; The doctype declaration should be in the current buffer
   (t
    (save-excursion (psgml-parse-prolog)))))


(defun psgml-modify-dtd (modifier)
  (setq psgml-dtd-info (psgml-pstate-dtd psgml-buffer-parse-state)
	psgml-top-tree (psgml-pstate-top-tree psgml-buffer-parse-state))
  (psgml-set-global)
  (setq psgml-current-tree psgml-top-tree)
  (while (stringp (cadr modifier))	; Loop thru the context elements
    (let ((et (psgml-lookup-eltype (car modifier))))
      (psgml-open-element et nil (point-min) (point-min))
      (setq modifier (cdr modifier))))

  (unless (stringp (car modifier))
    (error "wrong format of psgml-parent-document"))

  (let* ((doctypename (car modifier))
	 (et (psgml-lookup-eltype
	      (psgml-general-case (if (symbolp doctypename)
				     (symbol-name doctypename)
				   doctypename)))))

    (setq psgml-current-state
	  (psgml-make-primitive-content-token et))

    (when (consp (cdr modifier))	; There are "seen" elements
      (psgml-open-element et nil (point-min) (point-min))
      (loop for seenel in (cadr modifier)
	    do (setq psgml-current-state
		     (psgml-get-move psgml-current-state
				    (psgml-lookup-eltype seenel))))))

  (let ((top (psgml-pstate-top-tree psgml-buffer-parse-state)))
    (setf (psgml-tree-includes top) (psgml-tree-includes psgml-current-tree))
    (setf (psgml-tree-excludes top) (psgml-tree-excludes psgml-current-tree))
    (setf (psgml-tree-shortmap top) psgml-current-shortmap)
    (setf (psgml-eltype-model (psgml-tree-eltype top))
	  psgml-current-state)
    (setf (psgml-tree-content top) nil)))


(defun psgml-set-global ()
  (setq psgml-current-namecase-general psgml-namecase-general
	psgml-current-omittag psgml-omittag
	psgml-current-shorttag psgml-shorttag
	psgml-current-localcat psgml-local-catalogs
	psgml-current-local-ecat psgml-local-ecat-files
	psgml-current-top-buffer (current-buffer)
	psgml-markup-start nil))

(defun psgml-parse-prolog ()
  "Parse the document prolog to learn the DTD."
  (interactive)
  (psgml-debug "Parse prolog in buffer %s" (buffer-name))
  (unless psgml-debug
    (psgml-clear-log))
  (message "Parsing prolog...")
  (psgml-cleanup-entities)
  (psgml-set-global)
  (setq	psgml-dtd-info nil)
  (goto-char (point-min))
  (psgml-with-parser-syntax
   (while (progn (setq psgml-markup-start (point))
		 (or (psgml-parse-s)
		     (psgml-parse-processing-instruction)
		     (and (psgml-parse-markup-declaration 'prolog)
			  (null psgml-dtd-info)))))
   (unless psgml-dtd-info		; Set up a default doctype
     (let ((docname (or psgml-default-doctype-name
			(if (psgml-parse-delim "STAGO" gi)
			    (psgml-parse-name)))))
       (when docname
	 (psgml-setup-doctype docname '(nil))))))
  (unless psgml-dtd-info
    (error "No document type defined by prolog"))
  (psgml-message "Parsing prolog...done"))


(defun psgml-parse-until-end-of (psgml-close-element-trap &optional
							cont extra-cond quiet)
  "Parse until the PSGML-CLOSE-ELEMENT-TRAP has ended,
or if it is t, any additional element has ended,
or if nil, until end of buffer."
  (cond
   (cont (psgml-parse-continue (point-max)))
   (t    (psgml-parse-to (point-max) extra-cond quiet)))
  (when (eobp)				; End of buffer, can imply
					; end of any open element.
    (while (prog1 (not
		   (or (eq psgml-close-element-trap t)
		       (eq psgml-close-element-trap psgml-current-tree)
		       (eq psgml-current-tree psgml-top-tree)))
	     (psgml-implied-end-tag "buffer end" (point) (point))))))

(defun psgml-parse-to (psgml-goal &optional extra-cond quiet)
  "Parse until (at least) PSGML-GOAL.
Optional argument EXTRA-COND should be a function.  This function is
called in the parser loop, and the loop is exited if the function returns t.
If third argument QUIT is non-nil, no \"Parsing...\" message will be displayed."
  (psgml-need-dtd)

  (unless before-change-function
    (message "WARN: before-change-function has been lost, restoring (%s)"
	     (current-buffer))
    (setq before-change-function 'psgml-note-change-at)
    (setq after-change-function 'psgml-set-face-after-change)
    )

  (psgml-find-start-point (min psgml-goal (point-max)))
  (assert psgml-current-tree)
  (let ((bigparse (and (not quiet) (> (- psgml-goal (point)) 10000))))
    (when bigparse
      (psgml-message "Parsing..."))
    (psgml-with-parser-syntax
     (psgml-parser-loop extra-cond))
    (when bigparse
      (psgml-message ""))))

(defun psgml-parse-continue (psgml-goal &optional extra-cond quiet)
  "Parse until (at least) PSGML-GOAL."
  (assert psgml-current-tree)
  (unless quiet
    (psgml-message "Parsing..."))
  (psgml-with-parser-syntax
     (psgml-parser-loop extra-cond))
  (unless quiet
    (psgml-message "")))

(defun psgml-reparse-buffer (shortref-fun)
  "Reparse the buffer and let SHORTREF-FUN take care of short references.
SHORTREF-FUN is called with the entity as argument and `psgml-markup-start'
pointing to start of short ref and point pointing to the end."
  (psgml-note-change-at (point-min))
  (let ((psgml-shortref-handler shortref-fun))
    (psgml-parse-until-end-of nil)))

(defsubst psgml-move-current-state (token)
  (setq psgml-current-state
	(or (psgml-get-move psgml-current-state token)
	    psgml-current-state)))

(defun psgml-execute-implied (imps type)
  (loop for token in imps do
	(if (eq t token)
	    (psgml-implied-end-tag type psgml-markup-start psgml-markup-start)
	  (psgml-move-current-state token)
	  (when psgml-throw-on-element-change
	    (throw psgml-throw-on-element-change 'start))
	  (psgml-open-element (psgml-token-eltype token)
			     (and psgml-pxml-p
				  (eq psgml-empty
				      (psgml-eltype-model (psgml-token-eltype token))))
			     psgml-markup-start psgml-markup-start)
	  (unless (and psgml-current-omittag
		       (psgml-element-stag-optional psgml-current-tree))
	    (psgml-log-warning
	     "%s start-tag implied by %s; not minimizable"
	     (psgml-eltype-name (psgml-token-eltype token))
	     type)))))


(defun psgml-do-move (token type)
  (cond ((eq psgml-any psgml-current-state))
	(t
	 (let ((next-state (psgml-get-move psgml-current-state token)))
	   (cond (next-state
		  (setq psgml-current-state next-state))
		 (t
		  (psgml-execute-implied (psgml-list-implications token type) type)
		  (unless (eq psgml-any psgml-current-state)
		    (psgml-move-current-state token))))))))


(defun psgml-pcdata-move ()
  "Moify parser state to reflect parsed data."
  (psgml-do-move psgml-pcdata-token "data character"))

(defsubst psgml-parse-pcdata ()
  (/= 0
      (if psgml-current-shortmap
	  (skip-chars-forward (psgml-shortmap-skipstring psgml-current-shortmap))
	(skip-chars-forward "^<]/&"))))

(defsubst psgml-do-pcdata ()
  ;; Parse pcdata
  (psgml-pcdata-move)
  ;;*** assume psgml-markup-start = point
  ;;*** should perhaps handle &#nn;?
  (forward-char 1)
  (psgml-parse-pcdata)
  (when psgml-data-function
	(funcall psgml-data-function (buffer-substring-no-properties
				     psgml-markup-start
				     (point))))
  (psgml-set-markup-type nil))

(defun psgml-parser-loop (extra-cond)
  (let (tem
	(psgml-signal-data-function (function psgml-pcdata-move)))
    (while (and (eq psgml-current-tree psgml-top-tree)
		(or (< (point) psgml-goal) psgml-current-eref)
		(progn (setq psgml-markup-start (point)
			     psgml-markup-type nil)
		       (or (psgml-parse-s)
			   (psgml-parse-markup-declaration 'prolog)
			   (psgml-parse-processing-instruction)))))
    (while (and (or (< (point) psgml-goal) psgml-current-eref)
		(not (if extra-cond (funcall extra-cond))))
      (assert psgml-current-tree)
      (setq psgml-markup-start (point)
	    psgml-markup-type nil)
      (cond
       ((eobp) (psgml-pop-entity))
       ((and (or (eq psgml-current-state psgml-cdata)
		 (eq psgml-current-state psgml-rcdata)))
	(if (or (psgml-parse-delim "ETAGO" gi)
		(psgml-is-enabled-net))
	    (psgml-do-end-tag)
	  (psgml-do-data psgml-current-state)))
       ((and psgml-current-shortmap
	     (or (setq tem (psgml-deref-shortmap psgml-current-shortmap
						(eq (point)
						    psgml-rs-ignore-pos)))
		 ;; Restore position, to consider the delim for S+ or data
		 (progn (goto-char psgml-markup-start)
			nil)))
	(setq psgml-rs-ignore-pos psgml-markup-start) ; don't reconsider RS
	(funcall psgml-shortref-handler tem))
       ((and (not (psgml-current-mixed-p))
	     (psgml-parse-s psgml-current-shortmap)))
       ((or (psgml-parse-delim "ETAGO" gi)
	    (psgml-is-enabled-net))
	(psgml-do-end-tag))
       ((psgml-parse-delim "STAGO" gi)
	(psgml-do-start-tag))
       ((psgml-parse-general-entity-ref))
       ((psgml-parse-markup-declaration nil))
       ((psgml-parse-delim "MS-END")	; end of marked section
	(psgml-set-markup-type 'ms-end))
       ((psgml-parse-processing-instruction))
       (t
	(psgml-do-pcdata))))))

(defun psgml-handle-shortref (name)
  (psgml-set-markup-type 'shortref)
  (psgml-do-entity-ref name))

(defun psgml-do-start-tag ()
  ;; Assume point after STAGO
  (when psgml-throw-on-element-change
    (throw psgml-throw-on-element-change 'start))
  ;; Ugly? Let conref flag mean an empty element tag in XML mode,
  ;; then the node will be marked for special handling.
  (setq psgml-conref-flag nil)
  (let (net-enabled et asl)
    (setq et (if (psgml-is-delim "TAGC")	; empty start-tag
		 (psgml-do-empty-start-tag)
	       (psgml-lookup-eltype (psgml-check-name))))
    (unless (psgml-parse-delim "TAGC")	; optimize common case
      (setq asl (psgml-parse-attribute-specification-list et))
      (or
       (if (and (not psgml-pxml-p) (psgml-parse-delim "NET"))
	   (prog1 (setq net-enabled t)
	     (or psgml-current-shorttag
		 (psgml-log-warning
		  "NET enabling start-tag is not allowed with SHORTTAG NO"))))
       (if (and psgml-pxml-p (psgml-parse-delim "XML-TAGCE"))
	   (setq psgml-conref-flag t))
       (psgml-check-tag-close)))
    (psgml-set-markup-type 'start-tag)
    (cond ((and psgml-ignore-undefined-elements
		(not (psgml-eltype-defined et)))
	   (when psgml-warn-about-undefined-elements
	     (psgml-log-warning
	      "Start-tag of undefined element %s; ignored"
	      (psgml-eltype-name et))))
	  (t
	   (psgml-do-move (psgml-eltype-token et)
			 (format "%s start-tag" (psgml-eltype-name et)))
	   (psgml-open-element et psgml-conref-flag
			      psgml-markup-start (point) asl)
	   (when net-enabled            ;FIXME: why not argument to psgml-open-el?
	     (setf (psgml-tree-net-enabled psgml-current-tree) t))))))

(defun psgml-do-empty-start-tag ()
  "Return eltype to use if empty start tag"
  (cond
   ;; Document element if no element is open
   ((eq psgml-current-tree psgml-top-tree)
    (psgml-lookup-eltype
     (psgml-dtd-doctype psgml-dtd-info)))
   ;; If omittag use current open element
   (psgml-current-omittag
    (psgml-tree-eltype psgml-current-tree))
   ;; Find the eltype of the last closed element.
   ;; If element has a left sibling then use that
   (psgml-previous-tree
    (psgml-tree-eltype psgml-previous-tree))
   ;; No sibling, last closed must be found in enclosing element
   (t
    (loop named outer
	  for current = psgml-current-tree then (psgml-tree-parent current)
	  for parent  = (psgml-tree-parent current)
	  do;; Search for a parent with a child before current
	  (when (eq parent psgml-top-tree)
		(psgml-error "No previously closed element"))
	  (unless (eq current (psgml-tree-content parent))
	    ;; Search content of u for element before current
	    (loop for c = (psgml-tree-content parent) then (psgml-tree-next c)
		  do (when (eq current (psgml-tree-next c))
		       (return-from outer (psgml-tree-eltype c)))))))))


(defun psgml-do-end-tag ()
  "Assume point after </ or at / in a NET"
  (let ((gi "Null")			; Name of element to end or "NET"
	et				; Element type of end tag
	(found				; Set to true when found element to end
	 t))
    (cond ((psgml-parse-delim "TAGC")	; empty end-tag
	   (setq et (psgml-tree-eltype psgml-current-tree)))
	  ((psgml-parse-delim "NET"))
	  (t
	   (setq et (psgml-lookup-eltype (psgml-check-name)))
	   (psgml-parse-s)
	   (psgml-check-tag-close)))
    (psgml-set-markup-type 'end-tag)	; This will create the overlay for
					; the end-tag before the element
					; is closed
    (when et
      (setq gi (psgml-eltype-name et))
      (setq found			; check if there is an open element
					; with the right eltype
	    (loop for u = psgml-current-tree then (psgml-tree-parent u)
		  while u
		  thereis (eq et (psgml-tree-eltype u))))
      (unless found
	(psgml-log-warning
	 "End-tag %s does not end any open element; ignored"
	 gi)))
    (when found
      (setq found nil)
      (while (not found)		; Loop until correct element to
					; end is found
	(unless (psgml-final-p psgml-current-state)
	  (psgml-log-warning
	   "%s element can't end here, need one of %s; %s end-tag out of context"
	   (psgml-element-gi psgml-current-tree)
	   (psgml-required-tokens psgml-current-state)
	   gi))
	(when (eq psgml-current-tree psgml-top-tree)
	  (psgml-error "%s end-tag ended document and parse" gi))
	(setq found
	      (or (eq et (psgml-tree-eltype psgml-current-tree))
		  (and (null et)	; Null end-tag
		       (eq t (psgml-tree-net-enabled psgml-current-tree)))))
	(unless found
	  (psgml-implied-end-tag (format "%s end-tag" gi)
				psgml-markup-start psgml-markup-start)))
      (psgml-close-element psgml-markup-start (point)))))

(defun psgml-is-goal-after-start (goal tree)
  (and tree
       (if (psgml-bpos-p (psgml-tree-stag-epos tree))
	   (> goal (psgml-tree-stag-epos tree))
	 (>= goal (psgml-epos-after (psgml-tree-stag-epos tree))))))

(defun psgml-find-start-point (goal)
  (let ((u psgml-top-tree))
    (while
	(cond
	 ((psgml-is-goal-after-start goal (psgml-tree-next u))
	  (setq u (psgml-tree-next u)))
	 ((and (psgml-tree-etag-epos u)
	       (if (> (psgml-tree-etag-len u) 0) ; if threre is an end-tag
		   (>= goal (psgml-tree-end u))  ; precisely after is after
		 (> goal (psgml-tree-end u))))   ; else it could possibly
					; become part of the element
	  (psgml-set-parse-state u 'after)
	  nil)
	 ((psgml-is-goal-after-start goal (psgml-tree-content u))
	  (setq u (psgml-tree-content u)))
	 (t
	  (psgml-set-parse-state u 'start)
	  nil)))))


(defun psgml-check-tag-close ()
  (or
   (psgml-parse-delim "TAGC")
   (if (or (psgml-is-delim "STAGO" gi)
	   (psgml-is-delim "ETAGO" gi))
       (or psgml-current-shorttag
	   (psgml-log-warning
	    "Unclosed tag is not allowed with SHORTTAG NO")
	   t))
   (psgml-log-warning "Invalid character in markup %c"
		     (following-char))))

(defun psgml-implied-end-tag (type start end)
  (cond ((eq psgml-current-tree psgml-top-tree)
	 (unless (= start (point-max))
	   (psgml-error
	    "document ended by %s" type)))
	((not
	  (and psgml-current-omittag
	       (psgml-element-etag-optional psgml-current-tree)))
	 (psgml-log-warning
	  "%s end-tag implied by %s; not minimizable"
	  (psgml-element-gi psgml-current-tree)
	  type)))
  (psgml-close-element start end))


;;;; Parsing tasks and extending the element view of the parse tree

(defun psgml-find-context-of (pos)
  "Find the parser context for POS, returns the parse tree.
Also sets psgml-current-tree and psgml-current-state.  If POS is in
markup, psgml-markup-type will be a symbol identifying the markup
type.  It will be nil otherwise."
  (save-excursion
    (psgml-parse-to pos)
    (cond ((and (> (point) pos)
		psgml-markup-type)
	   ;;(setq psgml-current-state psgml-markup-type)
	   (cond ((memq psgml-markup-type '(start-tag end-tag))
		  (setq psgml-current-tree psgml-markup-tree))))
	  (t
	   (setq psgml-markup-type nil)))
    psgml-current-tree))

(defun psgml-parse-to-here ()
  "Find context of point.
See documentation of psgml-find-context-of."
  (psgml-find-context-of (point)))

(defun psgml-find-element-of (pos)
  "Find the element containing character at POS."
  (when (eq pos (point-max))
    (error "End of buffer"))
  (save-excursion
    (psgml-parse-to (1+ pos))		; Ensures that the element is
					; in the tree.
    ;;  Find p in u:
    ;;  assert p >= start(u)
    ;;  if next(u) and p >= start(next(u)): find p in next(u)
    ;;  else if end(u) and p >= end(u): in parent(u) unless u is top
    ;;  else if content:
    ;;    if p < start(content(u)): in u
    ;;    else find p in content(u)
    ;;  else: in u
    (let ((u psgml-top-tree))
      (while				; pos >= start(u)
	  (cond ((and (psgml-tree-next u)
		      (>= pos (psgml-element-start (psgml-tree-next u))))
		 (setq u (psgml-tree-next u))) ; continue searching next node
		((and (psgml-tree-etag-epos u)
		      (>= pos (psgml-tree-end u)))
		 (setq u (psgml-tree-parent u)) ; must be parent node
		 nil)
		((and (psgml-tree-content u)
		      (>= pos (psgml-element-start (psgml-tree-content u))))
		 (setq u (psgml-tree-content u))))) ; search content
      u)))

(defun psgml-find-previous-element (pos &optional in-element)
  "Find the element before POS and return it, error if non found.
If in IN-ELEMENT is given look for previous element in IN-ELEMENT else
look in current element.  If this element has no content elements but
end at POS, it will be returned as previous element."
  (save-excursion
    ;; Parse to point; now the previous element is in the parse tree
    (psgml-parse-to pos)
    ;; containing element may be given or obtained from parser
    (or in-element (setq in-element psgml-current-tree))
    ;; in-element is the containing element
    (let* ((c				; this is the content of the
					; containing element
	    (psgml-tree-content in-element)))
      (while
	  (cond
	   ((null c)			; If c = Nil: no previous element.
	    ;; But maybe the containing element ends at pos too.
	    (cond ((= pos (psgml-element-end in-element))
		   (setq c in-element))) ; Previous is parent!
	    nil)
	   ((<= pos (psgml-element-start c)) ; Pos before first content el
	    (setq c nil))		; No, previous element.
	   ((null (psgml-tree-next c)) nil) ; No next, c must be the prev el
	   ((>= (psgml-element-start (psgml-tree-next c)) pos)
	    nil)
	   (t
	    (setq c (psgml-tree-next c)))))
      (or c
	  (error "No previous element in %s element"
		 (psgml-element-gi in-element))))))

(defun psgml-find-element-after (pos &optional in-element)
  "Find the first element starting after POS.
Returns parse tree; error if no element after POS."
  (setq in-element (or in-element
		       (save-excursion (psgml-find-context-of pos))))
  (or
   ;; First try to find element after POS in IN-ELEMENT/current element
   (let ((c				; content of in-element
	  (psgml-element-content in-element)))
     (while (and c
		 (> pos (psgml-element-start c)))
       (setq c (psgml-element-next c)))
     c)
   ;; If there is no more elements IN-ELEMENT/current element try
   ;; to identify the element containing the character after POS.
   ;; If this element starts at POS, use it for element after POS.
   (let ((el (psgml-find-element-of pos)))
     (if (and el (= pos (psgml-element-start el)))
	 el))
   (progn
     (psgml-message "")			; force display of log buffer
     (error "No more elements in %s element"
	    (psgml-element-gi in-element)))))

(defun psgml-element-content (element)
  "First element in content of ELEMENT, or nil."
  (when (null (or (psgml-tree-content element)
		  (psgml-tree-etag-epos element)))
    (save-excursion (psgml-parse-until-end-of t)))
  (psgml-tree-content element))

(defun psgml-element-next (element)
  "Next sibling of ELEMENT."
  (unless (psgml-tree-etag-epos element)
    (save-excursion (psgml-parse-until-end-of element)))
  (unless (or (psgml-tree-next element)
	      (psgml-tree-etag-epos (psgml-tree-parent element)))
    (save-excursion (psgml-parse-until-end-of t)))
  (psgml-tree-next element))

(defun psgml-element-etag-start (element)
  "Last position in content of ELEMENT and start of end-tag, if any."
  (unless (psgml-tree-etag-epos element)
    (save-excursion
      (psgml-parse-until-end-of element)))
  (assert (psgml-tree-etag-epos element))
  (psgml-epos-promote (psgml-tree-etag-epos element)))

(defun psgml-element-end (element)
  "First position after ELEMENT."
  (psgml-element-etag-start element)	; make end be defined
  (psgml-tree-end element))

(defun psgml-read-element-name (prompt)
  (psgml-parse-to-here)
  (cond (psgml-markup-type
	 (error "No elements allowed in markup"))
	((eq psgml-current-state psgml-any)
	 ;; FIXME: should perhaps allow nondefined elements if here is
	 ;; no DTD
	 (psgml-read-element-type prompt
				 psgml-dtd-info))
	((and;;psgml-buffer-eltype-map
	  (not (eq psgml-current-state psgml-any)))
	 (let ((tab
		(mapcar (function
			 (lambda (x)
			   (cons (psgml-general-insert-case (symbol-name x))
				 nil)))
			(psgml-current-list-of-valid-eltypes))))
	   (cond ((null tab)
		  (error "No element valid at this point"))
		 (t
		  (let ((completion-ignore-case psgml-namecase-general))
		    (completing-read prompt tab nil t
				     (and (null (cdr tab)) (caar tab))))))))
	(t
	 (read-from-minibuffer prompt))))

(defun psgml-element-attribute-specification-list (element)
  "Return the attribute specification list for ELEMENT.
This is a list of (attname value) lists."
;;;  (if (> (psgml-element-stag-len element) 2)
;;;      (save-excursion
;;;	(psgml-with-parser-syntax
;;;	 (psgml-goto-epos (psgml-element-stag-epos element))
;;;	 (psgml-check-delim "STAGO")
;;;	 (psgml-check-name)
;;;	 (prog1 (psgml-parse-attribute-specification-list
;;;		 (psgml-element-eltype element))
;;;	   (psgml-pop-all-entities)))))
  (psgml-tree-asl element))

(defun psgml-find-attribute-element ()
  "Return the element to which an attribute editing command should be applied."
  (let ((el (psgml-find-element-of (point))))
    (save-excursion
      (psgml-parse-to (point))
      ;; If after a start-tag of an empty element return that element
      ;; instead of current element
      (if (eq psgml-markup-type 'start-tag)
	  psgml-markup-tree		; the element of the start-tag
	el))))


(defun psgml-element-attval (element attribute)
  "Return the value of the ATTRIBUTE in ELEMENT, string or nil."
  (let ((asl (psgml-element-attribute-specification-list element))
	(def (psgml-attdecl-default-value
	      (psgml-lookup-attdecl attribute (psgml-element-attlist element)))))
    (or (psgml-attspec-attval (psgml-lookup-attspec attribute asl))
	(psgml-default-value-attval def))))


(defun psgml-cohere-name (x)
  "Convert X into a string where X can be a string, a symbol or an element."
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	(t (psgml-element-gi x))))

(defun psgml-start-tag-of (element)
  "Return the start-tag for ELEMENT."
  (if (and psgml-pxml-p (psgml-check-empty (psgml-cohere-name element)))
      (format "<%s/>" (psgml-general-insert-case (psgml-cohere-name element)))
    (format "<%s>" (psgml-general-insert-case (psgml-cohere-name element)))))

(defun psgml-end-tag-of (element)
  "Return the end-tag for ELEMENT (token or element)."
  (format "</%s>" (psgml-general-insert-case (psgml-cohere-name element))))

(defun psgml-top-element ()
  "Return the document element."
  (psgml-element-content (psgml-find-context-of (point-min))))

(defun psgml-off-top-p (element)
  "True if ELEMENT is the pseudo element above the document element."
  (null (psgml-tree-parent element)))

(defun psgml-safe-context-of (pos)
  (let ((psgml-throw-on-error 'parse-error))
    (catch psgml-throw-on-error
      (psgml-find-context-of pos))))

(defun psgml-safe-element-at (pos)
  (let ((psgml-throw-on-error 'parse-error))
    (catch psgml-throw-on-error
      (if (= pos (point-max))
	  (psgml-find-context-of pos)
	(psgml-find-element-of pos)))))

(defun psgml-in-prolog-p ()
  (let ((el (psgml-safe-context-of (point))))
    (or (null el)
	(psgml-off-top-p el))))


;;;; Provide

(provide 'psgml-parse)

;; Local variables:
;; byte-compile-warnings:(free-vars unresolved callargs redefine)
;; End:
;;; psgml-parse.el ends here
