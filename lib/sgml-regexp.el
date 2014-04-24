;;;; sgml-regexp.el -- Match SGML tags and entity references.

;;; RCS $Id: sgml-regexp.el,v 1.2 1998/03/18 20:30:28 kevinr Exp $

;;; Description:
;;; 
;;; Define variables for the SGML (ISO 8879:1986) syntactic variables
;;; that can be used by Emacs' regular expression matching functions to
;;; search for occurrences of start-tags, end-tags, and general entity
;;; references.  These regular expressions are designed to show how the
;;; standard defines the markup syntax, and for robust -- but not
;;; necessarily efficient -- searching.
;;; 
;;; For example:
;;; 	(let ((html-tag (concat sgml-regexp-start-tag
;;; 				"\\|"
;;; 				sgml-regexp-end-tag)))
;;; 	  (while (re-search-forward html-tag nil t)
;;; 	    (replace-match "")))

;;; Copyright:
;;; 
;;; Copyright © 1998 Kevin Rodgers
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; 
;;; My employer (Information Handling Services) has not disclaimed any
;;; copyright interest in sgml-regexp.el.
;;; 
;;; Kevin Rodgers <kevinr@ihs.com>          Lead Software Engineer
;;; Information Handling Services           Electronic Systems Development
;;; 15 Inverness Way East, M/S A201         GO BUFFS!
;;; Englewood CO 80112-5776 USA             1+ (303) 397-2807[voice]/-2244[fax]


;;; Code:

(defconst sgml-regexp-s
  "[ \r\n\t]"				; SPACE | RE | RS | SEPCHAR
  "A regular expression matching an SGML `s' separator.
See ISO 8879:1986 clause 6.2.1, production [5].")


(defconst sgml-regexp-name-start-character
  "[a-zA-Z]"				; LC Letter | UL Letter |
					; LCNMSTRT | UCNMSTRT
  "A regular expression matching an SGML `name start character'.
See ISO 8879:1986 clause 9.2.1, production [53].")


(defconst sgml-regexp-name-character
  "[a-zA-Z0-9---.]"			; name start character | Digit |
					; LCNMCHAR | UCNMCHAR
  "A regular expression matching an SGML `name character'.
See ISO 8879:1986 clause 9.2.1, production [52].")


(defconst sgml-regexp-name
  (concat "\\(" sgml-regexp-name-start-character
	        sgml-regexp-name-character "*"
	  "\\)")
  "A regular expression matching an SGML `name'.
See ISO 8879:1986 clause 9.3, production [55].")


(defconst sgml-regexp-generic-identifier-specification
  sgml-regexp-name
  "A regular expression matching an SGML `generic identifier specification'.
See ISO 8879:1986 clause 7.8, productions [29] and [30].

This pattern does not match the `rank stem' alternative syntactic token.")


(defconst sgml-regexp-attribute-value
  (concat "\\(" sgml-regexp-name-character "+" "\\)")
  "A regular expression matching an SGML `attribute value'.
See ISO 8879:1986 clause 7.9.3.1 and clause 7.9.4, production [35].

This pattern matches a sequence of `name character's instead of the
`character data', ..., and `number token list' alternative syntactic
tokens.")


(defconst sgml-regexp-attribute-value-literal
  (concat "\\(" "\"[^\"]*\"" "\\|"
	        "'[^']*'"
	  "\\)")
  "A regular expression matching an SGML `attribute value literal'.
See ISO 8879:1986 clause 7.9.3, production [34].

This pattern matches `lit' or `lita' delimited sequences of any
characters (except the delimiters), instead of delimited sequences of
`replaceable character data'.")


(defconst sgml-regexp-attribute-value-specification
  (concat "\\(" sgml-regexp-attribute-value "\\|"
	        sgml-regexp-attribute-value-literal
	  "\\)")
  "A regular expression matching an SGML `attribute value specification'.
See ISO 8879:1986 clause 7.9.3, production [33].")


(defconst sgml-regexp-attribute-specification
  (concat "\\(" sgml-regexp-s "*"
	        "\\(" sgml-regexp-name sgml-regexp-s "*" "=" sgml-regexp-s "*"
		"\\)" "?"
		sgml-regexp-attribute-value-specification
	  "\\)")
  "A regular expression matching an SGML `attribute specification'.
See ISO 8879:1986 clause 7.9, production [32].")


(defconst sgml-regexp-attribute-specification-list
  (concat "\\(" sgml-regexp-attribute-specification "*" "\\)")
  "A regular expression matching an SGML `attribute specification list'.
See ISO 8879:1986 clause 7.9, production [31].")


(defconst sgml-regexp-start-tag
  (concat "<"
	  ;; sgml-regexp-document-type-specification
	  sgml-regexp-generic-identifier-specification
	  sgml-regexp-attribute-specification-list
	  sgml-regexp-s "*"
	  ">")
  "A regular expression matching an SGML `start-tag'.
See ISO 8879:1986 clause 7.4.1, production [14].

This pattern does not match an SGML `document type specification', or the
`minimized start-tag' alternative syntactic token.")


(defconst sgml-regexp-end-tag
  (concat "</"
	  ;; sgml-regexp-document-type-specification
	  sgml-regexp-generic-identifier-specification
	  sgml-regexp-s "*"
	  ">")
  "A regular expression matching an SGML `end-tag'.
See ISO 8879:1986 clause 7.5, production [19].

This pattern does not match a `document type specification', or the
`minimized end-tag' alternative syntactic token.")


(defconst sgml-regexp-reference-end
  "[;\r]"				; refc | RE
  "A regular expression matching an SGML `reference end'.
See ISO 8879:1986 clause 9.4.5, production [61].

This pattern does not match an omitted `refc' or `RE'.")


(defconst sgml-regexp-general-entity-reference
  (concat "&"
	  ;; sgml-regexp-name-group "?"
	  sgml-regexp-name
	  sgml-regexp-reference-end)
  "A regular expression matching an SGML `general entity reference'.
See ISO 8879:1986 clause 9.4.4, production [59].

This pattern does not match an SGML `name group'.")


(defconst sgml-regexp-function-name
  (concat "\\(" "RE" "\\|"
	        "RS" "\\|"
		"SPACE" "\\|"
		"TAB \\| SEPCHAR"	; sgml-regexp-name
	  "\\)")
  "A regular expression matching an SGML `function name'.
See ISO 8879:1986 clause 9.5, production [63].")


(defconst sgml-regexp-number
  "[0-9]+"				; Digit +
  "A regular expression matching an SGML `number'.
See ISO 8879:1986 clause 9.3, production [56].")


(defconst sgml-regexp-character-number
  sgml-regexp-number
  "A regular expression matching an SGML `character number'.
See ISO 8879:1986 clause 9.5, production [64].")


(defconst sgml-regexp-character-reference
  (concat "#"
	  "\\(" sgml-regexp-function-name "\\|"
	        sgml-regexp-character-number
	  "\\)"
	  sgml-regexp-reference-end)
  "A regular expression matching an SGML `character reference'.
See ISO 8879:1986 clause 9.5, production [62].")

;;;; sgml-regexp.el ends here
