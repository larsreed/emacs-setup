;;; sysdul.el --- Sysdul mode for GNU Emacs

;; Author: Olav M. Bjørnås <omb@sysdeco.no>
;; Author-2: Lars Reed <Lars@kalars.net>
;; Version 2014.01
;; Keywords: languages Sysdul
;;
;; Added changes from Øyvind Mossin <omo@sysdeco.no>
;; Suppport for new Systemator 5.0 syntax
;; Version 2.0.0
;;
;; Max line length is now 200 chars, fill-column set to 100
;; C is no longer a Comment start in column 1
;; Version 2.1.0 (Feb 20, 1996)
;;
;; Lars Reed
;; 1.  Added tplsub-interface
;; 2.  Version- and language dependent regexps.
;; 3.  GNU Emacs adaption (19.34).
;; 4.  sysdul-std-indent for quick customization.
;; 5.  More font-lock keywords, with tool version and -language dependency
;; 6.  sysdul-comment-box, sysdul-insert-*, sysdul-fix-white,
;;     sysdul-split-long, sysdul-next-error
;; 7.  Misc. cleanup.
;;
;; Revision 1.5  2005/10/19 05:19:17  larsr
;; run-mode-hooks
;;
;; Revision 1.4  2004/09/10 19:24:52  larsr
;; keywords
;;
;; Revision 1.3  2004/04/12 18:38:38  larsr
;; Tillegg for Genova
;;
;; Revision 1.2  2004/03/15 21:30:04  larsr
;; LF
;;
;; Revision 1.1.1.1  2004/03/15 20:50:35  larsr
;; Initial RCS revision
;;
;;Revision 3.6  1970/01/01  00:00:00  larsr
;;lretpl -> tplsub
;;
;;Revision 3.5  1998/12/06  23:26:46  larsr
;;korr.
;;
;;   Revision 3.4  1998/12/06  23:11:40  larsr
;;   easy-menu
;;
;;   Revision 3.3  1998/04/18  12:43:14  larsr
;;   Revision 3.2  1998/04/18  12:41:54  larsr
;;
;;
;;; Based on fortran.el - see comment block at end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (c) 1986, 1993, 1994 Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'cl)

(defconst sysdul-mode-version "version 2.1.0")
(defconst sysdul--local-version "$Revision: 1.5 $")
(defconst sysdul-local-version (concat
				"LRE v"
				(substring sysdul--local-version 11
					   (- (length sysdul--local-version)
					      2)))
  "Local modification ID.  The following synchronizations apply:
v1.1 == Sysdeco v2.1.0 \(Systemator 5.1 upgrade 1\)
v1.2 .. v1.39 -> Local additions")

;;; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;;; Code:

;; Make sure custom macros exist
(eval-and-compile
  (if (and (featurep 'custom)
	   (fboundp 'custom-declare-variable))
      nil
         (defmacro defgroup (&rest args) nil)
         (defmacro defface  (var values doc &rest args) `(make-face ,var))
         (defmacro defcustom (var value doc &rest args)
           `(defvar ,var ,value ,doc))))

(defgroup sysdul nil
  "Sysdul editing"
  :group 'languages
  :prefix "sysdul-")

(defcustom sysdul-tool-version (cond ((getenv "GENOVA8_HOME") 8)
                                     ((getenv "GENOVA_HOME") 7)
                                     ((getenv "SYSTEMATOR5") 5)
                                     (t 4))
  "* Version of Systemator used for programming.
Defaults to 7 if GENOVA_HOME is set, 5 if $SYSTEMATOR5 exists in the
environment, otherwise 4 \(which is also compatible with older versions\)"
  :type 'integer
  :group 'sysdul)

(defcustom sysdul-lang (if (< sysdul-tool-version 5) ?n ?e)
  "* Norwegian or english sysdul, represented by ?n or ?e.
Defaults to ?e iff `sysdul-tool-version'>=5."
  :type 'integer
  :group 'sysdul)

(defconst sysdul-gnuemacs-p (not (string-match "XEmacs\\|Lucid" emacs-version))
  "t if this is GNU emacs")

(defcustom sysdul-local-key-function nil
  "Local key setup function.
Called with 1 arg - keymap."
  :type 'symbol
  :group 'sysdul)

(defcustom sysdul-local-lib nil
  "Local library to load if non-nil"
  :type 'file
  :group 'sysdul)

;; Expansion

(defvar sysdul-he-list-tmp)

(defcustom sysdul-he-files nil
  "* File(s) containing repository data
The file should be a lisp file setting the variable `sysdul-he-list-tmp' to a
list, where each element has the form (\"NAME\" . \"TYPE\"), both strings in
caps.
The type is currently not used, but should be P for procedures."
  :group 'sysdul
  :type '(repeat file))

(defcustom sysdul-he-modes '(sysdul-mode)
  "* Modes in which sysdul should be hippie..."
  :type '(repeat symbol)
  :group 'sysdul)

(defvar sysdul-he-list nil
  "Expansion list for Sysdul - read from file.")

;; Indentation variables

(defcustom sysdul-std-indent 2
  "* Default indentation.
This is used by all indentation variables for which no explicit value has
been set."
  :type 'integer
  :group 'sysdul)

(defcustom sysdul-while-indent nil
  "*Extra indentation applied to WHILE blocks."
  :type '(choice (const :tag "none" nil)
		 integer)
  :group 'sysdul)

(defcustom sysdul-if-indent nil
  "*Extra indentation applied to IF blocks."
  :type '(choice (const :tag "none" nil)
		 integer)
  :group 'sysdul)

(defcustom sysdul-trans-indent nil
  "*Extra indentation applied to TRANSACTION blocks."
  :type '(choice (const :tag "none" nil)
		 integer)
  :group 'sysdul)

(defcustom sysdul-proc-indent nil
  "*Extra indentation applied to PROCEDURE blocks."
  :type '(choice (const :tag "none" nil)
		 integer)
  :group 'sysdul)

(defcustom sysdul-for-indent nil
  "*Extra indentation applied to FOR blocks."
  :type '(choice (const :tag "none" nil)
		 integer)
  :group 'sysdul)

(defcustom sysdul-comment-line-extra-indent nil
  "*Amount of extra indentation for text within full-line comments."
  :type '(choice (const :tag "none" nil)
		 integer)
  :group 'sysdul)
(make-variable-buffer-local 'sysdul-comment-line-extra-indent)

(defcustom sysdul-inhibit-templates nil
  "*Set to t to disable template/syntax help interface")

(defmacro sysdul-set-if-nil (var val)
  "Macro to set VAR to VAL iff VAR is nil."
  `(if (null ,var) (setq ,var ,val)))

(defun sysdul-set-indent ()
  "Set indent when no explicit value is given"
  (sysdul-set-if-nil sysdul-std-indent 2)
  (sysdul-set-if-nil sysdul-while-indent sysdul-std-indent)
  (sysdul-set-if-nil sysdul-if-indent sysdul-std-indent)
  (sysdul-set-if-nil sysdul-trans-indent sysdul-std-indent)
  (sysdul-set-if-nil sysdul-proc-indent sysdul-std-indent)
  (sysdul-set-if-nil sysdul-for-indent sysdul-std-indent)
  (sysdul-set-if-nil sysdul-continuation-indent sysdul-std-indent)
  (sysdul-set-if-nil sysdul-comment-line-extra-indent sysdul-std-indent))

(defcustom sysdul-continuation-indent nil
  "*Extra indentation applied to Sysdul continuation lines."
  :type '(choice (const :tag "none" nil)
		 integer)
  :group 'sysdul)

(defcustom sysdul-comment-indent-style 'fixed
  "*nil forces comment lines not to be touched,
'fixed makes fixed comment indentation to `sysdul-comment-line-extra-indent'
columns beyond `sysdul-minimum-statement-indent-fixed' (for
`indent-tabs-mode' of nil) or `sysdul-minimum-statement-indent-tab' (for
`indent-tabs-mode' of t), and 'relative indents to current
Sysdul indentation plus `sysdul-comment-line-extra-indent'."
  :type '(choice (const :tag "Leave alone" nil)
		 (const :tag "Fixed" fixed)
		 (const :tag "Relative" relative))
  :group 'sysdul)

(defvar comment-line-start nil
  "*Delimiter inserted to start new full-line comment.")
(make-variable-buffer-local 'comment-line-start)

(defvar comment-line-start-skip nil
  "*Regexp to match the start of a full-line comment.")
(make-variable-buffer-local 'comment-line-start-skip)

(defcustom sysdul-minimum-statement-indent-fixed 0
  "*Minimum statement indentation for fixed format continuation style."
  :type 'integer
  :group 'sysdul)

(defcustom sysdul-minimum-statement-indent-tab (max tab-width 6)
  "*Minimum statement indentation for TAB format continuation style."
  :type 'integer
  :group 'sysdul)

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defcustom sysdul-comment-indent-char " "
  "*Single-character string inserted for Sysdul comment indentation.
Normally a space."
  :type 'string
  :group 'sysdul)

(defcustom sysdul-blink-matching t
  "*From a Sysdul END statement, blink the matching starting statement."
  :type 'boolean
  :group 'sysdul)

(defcustom sysdul-continuation-string ":"
  "*Single-character string used for Sysdul continuation lines.
In fixed format continuation style, this character is inserted at
the end by \\[sysdul-split-line] to begin a continuation line.
Also, if \\[sysdul-indent-line] finds this at the end of a line, it will
convert the next line into a continuation line of the appropriate style.
Normally :."
  :type 'string
  :group 'sysdul)

(defcustom sysdul-comment-region "; "
  "*String inserted by \\[sysdul-comment-region]\
 at start of each line in region."
  :type 'string
  :group 'sysdul)

(defcustom sysdul-comment-column 48
  "*Indentation column for \\[indent-for-comment]"
  :type 'integer
  :group 'sysdul)

(defcustom sysdul-imenu-levels '(1 2)
  "*What expressions to include in imenu-generic-expression"
  :type '(repeat integer)
  :group 'sysdul)

(defcustom sysdul-startup-message t
  "*Non-nil displays a startup message when Sysdul mode is first called."
  :type 'boolean
  :group 'sysdul)

(defconst bug-sysdul-mode "omb@genera.no"
  "Address of mailing list for Sysdul mode bugs.")

(defvar sysdul-mode-syntax-table nil
  "Syntax table in use in Sysdul mode buffers.")

(defcustom sysdul-break-before-delimiters t
  "*Non-nil causes `sysdul-do-auto-fill' to break lines before delimiters."
  :type 'boolean
  :group 'sysdul)
(make-variable-buffer-local 'sysdul-break-before-delimiters)

(defcustom sysdul-fill-column 100
  "* fill-column value for Sysdul files"
  :type 'integer
  :group 'sysdul)

(defcustom sysdul-mode-name "Sysdul"
  "Name for mode-line."
  :type 'string
  :group 'sysdul)

(defvar sysdul-window-split 80
  "Used in `sysdul-window-create'")

;; Syntax regexps
;; Build regexps when loading, using make-regexp for speed
;;   (require 'make-regexp)

(defconst sysdul-RE-end
  (if (= sysdul-lang ?e) "end +" "slutt +"))

(defconst sysdul-RE-subr
  (cond ((> sysdul-tool-version 4) "control-\\(handling\\|mainprocedure\\|procedure\\)\\|event-\\(handler\\|mainprocedure\\|procedure\\)\\|mainprocedure\\|pro\\(cedure\\|gram\\)")
	((= sysdul-lang ?e) "control-\\(handling\\|mainprocedure\\|procedure\\)\\|mainprocedure\\|pro\\(cedure\\|gram\\)")
	(t "hovedprosedyre\\|kontroll-\\(administrasjon\\|hovedprosedyre\\|prosedyre\\)\\|pro\\(gram\\|sedyre\\)")))

(defconst sysdul-RE-subr-start
  (concat "^ *\\(" sysdul-RE-subr "\\)"))

(defconst sysdul-RE-subr-end
  (concat "^ *" sysdul-RE-end "\\(" sysdul-RE-subr "\\)"))

(defconst sysdul-RE-subr-start-or-end
  (concat "^ *\\("
	  (cond ((> sysdul-tool-version 4)
		 "control-\\(handling\\|mainprocedure\\|procedure\\)\\|e\\(nd +\\(control-\\(handling\\|procedure\\)\\|event-\\(handler\\|procedure\\)\\|pro\\(cedure\\|gram\\)\\)\\|vent-\\(handler\\|mainprocedure\\|procedure\\)\\)\\|mainprocedure\\|pro\\(cedure\\|gram\\)")
		((= sysdul-lang ?e)
		 "control-\\(handling\\|mainprocedure\\|procedure\\)\\|end +\\(control-\\(handling\\|procedure\\)\\|pro\\(cedure\\|gram\\)\\)\\|mainprocedure\\|pro\\(cedure\\|gram\\)")
		(t "hovedprosedyre\\|kontroll-\\(administrasjon\\|hovedprosedyre\\|prosedyre\\)\\|pro\\(gram\\|sedyre\\)\\|slutt +\\(kontroll-\\(administrasjon\\|prosedyre\\)\\|pro\\(gram\\|sedyre\\)\\)"))
	  "\\)\\( +\\(\\w\\|\\s_\\)+\\)?"))

(defconst sysdul-RE-trans
  (if (= sysdul-lang ?e) " +transaction\\b" " +transaksjonsenhet\\b"))

(defconst sysdul-RE-loop-start
  (concat "\\b"
	   (if (= sysdul-lang ?e)
;;	  \(make-regexp
;;	       '\("if" "while" "for +all" "for +those" "for" "read +all"
;;		 "start +loop" "start +transaction"\) t\)
	       "\\(for\\(\\| +\\(all\\|those\\)\\)\\|if\\|read +all\\|start +\\(loop\\|transaction\\)\\|while\\)"
;;	  \(make-regexp
;;		 '\("for +alle" "for +de" "for" "les +alle" "hvis" "mens"
;;		   "start +transaksjonsenhet" "start +l.kke +for +oppdatering"
;;		   "start +l.kke"\) t\)
	     "\\(for\\(\\| +\\(alle\\|de\\)\\)\\|hvis\\|les +alle\\|mens\\|start +\\(l.kke\\(\\| +for +oppdatering\\)\\|transaksjonsenhet\\)\\)"
	  ) "\\b"))

(defconst sysdul-RE-loop-end
  (concat sysdul-RE-end
	   (if (= sysdul-lang ?e)
;; \(make-regexp '\("if" "while" "for" "read" "loop" "transaction"\) t\)
	       "\\(for\\|if\\|loop\\|read\\|transaction\\|while\\)"
;; \(make-regexp '\("for" "hvis" "mens" "l.kke" "transaksjonsenhet"\) t\)
	     "\\(for\\|hvis\\|l.kke\\|mens\\|transaksjonsenhet\\)"
	     ) "\\b"))

(defconst sysdul-RE-loop-or-subr-start
  (concat "\\(\\(" sysdul-RE-loop-start "\\)\\|"
	  sysdul-RE-subr-start "\\)\\b"))

(defconst sysdul-RE-if
  (if (= sysdul-lang ?e) "\\bif\\b" "\\bhvis\\b"))

(defconst sysdul-RE-or-if
  (concat (if (= sysdul-lang ?e) "\\bor\\b +" "\\beller\\b +")
	  sysdul-RE-if))

(defconst sysdul-RE-else
  (if (= sysdul-lang ?e) "\\belse\\b" "\\bellers\\b"))

(defconst sysdul-RE-while
  (if (= sysdul-lang ?e) "\\bwhile\\b" "\\bmens\\b"))

(defconst sysdul-RE-goto+label
  (if (= sysdul-lang ?e)
      "\\(terminate\\|restart\\)"
    "\\(avslutt\\|restart\\)"))

(defconst sysdul-RE-goto-and-label
  (concat sysdul-RE-goto+label
	  (if (= sysdul-lang ?e)
	      "\\( +from\\|-label\\)"
	    "\\( +fra\\|-punkt\\)")))

(defconst sysdul-RE-label
  (concat sysdul-RE-goto+label
	  (if (= sysdul-lang ?e) "-label" "-punkt")
	  "\\b"))

(defconst sysdul-RE-spaghetti
  (concat "\\(exit +[0-9]+ +"
	  (if (= sysdul-lang ?e)
	      "levels?"
	    "niv.(er)?")
	  "\\)\\|\\("
	  (if (= sysdul-lang ?e)
	      "\\(terminate\\|continue\\) +loop"
	    "\\(avslutt\\|fortsett\\) +l.kke")
	  "\\)\\|\\("
	  (if (= sysdul-lang ?e)
	      "return"
	    "returner")
	  "\\)"))

(defconst sysdul-RE-call
  (if (= sysdul-lang ?e) "call" "kall"))

(defconst sysdul-RE-identify
  (if (= sysdul-lang ?e)
      "identify +\\(all\\|first\\|last\\|next\\|previous\\|current\\)"
    "velg +\\(alle\\|f.rste\\|siste\\|neste\\|forrige\\)"))

(defconst sysdul-RE-db-stmnt
  (if (= sysdul-lang ?e)
;; \(make-regexp '\("find +first" "find +last" "find" "get" "store"
;;    "remember" "forget" "delete" "delete all" "update +all"\)\)
      "delete\\(\\| all\\)\\|f\\(ind\\(\\| +\\(first\\|last\\)\\)\\|orget\\)\\|get\\|remember\\|store\\|update +all"
;; \(make-regexp '\("finn +f.rste" "finn +siste" "finn" "hent" "lagre" "husk"
;;    "husk +at" "glem" "glem +at" "slett"\)\)
    "finn\\(\\| +\\(f.rste\\|siste\\)\\)\\|glem\\(\\| +at\\)\\|h\\(ent\\|usk\\(\\| +at\\)\\)\\|lagre\\|slett"
    ))


(defconst sysdul-RE-decl
  (if (= sysdul-lang ?e)
;; \(make-regexp '\("declare" "predeclare" "define" "universal" "automark"
;;   "default +error-handling"\)\)
      "automark\\|de\\(clare\\|f\\(ault +error-handling\\|ine\\)\\)\\|predeclare\\|universal"
;; \(make-regexp '\("deklarer" "predeklarer" "definer" "universell" "automerk"
;;    "standard +feil-h.ndtering"\)\)
    "automerk\\|de\\(finer\\|klarer\\)\\|predeklarer\\|standard +feil-h.ndtering\\|universell"
    ))

(defconst sysdul-RE-gui-stmnt
  (cond ((> sysdul-tool-version 4)
;;  \(make-regexp '\("obtain +value" "open +dialog" "clear" "set"
;;     "display +acknowledge +message" "display +warning +message" "enable"
;;     "display +status +message"      "display +error +message""disable"
;;     "hide" "insert" "close +dialog" "display +value +list" \)\)
	 "cl\\(ear\\|ose +dialog\\)\\|dis\\(able\\|play +\\(acknowledge +message\\|error +message\\|status +message\\|value +list\\|warning +message\\)\\)\\|enable\\|hide\\|insert\\|o\\(btain +value\\|pen +dialog\\)\\|set")
	((= sysdul-lang ?e)
;; \(make-regexp '("obtain" "obtain +value +of"  "activate" "passivate"
;;   "display +user +acknowledge +message" "display +user +warning +message"
;;   "display +user +field +message" "display +user +error +message" "set"\)\)
	 "activate\\|display +user +\\(acknowledge +message\\|error +message\\|field +message\\|warning +message\\)\\|obtain\\(\\| +value +of\\)\\|passivate\\|set")
	(t
;; \(make-regexp '\("motta" "motta verdien +til" "aktiver" "passiver"
;;   "vis +bruker +kvittering +melding" "plasser +menyvalg" "ta +ut"\)\)
	 "aktiver\\|motta\\(\\| verdien +til\\)\\|p\\(assiver\\|lasser +menyvalg\\)\\|ta +ut\\|vis +bruker +kvittering +melding")
	))

(defconst sysdul-RE-misc-stmnt
   (if (= sysdul-lang ?e)
       "note +that\\|read +\\|write +"
     "les +\\|merk +at\\|skriv +"))

(defconst sysdul-RE-misc-words
  (concat "\\b"
	   (if (= sysdul-lang ?e)
	       "e\\(mploys\\|xport\\(ing\\|s\\)\\)\\|import\\(ing\\|s\\)")
	   "\\b"))

(defconst sysdul-RE-global  "global\\( +init\\)? *$")

(defconst sysdul-RE-svapp-macro  "@[^\n@]+@")

(defconst sysdul-RE-svapp-line  "^\\*V.*")

(defconst sysdul-imenu-expression-1
  (list nil
	(concat "^\\s-*\\("
		(cond ((> sysdul-tool-version 4)
		       (concat "control-\\(handling\\|mainprocedure\\|"
			       "procedure\\)\\|event-\\(handler\\|"
			       "mainprocedure\\|procedure\\)\\|"
			       "mainprocedure\\|pro\\(cedure\\|gram\\)"))
		      ((= sysdul-lang ?e)
		       (concat "control-\\(handling\\|mainprocedure\\|"
			       "procedure\\)\\|mainprocedure\\|pro\\("
			       "cedure\\|gram\\)"))
		      (t
		       (concat "hovedprosedyre\\|kontroll-\\(administrasjon"
			       "\\|hovedprosedyre\\|prosedyre\\)\\|pro\\("
			       "gram\\|sedyre\\)")))
		"\\)\\s-+\\([a-zA-Z0-9_-]+\\)")
	(cond ((> sysdul-tool-version 4) 5)
	      ((= sysdul-lang ?e) 4)
	      (t 4)))
  "Expression for imenu listing of functions")

(defconst sysdul-imenu-expression-2
   (list "*Declare*"
	 (concat "^\\s-*\\("
		 "de\\(clare\\|fine\\)"
		 "\\)\\s-+\\([a-zA-Z0-9_-]+\\)")
	 3)
  "Expression for imenu listing of declarations")

(defconst sysdul-imenu-expression-3
   (list "*Define*"
	 (concat "^\\("
		 (if (> sysdul-tool-version 4)
		     "#\\(undef\\|define\\)\\s-+\\|" "")
		 "\\s-*@LOCAL@_\\|"
		 "[*]V\\s-+MACRO\\s-+"
		 "\\)\\([a-zA-Z0-9_|]+\\)")
	 3)
  "Expression for imenu listing of macros")

(defconst sysdul-imenu-expression-4
   (list "*Include*"
	 (concat "^\\("
		 (if (> sysdul-tool-version 4)
		     "#include\\|" "")
		 "\\s-*@FI@\\|"
		 "[*]V\\s-+FILE-INCLUDE"
		 "\\)\\s-+\\([^\n]+\\)")
	 2)
  "Expression for imenu listing of includes")

;; Transform
(if (eq sysdul-imenu-levels t) (setq sysdul-imenu-levels '(1 2 3 4)))

(defvar sysdul-imenu-expression
  (if sysdul-imenu-levels
      (remove-if 'null
		 (list
		  (if (member 1 sysdul-imenu-levels) sysdul-imenu-expression-1)
		  (if (member 2 sysdul-imenu-levels) sysdul-imenu-expression-2)
		  (if (member 3 sysdul-imenu-levels) sysdul-imenu-expression-3)
		  (if (member 4 sysdul-imenu-levels) sysdul-imenu-expression-4)
		  ))
    nil)
  "Expression for imenu listing")

; ======================================================================

(if sysdul-local-lib
    (load-library sysdul-local-lib))

; ======================================================================

;; Syntax table.  Like text-mode, but:
;;   +,=,*,/  are punctuation characters
;;   .,_ are symbol characters (dot since it is used in database names)
;;   - is a punctuation character in version 5+, but a symbol character
;;     in earlier versions, as it is often used in repository elements
;;   single and double quotes are quote characters
;;   ; and linefeed are comment start and end characters  (it is not possible
;;     to handle "^C"-comments here).
(if sysdul-mode-syntax-table ()
  (setq sysdul-mode-syntax-table (copy-syntax-table text-mode-syntax-table))
  (if (< sysdul-tool-version 5)
      (modify-syntax-entry ?-  "_"  sysdul-mode-syntax-table)
    (modify-syntax-entry   ?-  "."  sysdul-mode-syntax-table))
  (modify-syntax-entry     ?+  "."  sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?=  "."  sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?*  "."  sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?/  "."  sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?\' "\"" sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?\" "\"" sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?.  "_"  sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?_  "_"  sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?\n ">"  sysdul-mode-syntax-table)
  (modify-syntax-entry	   ?\; "<"  sysdul-mode-syntax-table)
  )

(defvar sysdul-mode-menu-base
  '(
    ["Fixup whitespace"           sysdul-fix-white t ]
    ["Split line"                 sysdul-split-line t ]
    ["Break long sentence"        sysdul-split-long (mark) ]
    "--"
    ["Insert error message"       sysdul-insert-message t]
    ["Insert procedure header"    sysdul-insert-proc-head t]
    ["Insert version string"      sysdul-insert-version-string t]
    "--"
    ["Comment region"             sysdul-comment-region (mark) ]
    ["UnComment region"           sysdul-uncomment-region (mark) ]
    ["Box Comment"                sysdul-comment-box t ]
    "--"
    ["Show window size"           sysdul-window-create-momentarily t ]
    ["Set window size"            sysdul-window-create t ]
    "--"
    ["Top of subprogram"          beginning-of-sysdul-subprogram t ]
    ["End of subprogram"          end-of-sysdul-subprogram t ]
    ["Indent subprogram"          sysdul-indent-subprogram t ]
    "--"
    ["Next statement"             sysdul-next-statement t ]
    ["Previous statement"         sysdul-previous-statement t ]
    ["Goto matching statement"    sysdul-goto-matching t ]
    ["Next error in listing"      sysdul-next-error t ]
    )
  "Emacs menu for Sysdul mode.")

(defvar sysdul-mode-map ()
  "Keymap used in Sysdul mode.")
(if sysdul-mode-map
    ()
  (setq sysdul-mode-map (make-sparse-keymap))
  (define-key sysdul-mode-map "\C-c " 	 'sysdul-fix-white)
  (define-key sysdul-mode-map "\C-c;"	 'sysdul-comment-region)
  (define-key sysdul-mode-map "\e\C-a"	 'beginning-of-sysdul-subprogram)
  (define-key sysdul-mode-map "\e\C-e"	 'end-of-sysdul-subprogram)
  (define-key sysdul-mode-map "\e;"	 'sysdul-indent-comment)
  (define-key sysdul-mode-map "\e\C-h"	 'mark-sysdul-subprogram)
  (define-key sysdul-mode-map "\e\n"	 'sysdul-split-line)
  (define-key sysdul-mode-map "\n"	 'sysdul-indent-new-line)
  (define-key sysdul-mode-map "\e\C-q"	 'sysdul-indent-subprogram)
  (define-key sysdul-mode-map "\C-c\C-p" 'sysdul-previous-statement)
  (define-key sysdul-mode-map "\C-c\C-n" 'sysdul-next-statement)
  (define-key sysdul-mode-map "\C-c\C-c" 'sysdul-comment-box)
  (define-key sysdul-mode-map "\C-cn"	 'sysdul-next-error)
  (define-key sysdul-mode-map "\t"	 'sysdul-indent-line)
  (define-key sysdul-mode-map "\C-x\C-m" 'sysdul-goto-matching)
  (if sysdul-gnuemacs-p
      (define-key sysdul-mode-map [C-return] 'sysdul-indent-new-line)
    (define-key sysdul-mode-map "\C-c\C-w" 'sysdul-window-create-momentarily)
    (define-key sysdul-mode-map [(control return)] 'sysdul-indent-new-line)
    (define-key sysdul-mode-map '(button3) 'sysdul-popup-menu))
  (if sysdul-local-key-function
      (funcall sysdul-local-key-function sysdul-mode-map)))

(cond ((not sysdul-gnuemacs-p) ; XEmacs
       (defvar sysdul-mode-menu sysdul-mode-menu-base "Sysdul menu"))
      ((>= emacs-major-version 20)   ; GNU Emacs 20+
       (easy-menu-define sysdul-mode-menu sysdul-mode-map "Sysdul menu"
			 (cons "Sysdul " sysdul-mode-menu-base)))
      (sysdul-gnuemacs-p       ; GNU Emacs z20
       (define-key sysdul-mode-map [menu-bar sysdul]
	 (cons "Sysdul" (make-sparse-keymap "Sysdul")))
       (define-key sysdul-mode-map [menu-bar sysdul sep-4]
	 '("--"))
       (define-key sysdul-mode-map [menu-bar sysdul next-error]
	 '("Next error in listing" . sysdul-next-error))
       (define-key sysdul-mode-map [menu-bar sysdul goto-match]
	 '("Goto matching statement" . sysdul-goto-matching))
       (define-key sysdul-mode-map [menu-bar sysdul prev-s]
	 '("Previous statement" . sysdul-previous-statement))
       (define-key sysdul-mode-map [menu-bar sysdul next-s]
	 '("Next statement" . sysdul-next-statement))
       (define-key sysdul-mode-map [menu-bar sysdul sep-3]
	 '("--"))
       (define-key sysdul-mode-map [menu-bar sysdul sub-bot]
	 '("End of subprogram" . end-of-sysdul-subprogram))
       (define-key sysdul-mode-map [menu-bar sysdul sub-top]
	 '("Top of subprogram" . beginning-of-sysdul-subprogram))
       (define-key sysdul-mode-map [menu-bar sysdul sub-indent]
	 '("Indent subprogram" . sysdul-indent-subprogram))
       (define-key sysdul-mode-map [menu-bar sysdul sep-2]
	 '("--"))
       (define-key sysdul-mode-map [menu-bar sysdul comm-box]
	 '("Box comment" . sysdul-comment-box))
       (define-key sysdul-mode-map [menu-bar sysdul uncomm-reg]
	 '("UnComment region" . sysdul-uncomment-region))
       (define-key sysdul-mode-map [menu-bar sysdul comm-reg]
	 '("Comment region" . sysdul-comment-region))
       (define-key sysdul-mode-map [menu-bar sysdul sep-1]
	 '("--"))
       (define-key sysdul-mode-map [menu-bar sysdul version-string]
	 '("Insert version string" . sysdul-insert-version-string))
       (define-key sysdul-mode-map [menu-bar sysdul proc-head]
	 '("Insert procedure header" . sysdul-insert-proc-head))
       (define-key sysdul-mode-map [menu-bar sysdul err-mess]
	 '("Insert error message" . sysdul-insert-message))
       (define-key sysdul-mode-map [menu-bar sysdul sep-0]
	 '("--"))
       (define-key sysdul-mode-map [menu-bar sysdul split-long]
	 '("Break long sentence" . sysdul-split-long))
       (define-key sysdul-mode-map [menu-bar sysdul split]
	 '("Split line" . sysdul-line))
       (define-key sysdul-mode-map [menu-bar sysdul white-fix]
	 '("Fixup whitespace" . sysdul-fix-white))))

;; Register with tplsub-mode at load time
(if  sysdul-inhibit-templates ()
  (require 'tplsub)
  (tplsub-register-mode 'sysdul-mode
			tplsub-sysdul-tmpl-list
			tplsub-sysdul-help-list
			'default ; template regexp
			'default ; case sens.
			" :"     ; cont string
			'default ; expansion key
			))


;;;###autoload
(defun sysdul-mode ()
  "Major mode for editing Sysdul code.
\\[sysdul-indent-line] indents the current Sysdul line correctly.

Key definitions:
\\{sysdul-mode-map}

Variables controlling indentation style and extra features:

 comment-start
    Normally nil in Sysdul mode.  If you want to use comments
    starting with `!', set this to the string \"!\".
 sysdul-std-indent (Default 2)
    The default value for the other sysdul-*-indent variables.
 sysdul-while-indent
    Extra indentation within while blocks.
 sysdul-if-indent
    Extra indentation within if blocks.
 sysdul-trans-indent
    Extra indentation within transaction blocks.
 sysdul-proc-indent
    Extra indentation within procedure blocks.   (default 2)
 sysdul-for-indent
    Extra indentation within for blocks.
 sysdul-continuation-indent
    Extra indentation applied to continuation statements.
 sysdul-comment-line-extra-indent
    Amount of extra indentation for text within full-line comments. (default 0)
 sysdul-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at `sysdul-comment-line-extra-indent' beyond
           the value of `sysdul-minimum-statement-indent-fixed' (for fixed
           format continuation style) or `sysdul-minimum-statement-indent-tab'
           (for TAB format continuation style).
    relative  means indent at `sysdul-comment-line-extra-indent' beyond the
 	      indentation for a line of code.
    (default 'fixed)
 sysdul-comment-indent-char
    Single-character string to be inserted instead of space for
    full-line comment indentation.  (default \" \")
 sysdul-minimum-statement-indent-fixed
    Minimum indentation for Sysdul statements in fixed format mode. (def.0)
 sysdul-minimum-statement-indent-tab
    Minimum indentation for Sysdul statements in TAB format mode.
 sysdul-blink-matching
    From a Sysdul END statement, blink the matching start statement.
    (default t)
 sysdul-continuation-string
    Single-character string to be inserted before a continuation
    line.  (default \":\")
 sysdul-comment-region
    String inserted by \\[sysdul-comment-region] at start of each line in
    region.  (default \"; \")
 sysdul-break-before-delimiters
    Non-nil causes `sysdul-do-auto-fill' breaks lines before delimiters.
    (default t)
 sysdul-startup-message
    Set to nil to inhibit message first time Sysdul mode is used.

Turning on Sysdul mode calls the value of the variable `sysdul-mode-hook'
with no args, if that variable is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if sysdul-startup-message
      (message "Emacs Sysdul mode %s %s.  Bugs to %s"
	       sysdul-mode-version sysdul-local-version bug-sysdul-mode))
  (setq sysdul-startup-message nil)
  (set-syntax-table sysdul-mode-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-line-start-skip)
  (make-local-variable 'comment-line-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'fill-column)
  (setq sysdul-break-before-delimiters t
	indent-line-function	       'sysdul-indent-line
	comment-indent-function	       'sysdul-comment-function
	comment-line-start-skip	       (if (< sysdul-tool-version 5)
					   "^\\(C +\\|\\*\\| *\;\\).*"
					 "^[*\;].*")
	comment-line-start	       (if (< sysdul-tool-version 5)
					   "C "
					 "\; ")

	comment-start-skip	       (if (< sysdul-tool-version 5)
					   "\; *\\|^\\*V +\; *\\|^C +"
					 "\; *\\|^\\*V +\; *")
	comment-start		       "\;"
	comment-column		       sysdul-comment-column
	comment-end		       ""
	require-final-newline	       t
	paragraph-separate	       (concat "^$\\|" page-delimiter)
	paragraph-start		       paragraph-separate
	indent-tabs-mode	       nil
	imenu-generic-expression       sysdul-imenu-expression
	mode-name		       sysdul-mode-name
	major-mode		       'sysdul-mode
	fill-column		       sysdul-fill-column)
  (sysdul-set-indent)
  (use-local-map sysdul-mode-map)
  (if (and (not sysdul-gnuemacs-p)
	   current-menubar
	   (not (assoc mode-name current-menubar)))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "Sysdul" sysdul-mode-menu)))
  (if  sysdul-inhibit-templates ()
    (tplsub-mode 1)) ; Turn on!
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'sysdul-mode-hook)
    (run-hooks 'sysdul-mode-hook)))


(defun sysdul-comment-function ()
  (save-excursion
    (skip-chars-backward " ")
    (max (+ 1 (current-column))
	 comment-column)))

(defun sysdul-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive "*")
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((looking-at comment-line-start-skip)
	 (sysdul-indent-line))
	((sysdul-find-comment-start-skip) ; catches any inline comment and
					; leaves point after comment-start-skip
	 (if comment-start-skip
	     (progn (goto-char (match-beginning 0))
		    (if (not (= (current-column) (sysdul-comment-function)))
			(progn (delete-horizontal-space)
			       (indent-to (sysdul-comment-function)))))
	   (end-of-line)))        ; otherwise goto end of line or sth else?
	;; No existing comment.
	;; If side-by-side comments are defined, insert one,
	;; unless line is now blank.
	((and comment-start (not (looking-at "^ *$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (sysdul-comment-function))
	 (insert comment-start))
	;; Else insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^ *$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (insert comment-line-start)
	 (insert-char (if (stringp sysdul-comment-indent-char)
			  (aref sysdul-comment-indent-char 0)
			sysdul-comment-indent-char)
		      (- (calculate-sysdul-indent) (current-column))))))

(defun sysdul-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts sysdul-comment-region at the beginning of every line in the region.
BEG-REGION and END-REGION are args which specify the region boundaries.
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert sysdul-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert sysdul-comment-region)))
      (let ((com (regexp-quote sysdul-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))


(defun sysdul-uncomment-region (beg-region end-region)
  "Un-comments every line in the region."
  (interactive "*r")
  (sysdul-comment-region beg-region end-region t))


(defun sysdul-comment-box (&optional prompt)
  "Frame for Sysdul comment.
A line is considered to consist of a non-empty prefix, a gap, then text,
then trailing blanks.  The frame is made up of the same prefix and gap,
a dash is used for each text position.
E.g.:
    ;    -----------------
    ;    This is a comment
    ;    -----------------
If a prefix is given, the function wil prompt for another character or string
from which to build the frame.  It is also possible to the delete an already
existing frame."
  (interactive "*P")
  (let ((ts 0)      ; text-start
	(te 0)	    ; text-end
	(ps 0)      ; prefix-start
	(pe 0)      ; prefix-end
	(dele nil)  ; delete preceding and succeeding line
	(cchr "-")  ; comment-character
	iter        ; counter
	pfx         ; prefix
	)
    (if prompt (progn
		 (setq cchr (read-string "Box-character(s): " "-"))
		 (setq dele (y-or-n-p "Delete lines above&below "))))
    (if dele
	(progn
	  (save-mark-and-excursion
	    (beginning-of-line 0)
	    (kill-line 1))
	  (save-mark-and-excursion
	    (beginning-of-line 2)
	    (kill-line 1))))
    (beginning-of-line)
    (setq ps (point))
    (save-mark-and-excursion
      (setq pe (+ ps (skip-chars-forward "^ \t")))
      (setq ts (+ pe (skip-chars-forward " \t")))
      (setq ts (point))
      (end-of-line)
      (setq te (point)))
    (setq pfx (buffer-substring ps pe))
    (setq ps (point))
    (insert pfx)
    (setq iter (- ts pe))
    (insert-char ?\  iter)
    (if (= 1 (length cchr))
	(insert-char (string-to-char cchr) (- te ts))
      (setq iter (/ (- te ts) (length cchr)))
      (while (> iter 0)
	(insert cchr)
	(setq iter (1- iter))))
    (setq pfx (buffer-substring ps (point)))
    (insert "\n")
    (end-of-line 1)
    (insert "\n" pfx)))

(if sysdul-gnuemacs-p ()		; XEmacs only
  (if (not (fboundp 'frame-width))
      (fset 'frame-width 'screen-width))
  (defun sysdul-window-create ()
    "Makes the window 79 columns wide.
See also `sysdul-window-create-momentarily'."
    (interactive)
    (condition-case nil
	(progn
	  (let ((window-min-width 2))
	    (if (< (window-width) (frame-width))
		(enlarge-window-horizontally (- (frame-width)
						(window-width) 1)))
	    (split-window-horizontally sysdul-window-split)
	    (other-window 1)
	    (switch-to-buffer " sysdul-window-extra" t)
	    (select-window (previous-window))))
      (error (message "No room for Sysdul window.")
	     'error)))

  (defun sysdul-window-create-momentarily (&optional arg)
    "Momentarily makes the window 79 columns wide.
Optional ARG non-nil and non-unity disables the momentary feature.
See also `sysdul-window-create'."
    (interactive "p")
    (if (or (not arg)
	    (= arg 1))
	(save-window-excursion
	  (if (not (equal (sysdul-window-create) 'error))
	      (progn (message "Type SPC to continue editing.")
		     (let ((char (next-command-event)))
		       (or (equal (event-to-character char) ? )
			   (setq unread-command-event char))))))
      (sysdul-window-create)))
  )					; XEmacs only

(defun sysdul-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive "*")
  (delete-horizontal-space)
  (if (save-mark-and-excursion (beginning-of-line) (looking-at comment-line-start-skip))
      (insert "\n" comment-line-start " ")
    (insert " " sysdul-continuation-string "\n "));Space after \n important
  (sysdul-indent-line))		          ;when the cont string is ;, C, c or *.

(or (fboundp 'delete-horizontal-regexp)
    (defun delete-horizontal-regexp (chars)
      "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
      (interactive "*sDelete regexp: ")
      (skip-chars-backward chars)
      (delete-region (point) (progn (skip-chars-forward chars) (point)))))


(defun beginning-of-sysdul-subprogram ()
  "Moves point to the beginning of the current Sysdul subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward sysdul-RE-subr-start nil 'move)))

(defun end-of-sysdul-subprogram ()
  "Moves point to the end of the current Sysdul subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward sysdul-RE-subr-end nil 'move)
    (goto-char (match-beginning 0))
    (forward-line 1)))

(defun mark-sysdul-subprogram ()
  "Put mark at end of Sysdul subprogram, point at beginning.
The marks are pushed."
  (interactive)
  (end-of-sysdul-subprogram)
  (push-mark (point))
  (beginning-of-sysdul-subprogram))

(defun sysdul-previous-statement ()
  "Moves point to beginning of the previous Sysdul statement.
Returns `first-statement' if that statement is the first
non-comment Sysdul statement in the file, and nil otherwise."
  (interactive)
  (let ((cont-quot (regexp-quote sysdul-continuation-string))
	not-first-statement continue-test)
    (beginning-of-line)
    (setq continue-test
	  (and
	   (not (looking-at comment-line-start-skip))
	   (save-mark-and-excursion (if (= (skip-chars-backward
				   (concat " \n" cont-quot))
				  0)
			       nil
			   (looking-at
			    (concat ".*" cont-quot " *\n"))))))
    (while (and (setq not-first-statement (= (forward-line -1) 0))
		(or (looking-at comment-line-start-skip)
		    (looking-at " *$")
		    (save-mark-and-excursion
		      (if (= (skip-chars-backward
					    (concat " \n" cont-quot))
			     0)
			  nil
			(looking-at
			 (concat ".*" cont-quot " *\n"))))
		    (looking-at (concat " *"  comment-start-skip)))))
    (cond ((and continue-test
		(not not-first-statement))
	   (message "Incomplete continuation statement."))
	  (continue-test
	   (sysdul-previous-statement))
	  ((not not-first-statement)
	   'first-statement))))

(defun sysdul-next-statement ()
  "Moves point to beginning of the next Sysdul statement.
Returns `last-statement' if that statement is the last
non-comment Sysdul statement in the file, and nil otherwise."
  (interactive)
  (let ((cont-quot (regexp-quote sysdul-continuation-string))
	not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
		      (and (= (forward-line 1) 0)
			   (not (eobp))))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at " *$")
		    (save-mark-and-excursion (skip-chars-backward
				     (concat " \n" cont-quot))
				    (looking-at
				     (concat ".*" cont-quot "\n")))
 		    (looking-at (concat " *"  comment-start-skip)))))
    (if (not not-last-statement)
 	'last-statement)))

(defun sysdul-next-error (reverse)
  "Find next (or previous, with prefix) Sysdul error (in listing)."
  (interactive "P")
  (end-of-line)
  (if (not (funcall (if reverse 'search-backward-regexp
		      'search-forward-regexp)
		    "^ *\\*\\*\\*[^*].+\\*\\*\\*" nil t))
      (error "no more errors!")))


(defun sysdul-blink-matching ()
  "From a Sysdul END statement, blink the matching XX statement."
  (let ((count 1) (top-of-window (window-start)) matching-stm
	(endif-point (point)) message)
    (if (save-mark-and-excursion (beginning-of-line)
			(skip-chars-forward " ")
			(looking-at sysdul-RE-end))
	(progn
	  (save-mark-and-excursion
	    (while (and
		    (not (= count 0))
		    (not (eq (sysdul-previous-statement)
			     'first-statement))
		    (not (looking-at sysdul-RE-subr-end) ))
					; Keep local to subprogram
	      (skip-chars-forward " ")
	      (cond ((looking-at sysdul-RE-loop-or-subr-start)
		     (setq count (- count 1)))
		    ((looking-at sysdul-RE-end)
		     (setq count (+ count 1)))))
	    (if (not (= count 0))
		(setq message "No matching start of block.")
	      (if (< (point) top-of-window)
		  (setq message (concat "Matches "
					(buffer-substring
					 (progn (beginning-of-line) (point))
					 (progn (end-of-line) (point)))))
		(setq matching-stm (point)))))
	  (if message
	      (message "%s" message)
	    (goto-char matching-stm)
	    (sit-for 1)
	    (goto-char endif-point))))))

(defun sysdul-goto-matching ()
  "From a Sysdul END statement, goto the matching XX statement."
  (interactive)
  (let ((count 1) matching-stm message (endif-point (point)))
    (if (save-mark-and-excursion (beginning-of-line)
			(skip-chars-forward " ")
			(looking-at sysdul-RE-end))
	(progn
	  (while (and (not (= count 0))
		      (not (eq (sysdul-previous-statement)
			       'first-statement))
		      (not (looking-at sysdul-RE-subr-end)))
					; Keep local to subprogram
	    (skip-chars-forward " ")
	    (cond ((looking-at sysdul-RE-loop-or-subr-start)
		   (setq count (- count 1)))
		  ((looking-at sysdul-RE-end)
		   (setq count (+ count 1)))))
	  (if (not (= count 0))
	      (setq message "No matching start of block.")
	    (setq matching-stm (point)))
	  (if (not message)
	      (goto-char matching-stm)
	    (goto-char endif-point)
	    (message "%s" message))))))

(defun sysdul-indent-line ()
  "Indents current Sysdul line based on its contents and on previous lines."
  (interactive "*")
  (let ((cfi (calculate-sysdul-indent)))
    (save-mark-and-excursion
      (beginning-of-line)
      (if (not (= cfi (sysdul-current-line-indentation)))
	  (sysdul-indent-to-column cfi)
	(beginning-of-line)
	(if (and (not (looking-at comment-line-start-skip))
		 (sysdul-find-comment-start-skip))
	    (sysdul-indent-comment))))
    ;; Never leave point in left margin.
    (if (< (current-column) cfi)
	(move-to-column cfi))
    (if (and auto-fill-function
	     (> (save-mark-and-excursion (end-of-line) (current-column)) fill-column))
	(save-mark-and-excursion
	  (end-of-line)
	  (sysdul-do-auto-fill)))
    (if sysdul-blink-matching
	(sysdul-blink-matching))))

(defun sysdul-indent-new-line ()
  "Reindent the current Sysdul line, insert a newline and indent the newline."
  (interactive "*")
  (save-mark-and-excursion
    (beginning-of-line)
    (skip-chars-forward " ")
    (if (or (looking-at sysdul-RE-or-if)
	    (looking-at sysdul-RE-end)	; Reindent only where it is most
	    (looking-at sysdul-RE-else)	; likely to be necessary
	    (looking-at (regexp-quote sysdul-continuation-string)))
	(sysdul-indent-line)))
  (newline)
  (sysdul-indent-line))

(defun sysdul-unindent-line (n &optional indent)
  "Backup N standard indents."
  (interactive "*p")
  (backward-delete-char-untabify (* n (or indent sysdul-std-indent))))

(defun sysdul-indent-subprogram ()
  "Properly indents the Sysdul subprogram which contains point."
  (interactive "*")
  (let (blink-state sysdul-blink-matching)
    (save-mark-and-excursion
      (mark-sysdul-subprogram)
      (message "Indenting subprogram...")
      (indent-region (point) (mark t) nil))
    (setq sysdul-blink-matching blink-state)
    (message "Indenting subprogram...done.")))

(defun calculate-sysdul-indent ()
  "Calculates the Sysdul indent column based on previous lines."
  (let (icol
	first-statement
	(case-fold-search t)
	(quot-cont (regexp-quote sysdul-continuation-string))
	(sysdul-minimum-statement-indent
	 (if indent-tabs-mode
	     sysdul-minimum-statement-indent-tab
	   sysdul-minimum-statement-indent-fixed)))
    (save-mark-and-excursion
      (setq first-statement (sysdul-previous-statement))
      (if first-statement
	  (setq icol sysdul-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol sysdul-minimum-statement-indent)
	    (setq icol (sysdul-current-line-indentation)))
	  (skip-chars-forward " ")
	  (while (looking-at sysdul-RE-label)
	    (setq first-statement (sysdul-previous-statement))
	    (skip-chars-forward " "))))
      (if first-statement
	  (setq icol sysdul-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol sysdul-minimum-statement-indent)
	    (setq icol (sysdul-current-line-indentation)))
	  (skip-chars-forward " ")
	  (cond ((or (looking-at sysdul-RE-if)
		     (looking-at sysdul-RE-or-if)
		     (looking-at sysdul-RE-else))
		 (setq icol (+ icol sysdul-if-indent)))
		((looking-at sysdul-RE-subr)
		 (setq icol (+ icol sysdul-proc-indent)))
		((looking-at sysdul-RE-while)
		 (setq icol (+ icol sysdul-while-indent)))
		((looking-at (concat "start" sysdul-RE-trans))
		 (setq icol (+ icol sysdul-trans-indent)))
		((looking-at sysdul-RE-loop-start)
		 ;; if/while/start trans already covered
		 (setq icol (+ icol sysdul-for-indent)))
		((looking-at (concat sysdul-RE-end "[^ \t=(a-z]"))
		 ;; Previous END resets indent to minimum
		 (setq icol sysdul-minimum-statement-indent))))))
    (save-mark-and-excursion
      (beginning-of-line)
      (cond ((looking-at " *$"))
	    ((looking-at comment-line-start-skip)
	     (cond ((eq sysdul-comment-indent-style 'relative)
		    (setq icol (+ icol sysdul-comment-line-extra-indent)))
		   ((eq sysdul-comment-indent-style 'fixed)
		    (setq icol (+ sysdul-minimum-statement-indent
				  sysdul-comment-line-extra-indent))))
	     (setq sysdul-minimum-statement-indent 0))
	    ((save-mark-and-excursion
	       (skip-chars-backward (concat " \n" quot-cont))
	       (looking-at (concat ".*" quot-cont " *\n")))
	     (setq icol (+ icol sysdul-continuation-indent)))
	    ((looking-at " *#")	; Check for cpp directive. <-???
	     (setq sysdul-minimum-statement-indent 0 icol 0))
	    (first-statement)
	    (t
	     (skip-chars-forward " ")
	     (cond ((or (looking-at (concat sysdul-RE-end sysdul-RE-if))
			(looking-at sysdul-RE-or-if)
			(looking-at sysdul-RE-else))
		    (setq icol (- icol sysdul-if-indent)))
		   ((looking-at sysdul-RE-label)
		    (setq icol 0))
		   ((looking-at sysdul-RE-subr-end)
		    (setq icol (- icol sysdul-proc-indent)))
		   ((looking-at (concat sysdul-RE-end sysdul-RE-while))
		    (setq icol (- icol sysdul-while-indent)))
		   ((looking-at (concat sysdul-RE-end sysdul-RE-trans))
		    (setq icol (- icol sysdul-trans-indent)))
		   ((looking-at sysdul-RE-loop-end)
		    (setq icol (- icol sysdul-for-indent)))
		   ((and (looking-at (concat sysdul-RE-end "[^ \t=(a-z]"))
			 (not (= icol sysdul-minimum-statement-indent)))
 		    (message "Warning: `end' without type."))))))
    (max sysdul-minimum-statement-indent icol)))

(defun sysdul-current-line-indentation ()
  "Indentation of current line.
This is the column position of the first non-whitespace character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (save-mark-and-excursion
    (beginning-of-line)
    (cond ((looking-at comment-line-start-skip)
	   (goto-char (match-end 0))
	   (skip-chars-forward
	    (if (stringp sysdul-comment-indent-char)
		sysdul-comment-indent-char
	      (char-to-string sysdul-comment-indent-char)))))
    ;; Move past whitespace.
    (skip-chars-forward " ")
    (current-column)))

(defun sysdul-indent-to-column (col)
  "Indents current line with spaces to column COL."
  (save-mark-and-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(if sysdul-comment-indent-style
	    (let ((char (if (stringp sysdul-comment-indent-char)
			    (aref sysdul-comment-indent-char 0)
			  sysdul-comment-indent-char)))
	      (goto-char (match-end 0))
	      (delete-horizontal-regexp (concat " " (char-to-string char)))
	      (insert-char char (- col (current-column)))))
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
      (if (and comment-start-skip
	       (sysdul-find-comment-start-skip))
	  (progn (goto-char (match-beginning 0))
		 (if (not (= (current-column) (sysdul-comment-function)))
		     (progn (delete-horizontal-space)
			    (indent-to (sysdul-comment-function)))))))))

(defun sysdul-find-comment-start-skip ()
  "Move to past `comment-start-skip' found on current line.
Return t if `comment-start-skip' found, nil if not."
;;; In order to move point only if comment-start-skip is found,
;;; this one uses a lot of save-mark-and-excursions.  Note that re-search-forward
;;; moves point even if comment-start-skip is inside a string-constant.
;;; Some code expects certain values for match-beginning and end
  (interactive)
  (let ((save-match-beginning) (save-match-end))
    (if (save-mark-and-excursion
	  (re-search-forward comment-start-skip
			     (save-mark-and-excursion (end-of-line) (point)) t))
	(progn
	  (setq save-match-beginning (match-beginning 0))
	  (setq save-match-end (match-end 0))
	  (if (sysdul-is-in-string-p (match-beginning 0))
	      (progn
		(save-mark-and-excursion
		  (goto-char save-match-end)
		  (sysdul-find-comment-start-skip)) ; recurse for rest of line
		)
	    (goto-char save-match-beginning)
	    (re-search-forward comment-start-skip
			       (save-mark-and-excursion (end-of-line) (point)) t)
	    (goto-char (match-end 0))
	    t))
      nil)))

;;;From: ralf@up3aud1.gwdg.de (Ralf Fassel)
;;; Test if TAB format continuation lines work.
(defun sysdul-is-in-string-p (where)
  "Return non-nil if POS (a buffer position) is inside a Sysdul string,
nil else."
  (save-mark-and-excursion
    (goto-char where)
    (cond
     ((bolp) nil)			; bol is never inside a string
     ((save-mark-and-excursion			; comment lines too
	(beginning-of-line)(looking-at comment-line-start-skip)) nil)
     (t (let (;; ok, serious now. Init some local vars:
	      (parse-state '(0 nil nil nil nil nil 0))
	      (quoted-comment-start (if comment-start
					(regexp-quote comment-start)))
	      (not-done t)
	      parse-limit
	      end-of-line
	      )
	  ;; move to start of current statement
	  (sysdul-next-statement)
	  (sysdul-previous-statement)
	  ;; now parse up to WHERE
	  (while not-done
	    (if (or ;; skip to next line if:
		 ;; - comment line?
		 (looking-at comment-line-start-skip)
		 ;; - at end of line?
		 (eolp)
		 ;; - not in a string and after comment-start?
		 (and (not (nth 3 parse-state))
		      comment-start
		      (equal comment-start
			     (char-to-string (preceding-char)))))
		;; get around a bug in forward-line in versions <= 18.57
		(if (or (> (forward-line 1) 0) (eobp))
		    (setq not-done nil))
	      ;; else:
	      ;; if we are at beginning of code line, skip any
	      ;; whitespace, labels and tab continuation markers.
	      (if (bolp) (skip-chars-forward " "))
	      ;; find out parse-limit from here
	      (setq end-of-line (save-mark-and-excursion (end-of-line)(point)))
	      (setq parse-limit (min where end-of-line))
	      ;; parse max up to comment-start, if non-nil and in current line
	      (if comment-start
		  (save-mark-and-excursion
		    (if (re-search-forward quoted-comment-start end-of-line t)
			(setq parse-limit (min (point) parse-limit)))))
	      ;; now parse if still in limits
	      (if (< (point) where)
		  (setq parse-state (parse-partial-sexp
				     (point) parse-limit nil nil parse-state))
		(setq not-done nil))
	      ))
	  ;; result is
	  (nth 3 parse-state))))))

(defun sysdul-auto-fill-mode (arg)
  "Toggle sysdul-auto-fill mode.
With ARG, turn `sysdul-auto-fill' mode on iff ARG is positive.
In `sysdul-auto-fill' mode, inserting a space at a column beyond `fill-column'
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (setq auto-fill-function
	       (if (if (null arg)
		       (not auto-fill-function)
		     (> (prefix-numeric-value arg) 0))
		   'sysdul-indent-line
		 nil))
    ;; update mode-line
    (cond ((fboundp 'redraw-modeline)        (redraw-modeline))
	  ((fboundp 'force-mode-line-update) (force-mode-line-update))
	  (t            (set-buffer-modified-p (buffer-modified-p))))))

(defun sysdul-do-auto-fill ()
  (interactive)
  (let* ((opoint (point))
	 (bol (save-mark-and-excursion (beginning-of-line) (point)))
	 (eol (save-mark-and-excursion (end-of-line) (point)))
	 (bos (min eol (+ bol (sysdul-current-line-indentation))))
	 (quote
	  (save-mark-and-excursion
	    (goto-char bol)
	    (if (looking-at comment-line-start-skip)
		nil			; OK to break quotes on comment lines.
	      (move-to-column fill-column)
	      (cond ((sysdul-is-in-string-p (point))
		     (save-mark-and-excursion (re-search-backward "[^']'[^']" bol t)
				     (if sysdul-break-before-delimiters
					 (point)
				       (1+ (point)))))
		    (t nil)))))
	 ;;
	 ;; decide where to split the line. If a position for a quoted
	 ;; string was found above then use that, else break the line
	 ;; before the last delimiter.
	 ;; Delimiters are whitespace, commas, and operators.
	 ;; Will break before a pair of *'s.
	 ;;
	 (fill-point
	  (or quote
	      (save-mark-and-excursion
		(move-to-column (1+ fill-column))
		(skip-chars-backward "^ \t\n,'+-/*=)"
;;;		 (if sysdul-break-before-delimiters
;;;		     "^ \t\n,'+-/*=" "^ \t\n,'+-/*=)")
		 )
		(if (<= (point) (1+ bos))
		    (progn
		      (move-to-column (1+ fill-column))
;;;what is this doing???
		      (if (not (re-search-forward "[\n,'+-/*)=]" eol t))
			  (goto-char bol))))
		(if (bolp)
		    (re-search-forward " " opoint t)
		  (forward-char -1)
		  (if (looking-at "'")
		      (forward-char 1)
		    (skip-chars-backward " \*")))
		(if sysdul-break-before-delimiters
		    (point)
		  (1+ (point))))))
	 )
    ;; if we are in an in-line comment, don't break unless the
    ;; line of code is longer than it should be. Otherwise
    ;; break the line at the column computed above.
    ;;
    ;; Need to use sysdul-find-comment-start-skip to make sure that quoted !'s
    ;; don't prevent a break.
    (if (not (or (save-mark-and-excursion
		   (if (and (re-search-backward comment-start-skip bol t)
			    (not (sysdul-is-in-string-p (point))))
		       (progn
			 (skip-chars-backward " ")
			 (< (current-column) (1+ fill-column)))))
		 (save-mark-and-excursion
		   (goto-char fill-point)
		   (bolp))))
	(if (> (save-mark-and-excursion
		 (goto-char fill-point) (current-column))
	       (1+ fill-column))
	    (progn (goto-char fill-point)
		   (sysdul-break-line))
	  (save-mark-and-excursion
	    (if (> (save-mark-and-excursion
		     (goto-char fill-point)
		     (current-column))
		   (+ (calculate-sysdul-indent) sysdul-continuation-indent))
		(progn
		  (goto-char fill-point)
		  (sysdul-break-line))))))
    ))

(defun sysdul-break-line ()
  (let ((bol (save-mark-and-excursion (beginning-of-line) (point)))
	(eol (save-mark-and-excursion (end-of-line) (point)))
	(comment-string nil))

    (save-mark-and-excursion
      (if (and comment-start-skip (sysdul-find-comment-start-skip))
	  (progn
	    (re-search-backward comment-start-skip bol t)
	    (setq comment-string (buffer-substring (point) eol))
	    (delete-region (point) eol))))
;;; Forward line 1 really needs to go to next non white line
    (if (save-mark-and-excursion (forward-line 1)
			(or (looking-at "     [^ 0\n]")
			    (looking-at "\t[1-9]")))
	(progn
	  (forward-line 1)
	  (delete-indentation)
	  (delete-char 2)
	  (delete-horizontal-space)
	  (sysdul-do-auto-fill))
      (sysdul-split-line))
    (if comment-string
	(save-mark-and-excursion
	  (goto-char bol)
	  (end-of-line)
	  (delete-horizontal-space)
	  (indent-to (sysdul-comment-function))
	  (insert comment-string)))))


(defun sysdul-fix-white (&optional pfx)
  "Remove superfluous whitespace.  With prefix, also untabify."
  (interactive "*P")
  (save-mark-and-excursion
    (if pfx
	(untabify (point-min) (point-max)))
    (goto-char (point-min))
    (replace-regexp "\r$" "")		; DOS...
    (goto-char (point-min))
    (replace-regexp "[ \t]+$" "")	; Extra blanks at eol
    (goto-char (point-min))
    (replace-regexp "\n\n+\\'" "\n")))	; Extra lines at eob


(defun sysdul-split-long (minpt maxpt)
  "Sqeezes a long Sysdul sentence..."
  (interactive "*r")
  (save-restriction
    (narrow-to-region minpt maxpt)
    (let ((pos-var 0)
	  (collim 77))
      (goto-char (point-min))
      (save-mark-and-excursion (replace-regexp " +" " " nil))
      (save-mark-and-excursion (replace-regexp " *, *" "," nil))
      (save-mark-and-excursion (replace-regexp ",:\n" "," nil))
      (setq pos-var (- (point-max) (point)))
      (while (> pos-var collim)
	(message "[%d]" pos-var)
	(end-of-line)
	(while (> (current-column) collim) (forward-word -1))
	(skip-chars-backward "^ ")
	(insert ":\n")
	(setq pos-var (- (point-max) (point)))))))


(defun sysdul-insert-message (&optional number)
  "Insert message from Sysdul program.
This is typically overridden in local libraries."
  (interactive "*")
  (insert "display error message "))


(defun sysdul-insert-version-string ()
  "Insert version string.
This is typically overridden in local libraries."
  (interactive "*")
  (insert sysdul-comment-region " SCCS_ID = '@@(#)%" "M% %" "E% %" "I%>'\n"))


(defun sysdul-insert-proc-head (&optional proc-name)
  "Insert Sysdul procedure heading.
This is typically overridden in local libraries."
  (interactive "*")
  (insert sysdul-comment-region " <heading>\n")
  (sysdul-insert-version-string))

(if sysdul-gnuemacs-p ()
  (defun sysdul-popup-menu (e)
    "Pops up the Sysdul menu."
    (interactive "@e")
    (popup-menu (cons (concat "Sysdul Mode Commands") sysdul-mode-menu))))

;; Hippie expansion


(defun sysdul-he-read ()
  "Read the element definition list(s)."
  (setq sysdul-he-list '())
  (if sysdul-he-files
      (let ((flist sysdul-he-files) thisfile)
	(if (stringp sysdul-he-files) (setq flist (list sysdul-he-files)))
	(while flist
	  (setq thisfile (car flist))
	  (message "Reading %s..." thisfile)
	  (load thisfile)
	  (setq sysdul-he-list (append sysdul-he-list-tmp
				       sysdul-he-list)
		flist (cdr flist))))))


(defun he-sysdul-symbol-beg ()
  "Goto start of current name"
  (save-mark-and-excursion
    (skip-chars-backward "a-zA-Z0-9_æøåÆØÅ")
    (point)))


(defun try-complete-sysdul-symbol (old)
  "Try to complete word as a Systemator name.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise.
To use this, add it to `hippie-expand-try-functions-list'"
  (if (not (member major-mode sysdul-he-modes))
      nil
    (if (and (not old)
	     (null sysdul-he-list))
	(sysdul-he-read))
    (if (null sysdul-he-list)
	nil
      (if (not old)
	  (progn
	    (he-init-string (he-sysdul-symbol-beg) (point))
	    (setq he-search-string (upcase he-search-string))
	    (if (not (he-string-member he-search-string he-tried-table))
		(setq he-tried-table (cons he-search-string he-tried-table)))
	    (setq he-expand-list
		  (and (not (equal he-search-string ""))
		       (all-completions he-search-string sysdul-he-list)))))
      (while (and he-expand-list
		  (he-string-member (car he-expand-list) he-tried-table))
	(setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
	  (progn
	    (if old (he-reset-string))
	    ())
	(progn
	  (he-substitute-string (car he-expand-list))
	  (setq he-expand-list (cdr he-expand-list))
	  t)))))

; ======================================================================

(defsubst svapp-face()
  "Preprocessor face"
  (if (fboundp 'facep)
    (cond ((facep 'font-lock-preprocessor-face) 'font-lock-preprocessor-face)
	  ((facep 'font-lock-constant-face) 'font-lock-constant-face)
	  (t 'font-lock-reference-face))
    (cond ((boundp 'font-lock-preprocessor-face) 'font-lock-preprocessor-face)
	  ((boundp 'font-lock-constant-face) 'font-lock-constant-face)
	  (t  'font-lock-reference-face))))
(defvar bold 'bold)

(defvar sysdul-font-lock-keywords-1
  (purecopy
   (list
    ;; procedure declarations
    (cons sysdul-RE-subr-start-or-end    'font-lock-function-name-face)
    (cons sysdul-RE-svapp-line		 (svapp-face))
    (cons sysdul-RE-svapp-macro		 (svapp-face))
    ))
  "For consideration as a value of `sysdul-font-lock-keywords'.
This highlights procedure names and preprocessor statements.")

(defvar sysdul-font-lock-keywords-2
  (purecopy
   (append
    sysdul-font-lock-keywords-1 ; procedure declarations
    (list
     ;; Variable declarations
     (list (concat "^ *\\(" sysdul-RE-decl "\\)")
	   1 'font-lock-type-face)
     ;; Flow control
     (list (concat "^ *\\(" sysdul-RE-loop-start "\\) +")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-loop-end "\\)")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-or-if "\\) +")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-else "\\)")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-call "\\)")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-goto-and-label "\\)")
	   1 'bold)
     )))
  "For consideration as a value of `sysdul-font-lock-keywords'.
This highlights procedures, preprocessor statements, declarations and
flow control statements.")

(defvar sysdul-font-lock-keywords-3
  (purecopy
   (append
    sysdul-font-lock-keywords-2 ; see above
    (list
     ;; DB & GUI-statements
     (list (concat "^ *\\(" sysdul-RE-db-stmnt "\\)\\b")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-identify "\\)")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-gui-stmnt "\\)\\b")
	   1 'font-lock-keyword-face)
    )))
  "For consideration as a value of `sysdul-font-lock-keywords'.
This highlights procedure names, flow control statements, declarations,
DB-statements and GUI-statements.")

(defvar sysdul-font-lock-keywords-4
  (purecopy
   (append
    sysdul-font-lock-keywords-3 ; see above
    (list
    ;; Variable declarations
     sysdul-RE-misc-words
     (list (concat "^ *\\(" sysdul-RE-misc-stmnt "\\)\\b")
	   1 'font-lock-keyword-face)
     (list (concat "^ *\\(" sysdul-RE-spaghetti "\\)")
	   1 'bold)
     (cons sysdul-RE-global 'font-lock-type-face)
    )))
  "For consideration as a value of `sysdul-font-lock-keywords'.
This highlights procedure names, comments, flow control statements,
declarations, DB-statements, and misc. keywords.")

;; The keywords in the preceding lists assume case-insensitivity.
(put 'sysdul-mode 'font-lock-keywords-case-fold-search t)

(defvar sysdul-font-lock-keywords
  (if (< sysdul-tool-version 5)
      (append (list (cons "^C +.*$" 'font-lock-comment-face))
	      sysdul-font-lock-keywords-3)
    sysdul-font-lock-keywords-3)
  "Additional expressions to highlight in Sysdul mode.")

(provide 'sysdul)

;;; Comments from fortran.el:
;;
;; Author: Michael D. Prange <prange@erl.mit.edu>
;; Maintainer: bug-fortran-mode@erl.mit.edu
;; Version 1.30.2 (June 1, 1993)
;; Keywords: languages
;;
;; Fortran mode has been upgraded and is now maintained by Stephen A. Wood
;; (saw@cebaf.gov).  It now will use either fixed format continuation line
;; markers (character in 6th column), or tab format continuation line style
;; (digit after a TAB character.)  A auto-fill mode has been added to
;; automatically wrap fortran lines that get too long.
;;
;; We acknowledge many contributions and valuable suggestions by
;; Lawrence R. Dodd, Ralf Fassel, Ralph Finch, Stephen Gildea,
;; Dr. Anil Gokhale, Ulrich Mueller, Mark Neale, Eric Prestemon,
;; Gary Sabot and Richard Stallman.

;;; sysdul.el ends here
