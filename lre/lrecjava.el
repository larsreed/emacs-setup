;;; lrecjava.el --- Lars Reed - Emacs init file for C/C++/Java

;; Copyright (C) 1993-2004 Lars Reed @@(#) LRE %M% %I% %E%>
;; Author:		Lars Reed <Lars@kalars.net>
;; Last-Modified:	$Date: 2007/06/19 20:06:54 $
;; Version:		$Id: lrecjava.el,v 1.4 2007/06/19 20:06:54 larsr Exp $
;; Keywords: Emacs setup

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
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))

;; NOTE: depends upon settings in lresetup.el

(eval-and-compile
  (lre-safe-require 'tplsub))

(defcustom lre-inhibit-cc-lobotomize t
  "* if non-nil, cc-lobotomy is not loaded."
  :group 'lresetup  :type  'boolean)

(or lre-inhibit-cc-lobotomize (lre-add-memb 'cc-lobo))


;;; -------------------------------------------------------------------------
;;; /// variables etc ///

(when (and (lre-memb 'jde)
	   (fboundp 'custom-set-variables))
  (custom-set-variables
   '(jde-ant-read-target                t)
   '(jde-ant-read-args                  t)
   '(jde-ant-complete-target            t)
   '(jde-build-function                 (quote (jde-ant-build)))
   '(jde-checkstyle-classpath           (concat "C:\\Program Files\\Java\\"
                                                "checkstyle-3.5\\"
                                                "checkstyle-all-3.5.jar"))
   '(jde-checkstyle-option-config-file  (concat "c:\\usr\\lib\\java\\"
                                                "checkstyle_checks.xml"))
   '(jde-compile-option-depend		nil)
   '(jde-compile-option-deprecation	t)
   '(jde-compile-option-target		'("1.4"))
   '(jde-compile-option-verbose		t)
   '(jde-db-read-app-args	        t)
   '(jde-enable-senator			nil)
   '(jde-imenu-include-classdef		t)
   '(jde-imenu-include-modifiers	t)
   '(jde-imenu-modifier-abbrev-alist	(quote
					 (("public" . 43) ("protected" . 35)
					  ("private" . 45) ("static" . 167)
					  ("transient" . 84) ("volatile" . 126)
					  ("abstract" . 34) ("final" . 33)
					  ("native" . 36) ("synchronized" . 64)
					  ("strictfp" . 37))))
   '(jde-imenu-sort			'asc)
;;   '(jde-imenu-create-index-function    'lre-jde-imenu-create)
   '(jde-import-auto-sort		t)
   '(jde-import-insert-group-names	t)
   '(jde-import-sorted-groups		(quote gor))
   '(jde-jsee-javadoc-noindex-option	nil)
   '(jde-jsee-javadoc-notree-option	nil)
   '(jde-jsee-javadoc-package-option	t)
   '(jde-javadoc-gen-use		t)
   '(jde-run-option-enable-assertions	"Everywhere")
   '(jde-wiz-get-set-variable-prefix	"p")
   '(jde-wiz-get-set-methods-order	'("All set methods followed by all get methods"))
   ))

(setq
      c-default-style		      (if (lre-memb 'personal)
					  "lre"
					'((java-mode . "java")
					  (other . "xlre")))
      c-enable-xemacs-performance-kludge-p t
      pmd-home			      (lre-fixed :pmd)
      pmd-java-home		      "java.exe"
      pmd-ruleset-list		      (mapcar
				       (function (lambda (d)
						   (concat "rulesets/" d
							   ".xml")))
				       (list "basic" "braces" "codesize"
					     "coupling" "design"
					     "experimental" "imports"
					     "junit" "naming" "strings"
					     "strictexception" "unusedcode"))
      )


;;; ---------------------------------------------------------------------
;;; /// Loading ///

(autoload 'c-comment-edit "c-comment-edit2" "C comment edit" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)
(autoload 'jde-mode "jde" "Java Development Environment" t)
(autoload 'jde-check "jde-check"
  "Checks the Java program in the current buffer with checkstyle." t)
(when (lre-memb-all 'personal 'win32)
  (autoload 'pmd-current-buffer "pmd" "Run PMD on current buffer" t)
  (autoload 'pmd-current-dir "pmd" "Run PMD on current directory" t))
(autoload 'rebox-c-comment "c-boxes" nil t)
(autoload 'reindent-c-comment "c-boxes" nil t)
(when (lre-memb 'tpl)
  (tplsub-reg-fast 'java-mode	tplsub-java-list      tplsub-java-help-list)
  (tplsub-reg-fast 'jde-mode	tplsub-java-list      tplsub-java-help-list))

;;; ----------------------------------------------------------------------
;;; /// C/C++/Java ///

(defun lre-c-comment (txt &optional symb)
  "Insert C-comment."
  (interactive "*sComment: ")
  (if (> (length txt) 0)
      (let* ((rest-len (- 76 8))	; Fyll til 76 kolonner
	     (fill-len (- rest-len (length txt)))
	     (prex (if (< fill-len 0) 0 (/ fill-len 2)))
	     (postx (if (< fill-len 0) 0 (- fill-len prex))))
	(insert "/* " (make-string prex (or symb ?-))
		" " txt " "
		(make-string postx (or symb ?-)) "*/"))
    (insert "/*  */")
    (forward-char -3)))

(defun lre-c-style()
;; Design lre-style
  (lre-debug "lre-c-style")
    (c-add-style "lre"
	       (cons
		(cons 'c-basic-offset lre-std-indent)
		'((c-backslash-column . 65)
		  (c-hanging-braces-alist (brace-list-open)
					  (inexpr-class-open after)
					  (inexpr-class-close before)
					  (inline-open after)
					  (substatement-open after)
					  (class-open after)
					  (defun-open after)
					  (block-close . c-snug-do-while)
					  (extern-lang-open after))
		  (c-cleanup-list (scope-operator
				   list-close-comma
				   defun-close-semi
				   empty-defun-braces
				   compact-empty-funcall))
		  (c-offsets-alist
		   (string . c-lineup-dont-change)
		   (c . c-lineup-C-comments)
		   (defun-open . 0)
		   (defun-close . 0)
		   (defun-block-intro . +)
		   (class-open . 0)
		   (class-close . 0)
		   (inline-open . 0) ;; *
		   (inline-close . 0)
		   (func-decl-cont . c-lineup-java-throws) ;; *
		   (knr-argdecl-intro  . c-lineup-arglist) ;; *
		   (knr-argdecl . 0)
		   (topmost-intro . 0)
		   (topmost-intro-cont . +) ;; *
		   (member-init-intro . +)
		   (member-init-cont . c-lineup-multi-inher)
		   (inher-intro . +)
		   (inher-cont . c-lineup-java-inher) ;; *
		   (block-open . 0)
		   (block-close . 0)
		   (brace-list-open . 0)
		   (brace-list-close . 0)
		   (brace-list-intro . +)
		   (brace-list-entry . 0)
		   (brace-entry-open . 0)
		   (statement . 0)
		   (statement-cont . c-lineup-math) ;; *
		   (statement-block-intro . +)
		   (statement-case-intro . +)
		   (statement-case-open . +) ;; *
		   (substatement . +)
		   (substatement-open . 0) ;; *
		   (case-label . +) ;; *
		   (access-label . 0) ;; *
		   (label . 0) ;; *
		   (do-while-closure . 0)
		   (else-clause . 0)
		   (catch-clause . 0)
		   (comment-intro . c-lineup-comment)
		   (arglist-intro . c-lineup-arglist-intro-after-paren) ;; *
		   (arglist-cont . 0)
		   (arglist-cont-nonempty . c-lineup-arglist)
		   (arglist-close . c-lineup-arglist-close-under-paren) ;; *
		   (stream-op . c-lineup-streamop)
		   (inclass . +)
		   (cpp-macro . [0])
		   (cpp-macro-cont . c-lineup-dont-change)
		   (friend . 0)
		   (extern-lang-open . 0)
		   (extern-lang-close . 0)
		   (inextern-lang . +)
		   (inexpr-statement . 0)
		   (inexpr-class . c-lineup-inexpr-block) ;; *
		   ))))
  (c-add-style "xlre"
	       (cons
		(cons 'c-basic-offset lre-std-indent)
		'((c-backslash-column . 65)
		  (c-cleanup-list     . (scope-operator
					 list-close-comma
					 defun-close-semi
					 empty-defun-braces))
		  (c-offsets-alist    . (
					 (case-label         . +)
					 (knr-argdecl-intro  . c-lineup-arglist)
					 (statement-cont     . c-lineup-math)
				       ))
		 (c-hanging-braces-alist (brace-list-open)
					 (substatement-open after)
					 (class-open after)
					 (defun-open after)
					 (block-close . c-snug-do-while)
					 (extern-lang-open after))
		  )))
  )

(defvar lre--cstyle-loaded nil)

(defun lre-cc-common-init()
  "Initialize cc-mode"
  (lre-debug "lre-cc-common-init")
  (unless lre--cstyle-loaded
    (lre-c-style)
    (if (lre-memb 'cc-lobo)
	(let ((cc-lobotomy-pith-list '(literal)))
	  (require 'cc-lobotomy)
	  (cc-lobotomize)))
    (setq indent-tabs-mode nil
	  lre--cstyle-loaded t)))

(defmacro lre--cc-map ()
 (` (if (boundp 'c-mode-base-map) c-mode-base-map c-mode-map)))

(defun lre-cc-common ()
  "Personal C/C++/Java-mode defs.
\(require 'c-comment-edit \"c-comment-edit2\"\)"
  (lre-debug "lre-cc-common")
  (lre-cc-common-init)
  (when (lre-memb 'imenu)
    ;; (assoc "Macros" imenu-generic-expression)
    (setq imenu-generic-expression
	  (cons '("*Include*" "^#include[ \t]+.\\([^ \t\n\r]+\\)." 1)
		(cons '("*Macros*"
			"^#\\(define\\|undef\\)[ \t]+\\([a-zA-Z0-9_|]+\\)" 2)
		      imenu-generic-expression))))
  (lre--imenu-add)
  (abbrev-mode 0)
  (when (lre-memb 'keys)
    (if (and (lre-memb 'tvist)
	     lre-tvist-first-cf)
	(setq lre-tvist-key-description
	      (append lre-tvist-key-description
		      (list
		       "C - kun i C-mode: bytt til sysdul-mode"))
	      lre-tvist-first-cf nil))
    (define-key (lre--cc-map) "\C-c<" 'lre-sgml-tag)
    (define-key (lre--cc-map) "\C-cc" 'comment-dwim)
    (define-key (lre--cc-map) [?\C-c ?/] 'lre-c-comment)
    (define-key (lre--cc-map) "*" 'self-insert-command)
    (define-key (lre--cc-map) [kp-enter]
      (if (lre-memb 'e21+) 'c-context-line-break
	'newline-and-indent))
    (if (lre-memb 'e21+)
	(define-key (lre--cc-map) [C-return] 'c-context-line-break))
    (if (fboundp 'c-forward-into-nomenclature)
	(define-key (lre--cc-map) [C-M-right] 'c-forward-into-nomenclature)
    (if (fboundp 'c-beginning-of-defun)
	(define-key (lre--cc-map) "\C-\M-a" 'c-beginning-of-defun))
    (if (fboundp 'c-end-of-defun)
	(define-key (lre--cc-map) "\C-\M-e" 'c-end-of-defun))
      ))
  (and (lre-memb 'sccs)
       (lre-safe-require 'spe-sccs)
       (spe-sccs-mode 1))
  (lre-colors)
  (tplsub-mode 1)
  (lre-brace-mode 1)
  (make-variable-buffer-local 'dabbrev-case-fold-search)
  (make-variable-buffer-local 'dabbrev-case-replace)
  (setq c-echo-syntactic-information-p nil
	dabbrev-case-fold-search       nil
	dabbrev-case-replace	       nil
	delete-key-deletes-forward     nil
	c-comment-continuation-stars   " * "
	c-tab-always-indent            'other
	tplsub-tmpl-help	       t)
  (c-toggle-auto-hungry-state 1)
  (c-set-style "lre")
  (if (lre-memb 'e21+) (c-setup-filladapt))
;;  (setq c-font-lock-keywords	     c-font-lock-keywords-2
;;	 c++-font-lock-keywords	     c++-font-lock-keywords-2)
  (setq	c-comment-leader             " *"
	c-comment-edit-mode          'indented-text-mode
	c-comment-edit-empty-1-line  t))

(defun lre-c-mode ()
  (lre-cc-common)
  (setq c-recognize-knr-p  t))

(defun lre-c++-mode ()  (lre-cc-common))

;; Complicated regexp to match method declarations in interfaces or classes
;; A nasty test case is:
;;    else if(foo instanceof bar) {
;; To avoid matching this as a method named "if" must check that within
;; a parameter list there are an even number of symbols, i.e., one type name
;; paired with one variable name.  The trick there is to use the regexp
;; patterns \< and \> to match beginning and end of words.
(defvar java-function-regexp
  (concat
   "^[ \t]*"                                   ; leading white space
   "\\(public\\|private\\|protected\\|"        ; some of these 8 keywords
   "abstract\\|final\\|static\\|"
   "synchronized\\|native"
   "\\|[ \t\n\r]\\)*"                          ; or whitespace
   "[a-zA-Z0-9_$]+"                            ; return type
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\([a-zA-Z0-9_$]+\\)"                      ; the name we want!
   "[ \t\n\r]*"                                ; optional whitespace
   "("                                         ; open the param list
   "\\([ \t\n\r]*"                             ; optional whitespace
   "\\<[a-zA-Z0-9_$]+\\>"                      ; typename
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\<[a-zA-Z0-9_$]+\\>"                      ; variable name
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]*,?\\)*"                          ; opt whitespace and comma
   "[ \t\n\r]*"                                ; optional whitespace
   ")"                                         ; end the param list
   "[ \t\n\r]*"                                ; whitespace
;   "\\(throws\\([, \t\n\r]\\|[a-zA-Z0-9_$]\\)+\\)?{"
   "\\(throws[^{;]+\\)?"                       ; optional exceptions
   "[;{]"                                      ; ending ';' (interfaces) or '{'
   )
  "Matches method names in java code, select match 2")

(defvar java-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in java code, select match 2")

(defvar java-interface-regexp
  "^[ \t\n\r]*\\(abstract\\|public\\|[ \t\n\r]\\)*interface[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;]*;"
  "Matches interface names in java code, select match 2")

(defvar java-imenu-regexp
  (list (list nil java-function-regexp 2)
	(list ".CLASSES." java-class-regexp 2)
	(list ".INTERFACES." java-interface-regexp 2))
  "Imenu expression for Java")


(defadvice jde-run (around lre-jde-run-before activate)
  "Save and compile if needed."
  (if (and (buffer-modified-p)
	   (y-or-n-p "Buffer has changed.  Recompile instead? "))
      (jde-compile)
    ad-do-it))

(defadvice jde-run-applet (around lre-jde-run-applet-before activate)
  "Save and compile if needed."
  (if (and (buffer-modified-p)
	   (y-or-n-p "Buffer has changed.  Recompile instead? "))
      (jde-compile)
    ad-do-it))

(defun lre-java-mode ()
  (lre-cc-common)
  (setq imenu-generic-expression java-imenu-regexp)
  (lre--imenu-add)
;;	defun-prompt-regexp c-Java-defun-prompt-regexp  ; Skummel...
  (tplsub-set-according-to-mode))

(defun lre-jde-mode ()
  (if (listp 'align-c++-modes) (add-to-list 'align-c++-modes 'jde-mode))
;;  (if (featurep 'jde) (define-key-after  jde-menu-definition ????))
  (setq indent-tabs-mode nil)
  (lre-safe-require 'jsee)
  (lre-safe-require 'jmaker)
  (lre-safe-require 'decompile)
  (define-key jde-mode-map [(control \;)] 'jde-complete-at-point)
  (define-key jde-mode-map [(control C) (control Y)] 'jde-check))

(defun lre-jde-imenu-create ()
  (let ((mnu (semantic-create-imenu-index))
	arg index)
    (while mnu
      (setq arg (car mnu)
	    mnu (cd mnu)
	    index (append index (if (and (stringp (car arg))
					 (or (string= (car arg) "Classes")
					     (string= (car arg) "Package")))
				    (cdr arg)
				  (list arg)))))
    index  ;; !!!!!!!!!!!!!!!!
    ))

;;; ----------------------------------------------------------------------
;;; Hooks etc

(add-hook 'c-mode-hook 'lre-c-mode)
(add-hook 'c-initialization-hook 'lre-cc-common-init)
(add-hook 'c++-mode-hook 'lre-c++-mode)
(add-hook 'java-mode-hook 'lre-java-mode)
(add-hook 'jde-mode-hook 'lre-jde-mode)

(provide 'lrecjava)

;;; lrecjava.el ends here
