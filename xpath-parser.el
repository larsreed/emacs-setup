;;; xpath-parser.el --- XPATH parser

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: xml
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?XmlParser
;; Version: $Id: xpath-parser.el,v 1.8 2001/12/16 23:29:03 alex Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Used by xpath.el, tables created automatically from xpath.bnf.  The
;; main entry points are `xpath-lex-string' and `xpath-lex-region'.
;; These two functions prepare a list of preliminary tokens and store
;; them in the variable `xpath-token-input'.  Next, call `wisent-parse'
;; using `xpath-tables' and `xpath-pop-input' and an error function of
;; your liking: (wisent-parse xpath-tables #'xpath-pop-input #'error)
;;
;; `wisent-parse' then returns a list of elements STEP.  Each STEP has
;; the form (TEST PREDICATE).  Both TEST and PREDICATE have the form
;; (FUNC PARAMS...).  FUNC is always a function which must accept all
;; the PARAMS as arguments, plus a node.  The TEST FUNC must then return
;; a list of nodes, the PREDICATE must return either nil or non-nil.
;; The PREDICATE is used for filtering the list returned by TEST FUNC.
;;
;; See xpath.el for more information on all the functions used.

;;; Code:

(require 'wisent-bovine)

(defvar xpath-tables
  (eval-when-compile
  (wisent-compile-grammar
   '((NCNAME QNAME LITERAL NUMBER VARIABLEREFERENCE COLON AXISSUF DOTDOT AND OR LT GT LE GE NE STAR DIV MOD PLUS MINUS SLASH UNION LPAREN RPAREN LBRACK RBRACK AT DOT EQ COMMA COMMENT TEXT PROCESSING-INSTRUCTION NODE ANCESTOR ANCESTOR-OR-SELF ATTRIBUTE CHILD DESCENDANT DESCENDANT-OR-SELF FOLLOWING FOLLOWING-SIBLING NAMESPACE PARENT PRECEDING PRECEDING-SIBLING SELF POSITION LAST COUNT NAME)
     nil
     (TopExpr
      ((LocationPath)))
     (LocationPath
      ((RelativeLocationPath))
      ((AbsoluteLocationPath)))
     (AbsoluteLocationPath
      ((SLASH))
      ((SLASH RelativeLocationPath))
      ((AbbreviatedAbsoluteLocationPath)))
     (RelativeLocationPath
      ((Step))
      ((RelativeLocationPath SLASH Step)
       (append $1 $3 nil))
      ((AbbreviatedRelativeLocationPath)))
     (Step
      ((Basis predicates)
       (list
	(append $1 $2)))
      ((AbbreviatedStep)))
     (predicates
      (nil)
      ((predicates Predicate)
       (append $1 $2)))
     (Basis
      ((AxisName AXISSUF NodeTest)
       (list $1 $3))
      ((AbbreviatedBasis)))
     (AxisName
      ((ANCESTOR)
       'xpath-ancestor-axis)
      ((ANCESTOR-OR-SELF)
       'xpath-ancestor-or-self-axis)
      ((ATTRIBUTE)
       'xpath-attribute-axis)
      ((CHILD)
       'xpath-child-axis)
      ((DESCENDANT)
       'xpath-descendant-axis)
      ((DESCENDANT-OR-SELF)
       'xpath-descendant-or-self-axis)
      ((FOLLOWING)
       'xpath-following-axis)
      ((FOLLOWING-SIBLING)
       'xpath-following-sibling-axis)
      ((NAMESPACE)
       'xpath-namespace-axis)
      ((PARENT)
       'xpath-parent-axis)
      ((PRECEDING)
       'xpath-preceding-axis)
      ((PRECEDING-SIBLING)
       'xpath-sibling-axis)
      ((SELF)
       'xpath-self-axis))
     (NodeTest
      ((NameTest)
       (list 'xpath-name-filter $1))
      ((NodeType LPAREN Arglist RPAREN)
       (list 'xpath-node-type-filter $1))
      ((PROCESSING-INSTRUCTION LPAREN LITERAL RPAREN)))
     (Predicate
      ((LBRACK PredicateExpr RBRACK)
       (list $2)))
     (PredicateExpr
      ((Expr)))
     (AbbreviatedAbsoluteLocationPath
      ((SLASH SLASH RelativeLocationPath)))
     (AbbreviatedRelativeLocationPath
      ((RelativeLocationPath SLASH SLASH Step)))
     (AbbreviatedStep
      ((DOT))
      ((DOTDOT)))
     (AbbreviatedBasis
      ((NodeTest))
      ((AT NodeTest)))
     (Expr
      ((OrExpr)))
     (PrimaryExpr
      ((VARIABLEREFERENCE))
      ((LPAREN Expr RPAREN))
      ((LITERAL))
      ((NUMBER))
      ((FunctionCall)))
     (FunctionCall
      ((FunctionName LPAREN Arglist RPAREN)
       (append
	(list $1)
	$3)))
     (FunctionName
      ((POSITION)
       'xpath-position-function)
      ((LAST)
       'xpath-last-function)
      ((COUNT)
       'xpath-count-function)
      ((NAME)
       'xpath-name-function))
     (Arglist
      (nil)
      ((Arguments)))
     (Arguments
      ((Argument)
       (list $1))
      ((Arguments COMMA Argument)
       (append $1
	       (list $3))))
     (Argument
      ((Expr)))
     (UnionExpr
      ((PathExpr))
      ((UnionExpr UNION PathExpr)))
     (PathExpr
      ((LocationPath)
       (list 'xpath-resolve-steps 'xpath-context-node
	     (list 'quote $1)))
      ((FilterExpr))
      ((FilterExpr SLASH RelativeLocationPath))
      ((FilterExpr SLASH SLASH RelativeLocationPath)))
     (FilterExpr
      ((PrimaryExpr))
      ((FilterExpr Predicate)))
     (OrExpr
      ((AndExpr))
      ((OrExpr OR AndExpr)))
     (AndExpr
      ((EqualityExpr))
      ((AndExpr AND EqualityExpr)))
     (EqualityExpr
      ((RelationalExpr))
      ((EqualityExpr EQ RelationalExpr)
       (list 'xpath-equal $1 $3))
      ((EqualityExpr NE RelationalExpr)))
     (RelationalExpr
      ((AdditiveExpr))
      ((RelationalExpr LT AdditiveExpr))
      ((RelationalExpr GT AdditiveExpr))
      ((RelationalExpr LE AdditiveExpr))
      ((RelationalExpr GE AdditiveExpr)))
     (AdditiveExpr
      ((MultiplicativeExpr))
      ((AdditiveExpr PLUS MultiplicativeExpr))
      ((AdditiveExpr MINUS MultiplicativeExpr)))
     (MultiplicativeExpr
      ((UnaryExpr))
      ((MultiplicativeExpr STAR UnaryExpr))
      ((MultiplicativeExpr DIV UnaryExpr))
      ((MultiplicativeExpr MOD UnaryExpr)))
     (UnaryExpr
      ((UnionExpr))
      ((MINUS UnaryExpr)))
     (NameTest
      ((STAR))
      ((NCNAME COLON STAR))
      ((QNAME)))
     (NodeType
      ((COMMENT))
      ((TEXT))
      ((PROCESSING-INSTRUCTION))
      ((NODE))))
   'nil))
"Table for use with semantic for parsing XPATH.")

(defvar xpath-keywords
  (semantic-flex-make-keyword-table 
   `( ("" . NCNAME)
      ("" . QNAME)
      ("" . LITERAL)
      ("" . NUMBER)
      ("" . VARIABLEREFERENCE)
      ("and" . AND)
      ("or" . OR)
      ("div" . DIV)
      ("mod" . MOD)
      ("comment" . COMMENT)
      ("text" . TEXT)
      ("processing-instruction" . PROCESSING-INSTRUCTION)
      ("node" . NODE)
      ("ancestor" . ANCESTOR)
      ("ancestor-or-self" . ANCESTOR-OR-SELF)
      ("attribute" . ATTRIBUTE)
      ("child" . CHILD)
      ("descendant" . DESCENDANT)
      ("descendant-or-self" . DESCENDANT-OR-SELF)
      ("following" . FOLLOWING)
      ("following-sibling" . FOLLOWING-SIBLING)
      ("namespace" . NAMESPACE)
      ("parent" . PARENT)
      ("preceding" . PRECEDING)
      ("preceding-sibling" . PRECEDING-SIBLING)
      ("self" . SELF)
      ("position" . POSITION)
      ("last" . LAST)
      ("count" . COUNT)
      ("name" . NAME)
      )
   '(
     ))
  "Table for use with semantic for XPATH keywords.")

(defvar xpath-tokens
  '((close-paren
     (RBRACK . "]")
     (RPAREN . ")"))
    (open-paren
     (LBRACK . "[")
     (LPAREN . "("))
    (operator
     (COMMA . ",")
     (EQ . "=")
     (DOT . ".")
     (AT . "@")
     (UNION . "|")
     (SLASH . "/")
     (MINUS . "-")
     (PLUS . "+")
     (STAR . "*")
     (NE . "!=")
     (GE . ">=")
     (LE . "<=")
     (GT . ">")
     (LT . "<")
     (DOTDOT . "..")
     (AXISSUF . "::")
     (COLON . ":")))
  "Table for use with semantic for tokens.")

(defun xpath-default-setup ()
  "XPATH parsing setup function."
  ;; Code generated from xpath.bnf
  (setq semantic-toplevel-bovine-table xpath-tables
	semantic-toplevel-bovine-table-source "xpath.bnf")
  (setq semantic-flex-keywords-obarray xpath-keywords)
 
  ;; End code generated from xpath.bnf
  (setq semantic-flex-syntax-modifications
	'((?/ ".") (?* ".") (?= "."))
	semantic-flex-depth 64))

;;; Lexer

(defvar xpath-token-input nil
  "The parsed XPATH tokens created by `xpath-lex-region'.
The elements in this list are returned one by one using
`xpath-pop-input'.")

(defun xpath-pop-input ()
  "Pop an element from `xpath-token-input'.
If the list is empty, return `wisent-eoi-term' in a list."
  (or (pop xpath-token-input) (list wisent-eoi-term)))

(defconst xpath-qname-regexp "^[a-zA-Z_][a-zA-Z_0-9.-_]*$"
  "Regular expression which identifies a QNAME symbol.")

(defconst xpath-literal-regexp "^\"\\([^\"]*\\)\""
  "Regular expression which identifies a LITERAL symbol.
Actually, it identifies strings.  The first paren group
will be considered the value of the literal.")

(defun xpath-lex-region (start end)
  "Lex the region for XPATH.
This calls `semantic-flex' on the region, munges the result,
and stores a list of tokens in `xpath-token-input'.  The
tokens will be available via `xpath-pop-input' and are
suitable for `wisent-parse' consumption."
  (xpath-default-setup)
  (let ((objs (semantic-flex start end))
	obj token category key text result)
    (while objs
      (setq obj (car objs)
	    objs (cdr objs)
	    category (car obj)
	    pos (cdr obj)
	    text (semantic-flex-text obj)
	    key (cond ((semantic-flex-keyword-p text))
		      ((semantic-flex-token-key xpath-tokens category text))
		      ((eq category 'number)
		       'NUMBER)
		      ((eq category 'punctuation)
		       ;; Concat all successive punctuations
		       (let ((os objs)
			     operator)
			 (setq obj (car os)
			       category (car obj))
			 (while (and obj (eq category 'punctuation))
			   (setq text (concat text (semantic-flex-text obj))
				 os (cdr os)
				 obj (car os)
				 category (car obj)))
			 ;; Starting with the longest punctuation string
			 ;; search if it matches an operator.
			 (while (and (> (length text) 0)
				     (not (setq operator
						(semantic-flex-token-key
						 xpath-tokens 'operator text))))
			   (setq text (substring text 0 -1)))
			 (unless operator
			   (error "Invalid punctuation %s in input" text))
			 ;; Adjust input stream and end position of token.
			 (let ((len (length text))
			       (start (car pos)))
			   (setq objs (nthcdr (1- len) objs)
				 pos (cons start (+ start len))))
			 operator))
		      ((string-match xpath-literal-regexp text)
		       (setq text (match-string 1 text))
		       'LITERAL)
		      ((string-match xpath-qname-regexp text)
		       'QNAME)
		      (t
		       (error "Cannot lex input \"%s\" at %s" text pos)))
	    result (cons (append (list key text) pos) result)))
    (setq xpath-token-input (nreverse result))))

(defun xpath-lex-string (str)
  "Lex the string STR for XPATH.
This uses `xpath-lex-region', which see."
  (with-temp-buffer
    (insert str)
    (xpath-lex-region (point-min) (point-max))))

(defun xpath-steps (str)
  "Return the XPATH steps for string STR."
  (xpath-lex-string str)
  (wisent-parse xpath-tables #'xpath-pop-input #'error))

;;; Test stuff

(eval-when-compile
  (xpath-default-setup)
  (with-temp-buffer
    (xpath-default-setup)
    (insert "child::para/parent::text()")
    (semantic-flex (point-min) (point-max)))
  (semantic-flex-keyword-p "child")
  (semantic-flex-keyword-p ":")
  (semantic-flex-keyword-p "::")
  (assert (equal (xpath-steps "child::para")
		 '((xpath-child-axis (xpath-name-filter "para")))))
  (assert (equal (xpath-steps "child::para/parent::*")
		 '((xpath-child-axis (xpath-name-filter "para"))
		   (xpath-parent-axis (xpath-name-filter "*")))))
  (assert (equal (xpath-steps "child::para/parent::text()")
		 '((xpath-child-axis (xpath-name-filter "para"))
		   (xpath-parent-axis (xpath-node-type-filter "text")))))
  (assert (equal (xpath-steps "child::*")
		 '((xpath-child-axis (xpath-name-filter "*")))))
  (assert (equal (xpath-steps "child::foo/child::bar/child::test")
		 '((xpath-child-axis (xpath-name-filter "foo"))
		   (xpath-child-axis (xpath-name-filter "bar"))
		   (xpath-child-axis (xpath-name-filter "test")))))
  (assert (equal (with-temp-buffer
		   (xpath-default-setup)
		   (insert "child::*[position() = 1]")
		   (semantic-flex (point-min) (point-max)))
		 '((CHILD 1 . 6)
		   (punctuation 6 . 7)
		   (punctuation 7 . 8)
		   (punctuation 8 . 9)
		   (open-paren 9 . 10)
		   (POSITION 10 . 18)
		   (open-paren 18 . 19)
		   (close-paren 19 . 20)
		   (punctuation 21 . 22)
		   (number 23 . 24)
		   (close-paren 24 . 25))))
  (assert (equal (xpath-lex-string "child::*[position() = 1]")
		 '((CHILD "child" 1 . 6)
		   (AXISSUF "::" 6 . 8)
		   (STAR "*" 8 . 9)
		   (LBRACK "[" 9 . 10)
		   (POSITION "position" 10 . 18)
		   (LPAREN "(" 18 . 19)
		   (RPAREN ")" 19 . 20)
		   (EQ "=" 21 . 22)
		   (NUMBER "1" 23 . 24)
		   (RBRACK "]" 24 . 25))))
  (assert (equal (xpath-steps "child::*[position() = 1]")
		 '((xpath-child-axis (xpath-name-filter "*") 
				     (xpath-equal (xpath-position-function) "1")))))
  (assert (equal (xpath-steps "child::*[position(1,2,3,4) = 1]")
		 '((xpath-child-axis (xpath-name-filter "*")
				     (xpath-equal (xpath-position-function
						   "1" "2" "3" "4")
						  "1")))))
  (assert (equal (xpath-steps "child::*[attribute::type=\"id\"]")
		 '((xpath-child-axis (xpath-name-filter "*")
				     (xpath-equal (xpath-resolve-steps
						   xpath-context-node
						   (quote ((xpath-attribute-axis
							    (xpath-name-filter "type")))))
						  "id"))))))
(provide 'xpath-parser)

;;; xpath-parser.el ends here
