;;; JDE-CHECKSTYLE.EL --- Checkstyle interface for JDE
;; $Revision: 1.2 $ $Date: 2002/09/10 04:48:56 $

;; Copyright (C) 2001, 2002 Markus Mohnen and Paul Kinnucan

;; Authors: Markus Mohnen and Paul Kinnucan
;; Maintainers: Markus Mohnen and Paul Kinnucan
;; Created: 06 Jun 2001
;;
;;
;; Keywords: Java coding standard checker Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; ) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; jde-checkstyle|Markus Mohnen|
;; |Checkstyle interface for JDE
;; |$Date: 2002/09/10 04:48:56 $|$Revision: 1.2 $|~/packages/jde-checkstyle.el

;;; Commentary:

;;; This package provides an interface from JDE (see
;;; http://jde.sunsite.dk/) to Oliver Burn's CheckStyle (see
;;; http://checkstyle.sourceforge.net/) a development tool
;;; to help programmers write Java code that adheres to a coding
;;; standard.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jde-checkstyle)

;;; Usage:
;;
;;  M-x `jde-checkstyle' to check the java file in the current buffer.
;;

;;; Customization:
;;
;;  M-x `jde-checkstyle-customize' to customize all the jde-checkstyle options.

;;; Code:

(require 'jde-compile)

(if (fboundp 'jde-build-classpath)
    nil
  (require 'jde-run)
  (defalias 'jde-build-classpath 'jde-run-build-classpath-arg)
  )

(defconst jde-checkstyle-version "2.3")

(defgroup jde-checkstyle nil
  "JDE Checkstyle Options"
  :group 'jde
  :prefix "jde-checkstyle-option-")

(defcustom jde-checkstyle-class "com.puppycrawl.tools.checkstyle.Main"
  "*Java checker class.
Specifies the class of the the program to be used to check the source
in the current buffer. The default is the checkstyle program."
  :group 'jde-checkstyle
  :type 'string)

(defcustom jde-checkstyle-classpath nil
  "*Specify paths of classes required to run the jde-checkstyle application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-checkstyle
  :type '(repeat (file :tag "Path")))

(defcustom jde-read-checkstyle-args nil
  "*Specify whether to prompt for additional checker arguments.
If this variable is non-nil, the jde-checkstyle command prompts
you to enter additional checker arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments
entered in the minibuffer."
  :group 'jde-checkstyle
  :type 'boolean)

(defvar jde-interactive-checkstyle-args ""
  "String of checker arguments entered in the minibuffer.")

(defvar jde-interactive-compile-arg-history nil
  "History of checker arguments entered in the minibuffer.")

(defcustom jde-checkstyle-option-config-file nil
  "*Name of config-file containing checkstyle options"
  :group 'jde-checkstyle
  :type 'file)

(defcustom jde-checkstyle-option-member-format "^[a-z][a-zA-Z0-9]*$"
  "*Regular expression that specifies the format of nonstatic nonpublic
member variables."
  :group 'jde-checkstyle
  :type 'regexp)

(defcustom jde-checkstyle-option-public-member-format "^f[A-Z][a-zA-Z0-9]*$"
  "*Regular expression that specifies the format of public member variables.
Default example:

  public int fCMPField;"
  :group 'jde-checkstyle
  :type 'regexp)

(defcustom jde-checkstyle-option-constant-format "^[A-Z](_?[A-Z0-9]+)*$"
  "*Regular expression that specifies the format of final static variables.
Default example:

  public static final int MAX_ROWS = 2;"
  :group 'jde-checkstyle
  :type 'regexp)

(defcustom jde-checkstyle-option-static-format "^[a-z][a-zA-Z0-9]*$"
  "*Regular expression that specifies the format of static variables.
Default example:

  private static int numCreated = 0;"
  :group 'jde-checkstyle
  :type 'regexp)

(defcustom jde-checkstyle-option-todo-format "TODO:"
  "Regular expression that specifies the format of to-do comments."
  :group 'jde-checkstyle
  :type 'regexp)


(defcustom jde-checkstyle-option-parameter-format "^[a-z][a-zA-Z0-9]*$"
    "Regular expression that specifies the format of parameter names."
  :group 'jde-checkstyle
  :type 'regexp)


(defcustom jde-checkstyle-option-type-format "^[A-Z][a-zA-Z0-9]*$"
    "Regular expression that specifies the format of type names."
  :group 'jde-checkstyle
  :type 'regexp)

(defcustom jde-checkstyle-option-method-format "^[a-z][a-zA-Z0-9]*$"
    "Regular expression that specifies the format of method names."
  :group 'jde-checkstyle
  :type 'regexp)

(defcustom jde-checkstyle-option-localvar-format "^[a-z][a-zA-Z0-9]*$"
    "Regular expression that specifies the format of local variable names."
  :group 'jde-checkstyle
  :type 'regexp)

;;(makunbound 'jde-checkstyle-option-lcurly-type)
(defcustom jde-checkstyle-option-lcurly-type  (list "eol")
  "Specifies the policy of where to put the left brace '{' for type
declarations. The legal values are defined are:

ignore - Ignore the placement of the brace.
eol    - The brace must always be on the end of the line. For example:

    if (condition) {
        ...

nl     - The brace must always be on a new line. For example:

    if (condition)
    {
        ...

nlow - If the brace will fit on the first line of the statement, taking into
       account maximum line length, then apply eol rule. Otherwise apply the nl
       rule. nlow is a mnemonic for 'new line on wrap'. For the example above
       Checkstyle will enforce:

    if (condition) {
        ...

       But for a statement spanning multiple lines, Checkstyle will enforce:

    if (condition1 && condition2 &&
        condition3 && condition4)
    {
        ...
"
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Left brace placement"
	   (item "ignore")
	   (item "eol")
	   (item "nl")
	   (item "nlow"))))



(defcustom jde-checkstyle-option-lcurly-method  (list "eol")
  "Specifies the policy of where to put the left brace '{' for method
declarations. The legal values are the same ones allowed for `jde-checkstyle-option-lcurly-type'.
"
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Left brace placement"
	   (item "ignore")
	   (item "eol")
	   (item "nl")
	   (item "nlow"))))

(defcustom jde-checkstyle-option-lcurly-other  (list "eol")
  "Specifies the policy of where to put the left brace '{' for other
declarations. The keywords covered are switch, while, for, do, if, else,
synchronized, try, catch, finally and static. The legal values are the same ones
allowed for `jde-checkstyle-option-lcurly-type'.
"
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Left brace placement"
	   (item "ignore")
	   (item "eol")
	   (item "nl")
	   (item "nlow"))))


(defcustom jde-checkstyle-option-rcurly  (list "same")
  "Specifies the policy of where to put the right brace '}'. The keywords
covered are else, catch and finally. The legal values are:

ignore - Ignore the placement of the brace.
same   The brace must be on the same line as the next statement. For example:

    try {
        ...
    } finally {

alone - The brace must be alone on the line. For example:

    try {
        ...
    }
    finally {
"
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Right brace placement"
	   (item "ignore")
	   (item "same")
	   (item "alone"))))


(defcustom jde-checkstyle-option-allow-paren-padding  nil
  "Specifies whether to allow padding of parentheses. The default
is no padding, e.g.,  method(a, b);"
  :group 'jde-checkstyle
  :type 'boolean)


(defcustom jde-checkstyle-option-block-try  (list "stmt")
  "Specifies the policy for contents of try blocks. The legal values are:

ignore - Ignore empty blocks.
text   - Require that there is some text in the block. For example:

    catch (Exception ex) {
        // This is a bad coding practice
    }

stmt - Require that there is a statement in the block. For example:

    finally {
        lock.release();
    }
"
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Required try block content"
	   (item "ignore")
	   (item "text"
	   (item "stmt")))))


(defcustom jde-checkstyle-option-block-catch  (list "text")
  "Specifies the policy for the contents of catch blocks. The legal values are the same
as the ones for `jde-checkstyle-option-block-try'.
"
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Required try block content"
	   (item "ignore")
	   (item "text"
	   (item "stmt")))))

(defcustom jde-checkstyle-option-block-finally  (list "text")
  "Specifies the policy of how check finally blocks. The legal values are the same
as the ones for `jde-checkstyle-option-block-try'.
"
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Required try block content"
	   (item "ignore")
	   (item "text"
	   (item "stmt")))))

(defcustom jde-checkstyle-option-wrap-operator  t
  "Specifies the policy of how to wrap on operators. The legal values are:

ignore - Ignore wrapping on an operator.
nl     - The operator must be on a new line. For example:
    someVariable = aBigVariableNameToMakeThings + thisMayWork
                   + lookVeryInteresting;
"
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-allow-protected  nil
  "Indicates whether to allow protected data."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-allow-tabs  nil
  "Indicates whether to allow tabs."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-allow-package  nil
  "Indicates whether to allow package visible data."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-allow-noauthor  nil
  "Indicates whether to allow no @author tag to be defined for class and interface Javadoc comments."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-maxlinelen 80
  "Specifies the maximum line length."
  :group 'jde-checkstyle
  :type 'integer)

(defcustom jde-checkstyle-option-tab-width  8
  "The distance between tab stops, used in line number and column calculations."
  :group 'jde-checkstyle
  :type 'integer)

(defcustom jde-checkstyle-option-ignore-maxlinelen  "^$"
  "Regular expression that matches lines to ignore in maximum line length checks."
  :group 'jde-checkstyle
  :type 'regexp)

(defcustom jde-checkstyle-option-ignore-importlength  nil
  "Specifies whether to ignore the maximum line length for import statements."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-maxmethodlen 150
  "Specifies the maximum number of lines in a method."
  :group 'jde-checkstyle
  :type 'integer)

(defcustom jde-checkstyle-option-maxconstructorlen 150
  "Specifies the maximum constructor length."
  :group 'jde-checkstyle
  :type 'integer)

(defcustom jde-checkstyle-option-maxfilelen 2000
  "Specifies the maximum file length."
  :group 'jde-checkstyle
  :type 'integer)


(defcustom jde-checkstyle-option-header-file ""
  "Specifies the path of a file containing the boilerplate text that
should appear at the head of each source file. Checkstyle issues a
warning if the head of the current source file does not match the
contents of the header file. If no file is
specified, checkstyle does not check for a header."
  :group 'jde-checkstyle
  :type 'file)

(defcustom jde-checkstyle-option-header-regexp  nil
  "Specifies whether to interpret each line in the
`jde-checkstyle-option-header-file' as a regular expression.
Checkstyle also supports interpreting each header line as
a regular expression. For example, consider the following header when
regular expression checking is turned on:

line 1: /{71}
line 2: // checkstyle: Checks Java source code for adherence to a set of rules\.
line 3: // Copyright \(C\) \d\d\d\d  Oliver Burn
line 4: // Last modification by \$Author.*\$
line 5: /{71}

Lines 1 and 5 demonstrate a more compact notation for 71 '/' characters. Line 3
enforces that the copyright notice includes a four digit year. Line 4 is an
example how to enforce revision control keywords in a file header.
"
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-header-ignoreline nil
  "Specifies a list of the lines in the header to ignore when
comparing. Default is to not ignore any line.  Most copyright headers will
contain date information (for example the year) which will change over time. To
support this, checkstyle supports specifying the lines in the header to ignore
when comparing the start of the file with the header. For example, consider the
following header:

line 1: ///////////////////////////////////////////////////////////////////////
line 2: // checkstyle: Checks Java source code for adherence to a set of rules.
line 3: // Copyright (C) 2001  Oliver Burn
line 4: ///////////////////////////////////////////////////////////////////////
Since the year information will change over time, you can tell checkstyle to ignore line 3.
"
  :group 'jde-checkstyle
  :type '(repeat (integer :tag "line no")))

(defcustom jde-checkstyle-option-javadoc-scope (list "private")
  "Specifies the visibility scope where javadoc comments are checked."
  :group 'jde-checkstyle
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Javadoc scope"
	   (item "nothing")
	   (item "public")
	   (item "protected")
	   (item "package")
	   (item "private")
	   (item "anoninner"))))

(defcustom jde-checkstyle-option-require-packagehtml  nil
  "Specifies whether to require that package documentation is available."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-ignore-imports  nil
  "Specifies whether to ignore checking import statements."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-illegal-imports  (list "sun")
  "List of package prefixes that are not allowed in import statements."
  :group 'jde-checkstyle
  :type '(repeat (string :tag "Package")))

(defcustom jde-checkstyle-option-illegal-instantiations  nil
  "List of fully qualified class names that are not allowed to
be instantiated. A common mistake is to create new instances of
java.lang.Boolean instead of using the constants TRUE and FALSE or the
Boolean.valueOf() factory methods. This increases the program's memory
requirements and wastes CPU cycles during memory allocation and garbage
collection.  To find this error automatically, include java.lang.Boolean in the
list of illegal instantiations.
"
  :group 'jde-checkstyle
  :type '(repeat (string tag: "Class")))

(defcustom jde-checkstyle-option-ignore-whitespace  nil
  "Specifies whether to ignore checking whitespace."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-ignore-whitespace-cast  nil
  "Specifies whether to ignore checking for whitespace after a cast."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-ignore-braces  nil
  "Specifies whether to ignore checking braces."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-ignore-longell  nil
  "Specifies whether to ignore checking the L in long integer literals."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-ignore-public-in-interface  nil
  "Specifies whether to ignore the public keyword in interface definitions."
  :group 'jde-checkstyle
  :type 'boolean)

(defcustom jde-checkstyle-option-cache-file ""
  "Specifies the name of a file that can be used to cache details of files that
pass checkstyle. This can signicantly increase the speed of checkstyle on
successive runs."
  :group 'jde-checkstyle
  :type 'file)

(defcustom jde-checkstyle-option-javadoc-check-unused-throws  nil
  "Specifies whether to check if unused @throws tags are subclasses of java.lang.RuntimeException."
  :group 'jde-checkstyle
  :type 'boolean)

;; (makunbound 'jde-checkstyle-option-basedir)
(defcustom jde-checkstyle-option-basedir ""
  "Specifies a base directory for reporting file names relative to .For example,
if a base directory is specified as c:\\projects\\checkstyle, then an error in the
file c:\\projects\\checkstyle\\src\\dir\\subdir\\File.java will be reported as
src\\dir\\subdir\\File.java."
  :group 'jde-checkstyle
  :type 'directory)

(defun jde-checkstyle-get-options ()
  "Constructs a command-line argument string for checker.
The string consists of the contents of the jde-checkstyle-options
variable concatenated with the various jde-checkstyle-option
settings.
"
  (if jde-checkstyle-option-config-file
      nil
    (let (options)

      (if (not (string= jde-checkstyle-option-member-format "^[a-z][a-zA-Z0-9]*$"))
	  (add-to-list 'options (concat "-Dcheckstyle.pattern.member=" jde-checkstyle-option-member-format)))

      (if (not (string= jde-checkstyle-option-public-member-format "^f[A-Z][a-zA-Z0-9]*$"))
	  (add-to-list 'options (concat "-Dcheckstyle.pattern.publicmember="
					jde-checkstyle-option-public-member-format)))

      (if (not (string= jde-checkstyle-option-constant-format "^[A-Z](_?[A-Z0-9]+)*$"))
	  (add-to-list 'options (concat "-Dcheckstyle.pattern.const=" jde-checkstyle-option-constant-format)))

      (if (not (string= jde-checkstyle-option-static-format "^[a-z][a-zA-Z0-9]*$"))
	  (add-to-list 'options (concat "-Dcheckstyle.pattern.static=" jde-checkstyle-option-static-format)))

      (if (not (string= jde-checkstyle-option-todo-format "TODO:"))
	  (concat "-Dcheckstyle.pattern.todo=" jde-checkstyle-option-todo-format))

      (if (not (string= jde-checkstyle-option-parameter-format "^[a-z][a-zA-Z0-9]*$"))
	  (add-to-list 'options  (concat "-Dcheckstyle.pattern.parameter="
					 jde-checkstyle-option-parameter-format)))


      (if (not (string= jde-checkstyle-option-type-format "^[A-Z][a-zA-Z0-9]*$"))
	  (add-to-list 'options  (concat "-Dcheckstyle.pattern.type=" jde-checkstyle-option-type-format)))

      (if (not (string= jde-checkstyle-option-method-format "^[a-z][a-zA-Z0-9]*$"))
	  (add-to-list 'options  (concat "-Dcheckstyle.pattern.method="
					 jde-checkstyle-option-method-format)))

      (if (not (string= jde-checkstyle-option-localvar-format "^[a-z][a-zA-Z0-9]*$"))
	  (add-to-list 'options  (concat "-Dcheckstyle.pattern.localvar="
					 jde-checkstyle-option-localvar-format)))

      (if (not (string= (car jde-checkstyle-option-lcurly-type) "eol"))
	  (add-to-list 'options  (concat "-Dcheckstyle.lcurly.type=" (car jde-checkstyle-option-lcurly-type))))

      (if (not (string= (car jde-checkstyle-option-lcurly-method) "eol"))
	  (add-to-list 'options  (concat "-Dcheckstyle.lcurly.method=" (car jde-checkstyle-option-lcurly-type))))


      (if (not (string= (car jde-checkstyle-option-lcurly-other) "eol"))
	  (add-to-list 'options  (concat "-Dcheckstyle.lcurly.other=" (car jde-checkstyle-option-lcurly-other))))

      (if (not (string= (car jde-checkstyle-option-rcurly) "same"))
	  (add-to-list 'options  (concat "-Dcheckstyle.rcurly=" (car jde-checkstyle-option-rcurly))))

      (if jde-checkstyle-option-allow-paren-padding
	  (add-to-list 'options  "-Dcheckstyle.paren.pad=ignore"))

      (if (not (string= (car jde-checkstyle-option-block-try) "stmt"))
	  (add-to-list 'options  (concat "-Dcheckstyle.block.try=" (car jde-checkstyle-option-block-try))))

      (if (not (string= (car jde-checkstyle-option-block-catch) "text"))
	  (add-to-list 'options  (concat "-Dcheckstyle.block.catch=" (car jde-checkstyle-option-block-catch))))

      (if (not (string= (car jde-checkstyle-option-block-finally) "text"))
	  (add-to-list 'options  (concat "-Dcheckstyle.block.finally="
					 (car jde-checkstyle-option-block-finally))))

      (if (not  jde-checkstyle-option-wrap-operator)
	  (add-to-list 'options  "-Dcheckstyle.wrap.operator=ignore"))

      (if jde-checkstyle-option-allow-protected  (add-to-list 'options "-Dcheckstyle.allow.protected=yes"))

      (if jde-checkstyle-option-allow-tabs  (add-to-list 'options  "-Dcheckstyle.allow.tabs=yes"))

      (if jde-checkstyle-option-allow-package (add-to-list 'options  "-Dcheckstyle.allow.package=yes"))

      (if jde-checkstyle-option-allow-noauthor (add-to-list 'options "-Dcheckstyle.allow.noauthor=yes"))

      (if (not (= jde-checkstyle-option-maxlinelen 80))
	  (add-to-list 'options (concat "-Dcheckstyle.maxlinelen="
					(int-to-string jde-checkstyle-option-maxlinelen))))

      (if (not (= jde-checkstyle-option-tab-width 8))
	  (add-to-list 'options (concat "-Dcheckstyle.tab.width="
					(int-to-string jde-checkstyle-option-tab-width))))

      (if (not (string= jde-checkstyle-option-ignore-maxlinelen "^$"))
	  (add-to-list 'options  (concat "-Dcheckstyle.ignore.maxlinelen="
					 jde-checkstyle-option-ignore-maxlinelen)))

      (if jde-checkstyle-option-ignore-importlength
	  (add-to-list 'options "-Dcheckstyle.ignore.importlength=yes"))


      (if (not (= jde-checkstyle-option-maxmethodlen 150))
	  (add-to-list 'options (concat "-Dcheckstyle.maxmethodlen="
					(int-to-string jde-checkstyle-option-maxmethodlen))))

      (if (not (= jde-checkstyle-option-maxconstructorlen 150))
	  (add-to-list 'options  (concat "-Dcheckstyle.maxconstructorlen="
					 (int-to-string jde-checkstyle-option-maxconstructorlen))))

      (if (not (= jde-checkstyle-option-maxfilelen 2000))
	  (add-to-list 'options (concat "-Dcheckstyle.maxfilelen="
					(int-to-string jde-checkstyle-option-maxfilelen))))

      (if (not (string= jde-checkstyle-option-header-file ""))
	  (add-to-list 'options  (concat "-Djde-checkstyle-option-header-file=" checkstyle.header.file)))

      (if jde-checkstyle-option-header-regexp (add-to-list 'options "-Dcheckstyle.header.regexp=yes"))

      (if jde-checkstyle-option-header-ignoreline
	  (add-to-list 'options
		       (concat "-Dcheckstyle.header.ignoreline="
			       (mapconcat
				(lambda (line)
				  (int-to-string line))
				jde-checkstyle-option-header-ignoreline
				","))))

      (if (not (string= (car jde-checkstyle-option-javadoc-scope) "private"))
	  (add-to-list 'options  (concat "-Dcheckstyle.javadoc.scope=" (car jde-checkstyle-option-javadoc-scope))))

      (if jde-checkstyle-option-require-packagehtml (add-to-list 'options "-Dcheckstyle.require.packagehtml=yes"))

      (if jde-checkstyle-option-ignore-imports
	  (add-to-list 'options "-Dcheckstyle.ignore.imports=yes")

	(if (not
	     (and
	      (= (length jde-checkstyle-option-illegal-imports) 1)
	      (string= (car jde-checkstyle-option-illegal-imports) "sun")))
	    (add-to-list
	     'options
	     (concat
	      "-Dcheckstyle.illegal.imports="
	      (mapconcat
	       'identity
	       jde-checkstyle-option-illegal-imports
	       ","))))

	(if jde-checkstyle-option-illegal-instantiations
	    (add-to-list
	     'options
	     (concat
	      "-Dcheckstyle.illegal.instantiations="
	      (mapconcat
	       'identity
	       jde-checkstyle-option-illegal-instantiations
	       ","))))

	(if jde-checkstyle-option-ignore-whitespace
	    (add-to-list 'options  "-Dcheckstyle.ignore.whitespace=yes"))

	(if jde-checkstyle-option-ignore-whitespace-cast
	    (add-to-list 'options  "-Dcheckstyle.ignore.whitespace.cast=yes"))

	(if jde-checkstyle-option-ignore-braces (add-to-list 'options  "-Dcheckstyle.ignore.braces=yes"))

	(if jde-checkstyle-option-ignore-longell (add-to-list 'options  "-Dcheckstyle.ignore.longell=yes"))

	(if jde-checkstyle-option-ignore-public-in-interface
	    (add-to-list 'options   "-Dcheckstyle.ignore.public.in.interface=yes"))

	(if (not (string= jde-checkstyle-option-cache-file ""))
	    (add-to-list 'options  (concat "-Dcheckstyle.cache.file="  jde-checkstyle-option-cache-file)))

	(if jde-checkstyle-option-javadoc-check-unused-throws
	    (add-to-list 'options "-Dcheckstyle.javadoc.checkUnusedThrows=yes"))

	(if (not (string= jde-checkstyle-option-basedir ""))
	    (add-to-list 'options  (concat "-Dcheckstyle.basedir=" jde-checkstyle-option-basedir)))

	options))))

;;;###autoload
(defun jde-checkstyle-customize ()
  "Customization of the group jde-checkstyle."
  (interactive)
  (customize-group "jde-checkstyle"))
(defun jde-make-checkstyle-command (more-args)
  "Constructs the java checker command as: jde-checker + options + buffer file name."
  (concat
   (if (string< jde-version "2.2.9")
       (if (eq system-type 'windows-nt)
           jde-run-java-vm-w
         jde-run-java-vm)
     (oref (jde-run-get-vm) :path))
   (jde-get-checkstyle-options)
   (if (not (string= more-args "")) (concat " " more-args))
   " "
   jde-checkstyle-class
   " "
   (file-name-nondirectory buffer-file-name)))


(defclass jde-checkstyle-checker ()
  ((buffer           :initarg :buffer
	             :type buffer
	             :documentation
	             "Compilation buffer")
   (window           :initarg :window
		     :type window
		     :documentation
		     "Window that displays the compilation buffer.")
   (interactive-args :initarg :interactive-args
		     :type list
		     :documentation
		     "Arguments entered in the minibuffer."))
  "Class of Java style checkers.")

(defmethod jde-checkstyle-create-checker-buffer ((this jde-checkstyle-checker))
  (save-excursion
    (let ((buf (get-buffer-create "*check style*"))
	  (error-regexp-alist compilation-error-regexp-alist)
	  (enter-regexp-alist (if (not jde-xemacsp) compilation-enter-directory-regexp-alist))
	  (leave-regexp-alist (if (not jde-xemacsp) compilation-leave-directory-regexp-alist))
	  (file-regexp-alist (if (not jde-xemacsp) compilation-file-regexp-alist))
	  (nomessage-regexp-alist (if (not jde-xemacsp) compilation-nomessage-regexp-alist))
	  (parser compilation-parse-errors-function)
	  (error-message "No further errors")
	  (thisdir default-directory))

      (oset this :buffer buf)

      (set-buffer buf)

      ;; Make sure a style checker process is not
      ;; already running.
      (let ((check-proc (get-buffer-process (current-buffer))))
	(if check-proc
	    (if (or (not (eq (process-status check-proc) 'run))
		    (yes-or-no-p
			 "A check style process is running; kill it?"))
		(condition-case ()
		    (progn
		      (interrupt-process check-proc)
		      (sit-for 1)
		      (delete-process check-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
			 (buffer-name)))))

      ;; In case the checker buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)

      ;; Clear out the compilation buffer and make it writable.
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))

      (compilation-mode "Check Style")

      (set (make-local-variable 'compilation-parse-errors-function) parser)
      (set (make-local-variable 'compilation-error-message) error-message)
      (set (make-local-variable 'compilation-error-regexp-alist)
	     error-regexp-alist)
      (if (not jde-xemacsp)
	  (progn
	    (set (make-local-variable 'compilation-enter-directory-regexp-alist)
		 enter-regexp-alist)
	    (set (make-local-variable 'compilation-leave-directory-regexp-alist)
		 leave-regexp-alist)
	    (set (make-local-variable 'compilation-file-regexp-alist)
		 file-regexp-alist)
	    (set (make-local-variable 'compilation-nomessage-regexp-alist)
	      nomessage-regexp-alist)))
      (setq default-directory thisdir
	    compilation-directory-stack (list default-directory)))))

(defmethod jde-checkstyle-exec ((this jde-checkstyle-checker))

  (jde-checkstyle-create-checker-buffer this)

  ;; Pop to checker buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if (not jde-xemacsp)
      (if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))

  (let* ((outbuf (oref this :buffer))
	 (vm-path (oref (jde-run-get-vm) :path))
	 (source-file (file-name-nondirectory buffer-file-name))
	 (jde-java-directory
	  (concat
	   (jde-find-jde-data-directory)
	   "java/"))
	 (args (append
		(jde-checkstyle-get-options)
		(oref this :interactive-args)
		(list "-classpath"
		      (or jde-checkstyle-classpath
			  (expand-file-name "lib/checkstyle-all.jar"
					    jde-java-directory)))
		(list jde-checkstyle-class)
		(if jde-checkstyle-option-config-file
		    (list "-c"
			  jde-checkstyle-option-config-file))
		(list source-file))))

    (save-excursion
      (set-buffer outbuf)

      (insert (format "cd %s\n" default-directory))

      (insert (concat
	       vm-path
	       " "
               (mapconcat 'identity args " ")
	       "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (proc (apply 'start-process
			  (downcase mode-name)
			  outbuf
			  vm-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress)))

      (set-buffer-modified-p nil)
      (setq compilation-last-buffer (oref this :buffer)))))



;;;###autoload
(defun jde-checkstyle ()
  "Checks the Java program in the current buffer.
This command invokes the style checker specified by `jde-checkstyle-class'
with the options specified by the JDEE customization variables
that begin with `jde-checkstyle'. If the variable
`jde-read-checkstyle-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)

  (if jde-read-checkstyle-args
      (setq jde-interactive-checkstyle-args
            (read-from-minibuffer
             "Check args: "
             jde-interactive-checkstyle-args
             nil nil
             '(jde-interactive-checkstyle-arg-history . 1))))

  (let ((checker (jde-checkstyle-checker
		  "checker"
		  :interactive-args (if jde-read-checkstyle-args
					jde-interactive-checkstyle-args))))

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-checkstyle from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
             (not jde-xemacsp))
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-checkstyle
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (jde-checkstyle-exec checker)))

(provide 'jde-checkstyle)

;; $Log: jde-checkstyle.el,v $
;; Revision 1.2  2002/09/10 04:48:56  paulk
;; Updated CheckStyle URL.
;;
;; Revision 1.1  2002/09/06 12:53:48  paulk
;; Initial revision to style checker interface.
;;

;; Version: 1.3
;; - Updated to support latest version of checkstyle (2.3), adding
;; support for all the command-line properties as customizable variables. A
;; total of 43 properties are now supported, from 19 in the previous jde-checkstyle
;; version.
;;
;; Version: 1.2
;; - adopted to checkstyle version 1.3
;; - compatible with recent versions of jde
;;   patch by Nascif Abousalh-Neto
;;
;; Version: 1.1
;; - minor bug fix to be compatible with recent versions of jde
;;
;; Version: 1.0

;;; JDE-CHECKSTYLE.EL ends here
