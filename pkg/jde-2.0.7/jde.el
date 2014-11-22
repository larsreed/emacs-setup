;;; jde.el -- Integrated Development Environment for Java.
;; $Revision: 1.63 $ $Date: 1998/07/10 00:49:24 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998 Paul Kinnucan.

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

;;;###autoload
(defconst jde-version "2.0.7"
  "JDE version number.")

;;; Commentary:

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.


;;; Code:

(defvar jde-xemacsp (string-match "XEmacs" emacs-version)
  "Non-nil if we are running in the XEmacs environment.")

(defvar jde-xemacs20p (and jde-xemacsp (>= emacs-major-version 20)))

(require 'easymenu)
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(c-initialize-cc-mode)
(require 'jde-db)
(require 'jde-run)
(require 'jde-make)
(require 'jde-gen)
(require 'compile)
(require 'imenu)
(require 'speedbar)
(require 'browse-url)

;; This is copied straight out of andersl-java-font-lock.el
;; Necessary to set here because andersl assumes that the
;; buffer is in java-mode (it is actually in jde-mode).
(defun setup-fontlock()
  (if (not (assq 'jde-mode font-lock-defaults-alist))
      (setq font-lock-defaults-alist
	    (cons
	     (cons 'jde-mode

		   ;; jde-mode-defaults
		   '((java-font-lock-keywords java-font-lock-keywords-1
		      java-font-lock-keywords-2 java-font-lock-keywords-3)
		     nil nil ((?_ . "w") (?$ . "w")) nil
		     (font-lock-mark-block-function . mark-defun)))

	     font-lock-defaults-alist))))

(cond ((not jde-xemacsp)
       (if (< emacs-major-version 20)
	   (require 'aljfl)) [[LRE]]
       (setup-fontlock)))

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defgroup jde nil
  "Java Development Environment"
  :group 'tools
  :prefix "jde-")

(defgroup jde-project nil
  "JDE Project Options"
  :group 'jde
  :prefix "jde-")

(defcustom jde-jdk-doc-url "http://www.javasoft.com/products/jdk/1.1/docs/index.html"
  "*URL of JDK documentation. 
This can point to a remote or local copy of the documentation. By
default, this variable points to the copy stored at JavaSoft's
website."
  :group 'jde-project
  :type 'string)

(defcustom jde-global-classpath nil
  "*Specify class paths for compile, run, and debug commands.
Use this option to specify the paths to the root directories of the
classes used by your project. The JDE uses the specified paths to
construct a classpath argument for the Java compile, run, and debug
commands. The JDE also allows you to set classpaths individually for
the compile, run, and debug commands. If you do not set a classpath
for those commands, the JDE uses the global classpath. If you do not
specify a global or a local classpath, the compile/run/debug commands
uses the value of the CLASSPATH environment variable, if set, as the
classpath."
  :group 'jde-project
  :type '(repeat (string :tag "Path")))

(defcustom jde-quote-classpath t
  "*Quote the classpath argument.
Set this option on when using the bash shell with Windows 95 or NT.
The semicolons in the classpath confuse the shell."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-project-name ""
"*Specifies name of project to which the current buffer belongs."
  :group 'jde-project
  :type 'string)

(defcustom jde-project-file-name "prj.el"
  "*Specify name of JDE project file.
When it loads a Java source file, the JDE looks for a lisp file of
this name (the default is prj.el in the source file hierarchy. If it
finds such a file, it loads the file. You can use this file to set the
classpath, compile options, and other JDE options on a
project-by-project basis."
  :group 'jde-project
  :type 'string)

(defcustom jde-use-font-lock t
  "*Turn on font-locking if on.
	Set to nil to disable the use of font-locking."
  :group 'jde-project
  :type 'boolean)


(defcustom jde-compiler "javac"
  "*Java compiler.
Specifies the path to the compiler to be used to compile the source
in the current buffer. The default is the JDK compiler (javac)."
  :group 'jde-project
  :type 'string)

(defcustom jde-read-compile-args nil
"*Specify whether to prompt for additional compiler arguments.
If this variable is non-nil, the jde-compile command prompts
you to enter additional compiler arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments 
entered in the minibuffer."
  :group 'jde-project
  :type 'boolean
)

(defvar jde-interactive-compile-args ""
"String of compiler arguments entered in the minibuffer.")

(defvar jde-interactive-compile-arg-history nil
"History of compiler arguments entered in the minibuffer.")


(defcustom jde-entering-java-buffer-hooks nil
"*Lists functions to run when entering a Java source buffer"
  :group 'jde-project
  :type 'hook)

(defcustom jde-build-use-make nil
"*If true, use make to build JDE projects."
  :group 'jde-project
  :type 'boolean)


(defgroup jde-compile-options nil
  "JDE Compiler Options"
  :group 'jde
  :prefix "jde-compile-option-")

(defcustom jde-compile-option-command-line-args ""
  "*Specify options as a string of command-line arguments.
The value of this variable should be a string of switches understood
by the compiler, for example, -depend -g. This variable is intended to
be used to set compile options not otherwise defined by the JDE, in
particular, options not defined by javac but used by another compiler
that you might want to use with the JDE."
  :group 'jde-compile-options
  :type 'string)

(defcustom jde-compile-option-classpath nil
"*Specify paths of classes required to compile this project.
The JDE uses the specified paths to construct a -classpath
argument to pass to the compiler. This option overrides the
`jde-global-classpath' option."
  :group 'jde-compile-options
  :type '(repeat (string :tag "Path")))


(defcustom jde-compile-option-directory ""
  "*Specifies the root directory of the class file hierarchy.
The compiler places compiled classes in the specified
directory. For example, specifying the class
directory as: 
  
  C:\\users\\dac\\classes

causes the class files for the classes in the MyProgram.java source
file to be saved in the directory C:\\users\\dac\\classes. If your class 
is in the package demos\\awt, the class files would be placed in directory
C:\\users\\dac\\classes\\demos\\awt."
  :group 'jde-compile-options
  :type 'string)

(defcustom jde-compile-option-deprecation nil
  "*Warn use or override of a deprecated member or class. 
A member or class is deprecated if its documentation comment contains
the @deprecated tag. The compiler will emit a warning at the end of
compilation whether or not the deprecation option is on; this option
causes the location of each individual use or override to be noted.

Deprecated members or classes are deliberately not mentioned if the
source file containing the deprecation is being recompiled.  This can
happen because the file is on the command line or because the depend
option is on and the source file is out of date.
"
  :group 'jde-compile-options
  :type 'boolean)


(defcustom jde-compile-option-debug nil
"*Generate information about local variables for debug tools.
By default, only line number information is generated.

Prior to JDK 1.2, the the debug and optimize options were
mutually exclusive. In JDK 1.2, it is possible to combine debug and
optimize, but the shortcuts taken by optimized code may occasionally
produce surprising debugging results. For example, declared variables
may not exist and code may appear to move or not be executed at all."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-nodebug nil
"*Do not generate line number or local variable debugging information."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-optimize nil
"*Directs the compiler to try to generate faster code. 
This may slow down compilation, make larger class files, and/or make
it difficult to debug.

Prior to 1.2, the optimize option tried to inline methods across
classes. This created compatibility problems and sometimes generated
illegal bytecode. The optimize option also implicitly turned on the
depend option and implicitly turned off the debug option.

In JDK 1.2, the optimize option no longer inlines across classes and
so may safely be used for any java compilation. Optimize no longer
implicitly turns on depend or implicitly turns off debug."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-optimize-interclass nil
"*Enable interclass optimizations.
This option informs the compiler that all generated class files are
guaranteed to be delivered and upgraded as a unit, enabling interclass
optimizations that may otherwise break binary compatibility. Use this
option with discretion.

The Java Language Specification section 13.4.21 describes situations
in which it is legal to use a java compiler to inline methods. The
compiler will only optimize code for which source is available during
the compilation, so the only .java files discoverable by the compiler
should be for classes intended to be delivered or upgraded as a
unit. In particular, ensure that no sources for other classes are
accessible on CLASSPATH, keeping in mind that the present working
directory, `.', is appended to CLASSPATH.

To ensure that a product is able to run on 1.2 as well as future
binary-compatible java virtual machines, one must ensure that any
sources for JDK 1.2 classes are never available along CLASSPATH while
using the interclass optimization option.

Line number debugging information in class files compiled with the
interclass optimization option may refer to lines of a different
source file. This is a limitation of the class file format, which does
not allow the originating file to be encoded along with the line
number.

This option implicitly turns on the dependency option."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-depend nil
"*Analyze dependencies.
Causes recompilation of class files on which the source files given as
command line arguments recursively depend. Without this option, only
files that are directly depended on and missing or out-of-date will be
recompiled. Recompilation does not extend to missing or out-of-date
files only depended on by already up-to-date class files."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-vm-args nil
"*Specify command-line arguments for Java interpreter.
Passes the specified arguments to the Java interpreter that runs the
compiler. The argument should not contain spaces. This is useful for
adjusting the compiler's execution environment or memory usage."
  :group 'jde-compile-options
  :type '(repeat (string :tag "Option")))

(defcustom jde-compile-option-verbose nil
"*Print verbose messages.
Causes the compiler and linker to print out messages about what source
files are being compiled and what class files are being loaded."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-nowarn nil
"*Turn off warnings.
If this option is specified, the compiler does not print out any
warnings."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-encoding nil
"*Specify the source file encoding name, such as EUCJIS\\SJIS.
If this option is not specified, then the platform default converter
is used."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-enable-abbrev-mode nil
"*Enable expansion of abbreviations in jde-mode.
See `jde-mode-abbreviations' for more information."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-mode-abbreviations
  (list 
   (cons "ab" "abstract")
   (cons "bo" "boolean")
   (cons "br" "break")
   (cons "by" "byte")
   (cons "byv" "byvalue")
   (cons "cas" "cast")
   (cons "ca" "catch")
   (cons "ch" "char")
   (cons "cl" "class")
   (cons "co" "const")
   (cons "con" "continue")
   (cons "de" "default")
   (cons "dou" "double")
   (cons "el" "else")
   (cons "ex" "extends")
   (cons "fa" "false")
   (cons "fi" "final")
   (cons "fin" "finally")
   (cons "fl" "float")
   (cons "fo" "for")
   (cons "fu" "future")
   (cons "ge" "generic")
   (cons "go" "goto")
   (cons "impl" "implements")
   (cons "impo" "import")
   (cons "ins" "instanceof")
   (cons "in" "int")
   (cons "inte" "interface")
   (cons "lo" "long")
   (cons "na" "native")
   (cons "ne" "new")
   (cons "nu" "null")
   (cons "pa" "package")
   (cons "pri" "private")
   (cons "pro" "protected")
   (cons "pu" "public")
   (cons "re" "return")
   (cons "sh" "short")
   (cons "st" "static")
   (cons "su" "super")
   (cons "sw" "switch")
   (cons "sy" "synchronized")
   (cons "th" "this")
   (cons "thr" "throw")
   (cons "throw" "throws")
   (cons "tra" "transient")
   (cons "tr" "true")
   (cons "vo" "void")
   (cons "vol" "volatile")
   (cons "wh" "while")
   )
"*Abbreviations used for Java keywords.
To use these abbreviations, you must enable abbrev-mode (see
`jde-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space."
   :group 'jde-project
  :type '(repeat 
	  (cons :tag "jde-mode abbreviation"
		(string :tag "Abbreviation")
		(string :tag "Expansion"))))


;;;###autoload
(defun jde-set-compiler (compiler)
  "Specify the pathname of the compiler to be used to compile the
current buffer. Default is javac."
  (interactive
   "sEnter compiler (javac): ")
   (if (string= compiler "")
       (setq jde-compiler "javac")
     (setq jde-compiler compiler)))

(defun jde-path-string-to-list (paths)
 "Converts a string of paths to a list of paths.
It is assumed that the default path separator for the
current platform (e.g., semicolon on Win32) separates
the paths."
 (let ((path-list (list))
       (m 0)
       (n (string-match path-separator paths)))
   (while n
     (let ((path (substring paths m n)))
       (if path
	   (setq path-list
		 (cons path path-list)))
       (setq m (+ n 1))
       (setq n (string-match path-separator paths m))))
   (setq n (length paths))
   (if (and (> n 0) (< m n))
       (let ((path (substring paths m n)))
	 (if path
	     (setq path-list
		   (cons path path-list)))))
   (setq path-list (nreverse path-list))))

;;;###autoload
(defun jde-set-global-classpath (classpath)
  "Specify the value of the -classpath argument for the Java compiler and
interpreter."
  (interactive 
   "sEnter classpath: ")
  (setq jde-global-classpath (jde-path-string-to-list classpath)))

(defun jde-build-classpath-arg (path-list quote)
"Build a classpath from a list of paths."
  (let ((classpath "")
	(len (length path-list))
	(n 0))
    (while (< n len)
      (setq classpath (concat classpath
			      (if (> n 0)
				  path-separator)
			      (elt path-list n)))
      (setq n (1+ n)))
    (if quote
	(setq classpath (concat "\"" classpath "\"")))
    (setq classpath (concat "-classpath " classpath))))

(defun jde-build-compile-vm-args ()
  (let ((args " ")
	(len (length jde-compile-option-vm-args))
	(n 0))
    (while (< n len)
      (setq args (concat " -J"
			 (elt jde-compile-option-vm-args n)))
      (setq n (1+ n)))
    args))

;;;###autoload
(defun jde-browse-jdk-doc ()
  "Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation."
  (interactive)
  (browse-url jde-jdk-doc-url browse-url-new-window-p))

(defun jde-make-compile-command (more-args)
  "Constructs the java compile command as: jde-compiler + options + buffer file name."
  (concat jde-compiler " " 
	  (jde-get-compile-options) 
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "
	  (file-name-nondirectory buffer-file-name)))

(defun jde-get-compile-options ()
"Constructs a command-line argument string for compiler.
The string consists of the contents of the jde-compile-options
variable concatenated with the various jde-compile-option
settings."
  (let (options)

    (if jde-compile-option-classpath
	(setq options 
	      (jde-build-classpath-arg
	       jde-compile-option-classpath jde-quote-classpath))
      (if jde-global-classpath
	  (setq options
		(jde-build-classpath-arg
		 jde-global-classpath jde-quote-classpath))))
	     
    (if jde-compile-option-debug
	(setq options (concat options " -g")))

    (if (not (string= jde-compile-option-directory ""))
	(setq options
	      (concat options 
		" -d "
		jde-compile-option-directory)))

    (if jde-compile-option-deprecation
	(setq options (concat options " -deprecation")))

    (if jde-compile-option-nodebug
	(setq options (concat options " -g:nodebug")))

    (if jde-compile-option-optimize
	(setq options (concat options " -O")))

    (if jde-compile-option-optimize-interclass
	(setq options (concat options " O:interclass")))

    (if jde-compile-option-depend
	(setq options (concat options " -depend")))

    (if jde-compile-option-vm-args
	(setq options 
	      (concat options (jde-build-compile-vm-args))))

    (if jde-compile-option-verbose
	(setq options (concat options " -verbose")))

    (if jde-compile-option-nowarn
	(setq options (concat options " -nowarn")))

    (if (not (string= jde-compile-option-command-line-args ""))
	(setq options (concat options " " 
			      jde-compile-option-command-line-args)))

    options))

;;;###autoload
(defun jde-set-compile-options (options)
  "Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose."
  (interactive
   "sEnter options: ")
  (setq jde-compile-option-command-line-args options))

(defun jde-show-compile-options ()
  "Show the JDE Compile Options panel."
  (interactive)
  (customize-apropos "jde-compile-options" 'groups))

(defun jde-show-run-options ()
  "Show the JDE Run Options panel."
  (interactive)
  (customize-apropos "jde-run-options" 'groups))

(defun jde-show-debug-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-db-options" 'groups))

(defun jde-show-project-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-project" 'groups))

(defun jde-show-autocode-options ()
  "Show the JDE Autocode panel."
  (interactive)
  (customize-apropos "jde-gen" 'groups))

;;;###autoload
(defun jde-compile ()
  "Compile the Java program in the current buffer.
This command invokes the compiler specified by `jde-compiler'
with the options specified by the JDE customization variables
that begin with `jde-compile'. If the variable
`jde-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)

  (if jde-read-compile-args
      (setq jde-interactive-compile-args
	      (read-from-minibuffer 
	       "Compile args: "
	       jde-interactive-compile-args
	       nil nil
	       '(jde-interactive-compile-arg-history . 1))))

  (let ((compile-command
	 (jde-make-compile-command 
	  jde-interactive-compile-args)))
	  

  

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-compile from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (eq system-type 'windows-nt)
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-compile
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (compile-internal compile-command "No more errors")))

;;;###autoload
(defun jde-java-build ()
  "Use javac -depend to build the application whose main class is
specified by `jde-run-application-class'."
  (interactive)
  (cond 
   ((string= jde-run-application-class "")
    (message "No application main class specified."))
   (t
    (string-match "\\(\\(\\w*\\.\\)*\\)\\(\\w*\\b\\)"
		jde-run-application-class)
    (let* ((b1 (match-beginning 1))
	   (e1 (match-end 1))
	   (b2 (match-beginning 3))
	   (e2 (match-end 3))
	   (file (concat
		  (substring jde-run-application-class b2 e2)
		  ".java"))
	   (package (if e1
			(substring jde-run-application-class b1 e1)))
	   (directory (jde-db-search-src-dirs file package)))
      (cond
       (directory
	(let ((file-path 
	       (concat directory 
		       file))
	      (save-depend jde-compile-option-depend))
	  (find-file file-path)
	  (setq jde-compile-option-depend t)
	  (jde-compile)
	  (setq jde-compile-option save-depend)))
       (t
	(message (concat "Could not find source for "
			 jde-run-application-class))))))))
    
;;;###autoload
(defun jde-build ()
  "Rebuild the entire project.
This command has two operating modes: java and make. In java mode,
this command uses javac's built-in make facility to rebuild a
project. In make mode, this command uses a user-specified make program
to rebuild the project. JDE configuration variables control which mode
is used.  In particular, if the variable `jde-build-use-make' is
non-nil, this command invokes the make program specified by the
variable `jde-make-program'. If the variable `jde-make-args' is a
non-empty string, this function uses its contents to invoke make;
otherwise, it prompts you to enter command-line arguments for make. If
`jde-build-use-make' is nil, this function invokes javac on the source
file specified by `jde-run-app-class', with the -depend option. This
causes javac to recompile all missing or out-of-date files required
to run the application's main class."
  (interactive)
  (if jde-build-use-make
      (jde-make
       (if (string= jde-make-args "")
	   (read-from-minibuffer (concat jde-make-program " ")
				 (nth 0 minibuffer-history))
	 jde-make-args))
    (jde-java-build)))

;; This is actually a no-op to get jde auto-loaded.
;;;###autoload
(defun jde-mode ()
  "Major mode for developing Java applications and applets."
  nil)

(define-derived-mode 
  jde-mode java-mode "JDE"
  "Major mode for developing Java applications and applets.
  \\{jde-mode-map}"

  ;; Define buffer-local variables.
  (make-local-variable 'jde-project-name)


  ;; Enable support for automatic project switching.
  ;; This feature loads the appropriate project settings whenever
  ;; a user switches from a Java buffer belonging to one project
  ;; to a buffer belonging to another.
  (make-local-hook 'post-command-hook)
  (unless (find 'jde-detect-java-buffer-activation post-command-hook)
    (add-hook 'post-command-hook 'jde-detect-java-buffer-activation))

  (if jde-xemacsp
      (jde-insert-menu-in-XEmacs-menubar))

  (if jde-use-font-lock
      (jde-setup-syntax-coloring))

  (setq imenu-create-index-function 'jde-create-imenu-index)

  ;; Load the project file for this buffer. The project file
  ;; defines JDE options for a project.
  (jde-load-project-file)

  (setq jde-current-project jde-project-name)


  ;; Define abbreviations.
  (mapc (lambda (x) 
	  (define-mode-abbrev (car x) (cdr x)))
	jde-mode-abbreviations)

  (if jde-enable-abbrev-mode
      (abbrev-mode 1))

  )

(defun jde-setup-syntax-coloring() 
  ;; Set up syntax coloring.
  (cond (window-system

	 ;; If not XEmacs 20.1 turn on font lock.
	 ;; (XEmacs 21 has font-lock on by default.)
	 (if (or
	      (not jde-xemacsp)
	      (not
	       (and
		(eq emacs-major-version 21)
		(eq emacs-minor-version 0))))
	     (turn-on-font-lock))

	 (setq font-lock-maximum-decoration t)

	 (if (not jde-xemacsp)
	     (global-font-lock-mode 1))
	 )))

;; Setup jde-mode for font locking.
(if jde-xemacsp
    (put 'jde-mode 'font-lock-defaults
	 '((java-font-lock-keywords
	    java-font-lock-keywords-1 java-font-lock-keywords-2)
	   nil nil ((?_ . "w")) beginning-of-defun)))

;; Make jde-mode the default mode for Java source code buffers.
;; Prepend the jde-mode entry so that it shadows the java-mode
;; entry already in the list.
;;;###autoload
(or (rassq 'jde-mode auto-mode-alist)  ; [[[LRE]]]
    (setq auto-mode-alist
	  (append
	   '(("\\.java\\'" . jde-mode))
	   auto-mode-alist)))

(defvar jde-menu 
  (list "JDE"
	["Compile"           jde-compile t]
	["Run App"           jde-run t]
	["Debug App"         jde-db t]
	["Run Applet"        jde-run-menu-run-applet t]
	["Build"             jde-build t]
        ["-"                 ignore nil]
	(list "Generate"
	      ["Get/Set Pair..."  jde-gen-get-set t]
	      (list "Listener"
		    ["Action"          jde-gen-action-listener t]
		    ["Window"          jde-gen-window-listener t]
		    ["Mouse"           jde-gen-mouse-listener t]
		    )
	      ["Other..."        jde-gen-code t]
	      )
	["Speedbar"          speedbar-frame-mode t]
	["Browse JDK Doc"    jde-browse-jdk-doc t]
	(list "Options"
	      ["Compile"         jde-show-compile-options t]
	      ["Run"             jde-show-run-options t]
	      ["Debug"           jde-show-debug-options t]
	      ["Project"         jde-show-project-options t]
	      ["Autocode"        jde-show-autocode-options t]
	      ["-"                 ignore nil]
	      ["Save Project"    jde-save-project t]
	      )
	(list "JDE New" ; [[LRE]]
	      ["Class..."         jde-gen-class-buffer t]
	      ["Console..."       jde-gen-console-buffer t]
	      ["Custom..."        jde-gen-buffer t]
	      )
	(list "Help"
	      ["Contents"        jde-show-help t]
	      ["-"                 ignore nil]
	      (concat "JDE " jde-version)
	 )
	)
  "Menu for JDE.")

;; Define JDE menu for FSF Emacs.
(if (not jde-xemacsp)
    (easy-menu-do-define 'jde-menu 
			 jde-mode-map
			 "Menu for JDE."
			 jde-menu)
;;
)



(defun jde-insert-menu-in-XEmacs-menubar ()
  "Insert JDE menu in the XEmacs menu bar."
  (if (and 
       (not (featurep 'infodock))
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil jde-menu)
	(add-menu nil "JDE" (cdr jde-menu)))))


;; [[LRE]] (defvar jde-new-buffer-menu
;; [[LRE]]   (list
;; [[LRE]]    "JDE New"
;; [[LRE]]    ["Class..."         jde-gen-class-buffer t]
;; [[LRE]]    ["Console..."       jde-gen-console-buffer t]
;; [[LRE]]    ["Other..."         jde-gen-buffer t]
;; [[LRE]]    )
;; [[LRE]]   "Menu for creating new Java buffers.")
;; [[LRE]] 
;; [[LRE]] ;; Add JDE New menu to Emacs Files menu.
;; [[LRE]] (if (not jde-xemacsp)
;; [[LRE]]     (let* ((mb (assq 'menu-bar global-map))
;; [[LRE]] 	      (files (assq 'files mb))
;; [[LRE]] 	      (menu (if (fboundp 'easy-menu-create-menu)
;; [[LRE]] 			(easy-menu-create-menu 
;; [[LRE]] 			 (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))
;; [[LRE]] 		      (easy-menu-create-keymaps 
;; [[LRE]] 		       (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))))     
;; [[LRE]] 	      (menu-name (car jde-new-buffer-menu)))
;; [[LRE]] 	 (define-key-after (cdr (cdr files)) [jde-new]
;; [[LRE]] 	   (cons menu-name menu)
;; [[LRE]] 	   'open-file))
;; [[LRE]]   (unless (featurep 'infodock)
;; [[LRE]]     (add-submenu '("File") jde-new-buffer-menu "Insert File...")))



;; Define jde-mode accelerator keys
(define-key jde-mode-map "\C-c\C-v\C-c" 'jde-compile)
(define-key jde-mode-map "\C-c\C-v\C-r" 'jde-run)
(define-key jde-mode-map "\C-c\C-v\C-d" 'jde-db)
(define-key jde-mode-map "\C-c\C-v\C-b" 'jde-build)
(define-key jde-mode-map "\C-c\C-v\C-s" 'speedbar-frame-mode)
(define-key jde-mode-map "\C-c\C-v\C-a" 'jde-run-menu-run-applet)
(define-key jde-mode-map "\C-c\C-v\C-n" 'jde-browse-jdk-doc)
(define-key jde-mode-map "\C-c\C-v\C-u" 'jde-update-buffer)
(define-key jde-mode-map "\C-c\C-v\C-p" 'jde-save-project)

;; Project File Functions

(defun jde-root-dir-p (dir)
  (let ((parent (concat dir "../")))
    (if (eq system-type 'windows-nt)
	(not (file-exists-p parent))
      (and 
       (string= (file-truename dir) "/")
       (string= (file-truename parent) "/")))))

(defun jde-find-project-file (dir)
  "Finds the project file for the Java source file in the current
buffer. Returns nil if it cannot find a project file in the
source file directory or an ascendant directory."
  (let ((file (find jde-project-file-name
		    (directory-files dir) :test 'string=)))
    (if file
	(concat dir file)
      (if (not (jde-root-dir-p dir))
	  (jde-find-project-file (concat dir "../"))))))

(defun jde-load-project-file ()
  "Loads the project file for the Java source file in the current
directory. Searches for the project file first in the source directory,
then in ascendant directories. Uses the first file that it encounters."
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(load-file prj-file))))

;;;###autoload
(defun jde-open-project-file ()
  "Opens the project file for the Java source file in the
current buffer."
  (interactive)
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(find-file prj-file)
      (message "%s" "Project file not found."))))


(defun jde-save-delete (symbol)
  "Delete the call to SYMBOL from project file.
Leave point at the location of the call, or after the last expression."
  (save-excursion
    (let ((project-file (or
			 (jde-find-project-file default-directory)
			 (concat "./" jde-project-file-name))))
      (set-buffer (find-file-noselect project-file)))

    (goto-char (point-min))
    (catch 'found
      (while t
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region (save-excursion
			     (backward-sexp)
			     (point))
			   (point))
	    (throw 'found nil)))))
    (unless (bolp)
      (princ "\n"))))

(defun jde-save-variables ()
  "Save all JDE variables in project file."
  (jde-save-delete 'jde-set-variables)
  (let ((standard-output (get-buffer jde-project-file-name)))
    (unless (bolp)
      (princ "\n"))
    (princ "(jde-set-variables ")
    (mapatoms
     (lambda (symbol)
       (when 
	   (and (string-match "jde-" (symbol-name symbol))
		(get symbol 'custom-type))
	 (let ((value (symbol-value symbol)))
	   (when value
	     (princ "\n '(")
	     (princ symbol)
	     (princ " ")
	     (prin1 (custom-quote value))
	     ;; Check whether the user has changed the value of this
	     ;; variable in a customization buffer. If so, save flag
	     ;; so that custom knows that this value differs from
             ;; standard value.
	     (if (get symbol 'customized-value)
		 (princ " t)")
	       (princ ")"))		 
	     )))))
      (princ ")")
      (save-excursion
	(set-buffer (get-buffer jde-project-file-name))
	(unless (looking-at "\n")
	  (princ "\n"))
	(save-buffer))))

(defun jde-set-variables (&rest args)
  "Initialize JDE customization variables.  

Takes a variable number of arguments. Each argument 
should be of the form:

  (SYMBOL VALUE)

The value of SYMBOL is set to VALUE.
"
  (while args 
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (nth 0 entry))
		 (value (nth 1 entry))
		 (customized (nth 2 entry))
		 (set (or (get symbol 'custom-set) 'set-default)))
	    (if customized
		(put symbol 'customized-value (list value)))
	    (when (default-boundp symbol)
		   ;; Something already set this, overwrite it
		   (funcall set symbol (car value)))
	    (setq args (cdr args)))))))

;;;###autoload
(defun jde-save-project (proj-name)
  "Saves local source file buffer options in project file.
This command provides an easy way to create and update a
project file for a Java project. Simply open a source file,
set the desired options, using the JDE Options menu, then
save the settings in the project file, using this command.
Now, whenever you open a source file from the same directory
tree, the saved settings will be restored for that file."
  (interactive
   (list 
    (let (prompt)
      (if (string= jde-project-name "")
	  (setq prompt "Enter project name: ")
	(setq prompt
	      (format "Enter project name (%s): " 
		      jde-project-name)))
      (read-string prompt))))
  (unless (string= proj-name "")
      (setq jde-project-name proj-name))
  (jde-save-variables))

(defun jde-convert-prj-file (file) 
"Converts a pre-JDE-2.0.7 project file to JDE-2.0.7 format.
Note: old project files did not preserve information about 
whether a saved value differed from the standard (JDE-defined)
value of a variable. Thus, all values are saved in the
converted file as though they were standard values. This means
that when JDE reloads the file, a custom buffer will customized
values as though they were standard. If you want to restore
a customized value to a standard value, simply make some
innocuous edit to the customized value and choose 
'Set for current session' from the customization buffer's
Set menu. Custom will then enable the Set menu option that
allows you to restore the value to its default value."
  (interactive "F")
  (let ((olddef (symbol-function 'jde-set-variables))
	(newdef 
	 (lambda (&rest args)
	   (while args 
	     (let ((entry (car args)))
	       (if (listp entry)
		   (let* ((symbol (nth 0 entry))
			  (value (nth 1 entry))
			  (set (or (get symbol 'custom-set) 'set-default)))
		     (when (default-boundp symbol)
		       ;; Something already set this, overwrite it
		       (funcall set symbol value))
		     (setq args (cdr args)))))))))
    (defalias 'jde-set-variables newdef)
    (require 'cus-edit)
    (load-file file)
    (jde-save-project jde-project-name)
    (defalias 'jde-set-variables olddef)))

;; Code to update JDE customization variables when a user switches
;; from a Java source buffer belonging to one project to a buffer
;; belonging to another.

(setq jde-current-project "")

(defun jde-reload-project-file ()
"Reloads the project file for a newly activated Java buffer when
the new buffer's project differs from the old buffer's."
  (if (not (string= jde-current-project jde-project-name))
      (progn
	(setq jde-current-project jde-project-name)
	(jde-load-project-file))))

(add-hook 'jde-entering-java-buffer-hooks 'jde-reload-project-file)

(setq jde-current-buffer (current-buffer))

(defun jde-detect-java-buffer-activation ()
"Detects when a user activates a buffer.
If the activated buffer is a Java buffer, runs the 
`jde-entering-java-buffer' hooks."
  (let ((curr-buff (current-buffer)))
    (if (not
	 (equal curr-buff jde-current-buffer))
	(progn
	  (setq jde-current-buffer curr-buff)
	  (if (eq major-mode 'jde-mode)
		(run-hooks 'jde-entering-java-buffer-hooks))))))


(defun jde-count-open-java-buffers ()
  "Returns non-nil if any java buffers are open."
  (count 
   ".java"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (string-match file-type file-name))))))
	 

(defun jde-remove-jde-hook ()
  "Removes `jde-detect-java-buffer-activation-hook' when
all Java source buffers have been closed."
  (unless (> (jde-count-open-java-buffers) 1)
  (remove-hook 'post-command-hook 'jde-detect-java-buffer-activation)))

(add-hook 'kill-buffer-hook 'jde-remove-jde-hook)


;; JDE help

(defun jde-find-jde-doc-directory ()
  "Return the path of the JDE documentation directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the JDE. Versions of XEmacs
that include JDE will store the JDE doc in a data
directory called jde. On all other Emacs versions, the JDE
expects to find the documentation in a subdirectory 
named doc of the directory that contains the file
jde.el."
  (let ((dir (if jde-xemacsp
		 (locate-data-directory "jde"))))
    (if dir
	dir
      (file-name-directory (locate-library "jde")))))
 
;;;###autoload
(defun jde-show-help ()
  "Displays the JDE User's Guide in a browser."
  (interactive)
  (let* ((jde-dir (jde-find-jde-doc-directory))
	 (jde-help
	  (if jde-dir
	      (if (and jde-xemacsp
		       (locate-data-directory "jde"))
		  (expand-file-name "jde.htm" jde-dir)
		(expand-file-name "doc/jde.htm" jde-dir)))))	  
    (if jde-help
	(if (eq system-type 'windows-nt)
	    (browse-url jde-help browse-url-new-window-p)
	  (browse-url (concat "file:" jde-help)  browse-url-new-window-p))
      (signal 'error '("Cannot find JDE help file.")))))


;; speedbar

(defun jde-make-imenu-patterns ()
  "Makes a replacement for the regular expression indexing  
patterns in imenu, which are too slow for the JDE's
speedbar. See `imenu-generic-expression'."
  (let* ((capital "A-Z\300-\326\330-\337")
	 (letter "a-zA-Z_$\300-\326\330-\366\370-\377")
	 (digit "0-9")
	 (white-space "\\s-")
	 (optional-white-spaces
	  (concat white-space "*"))
	 (bol "^")
	 (eol "$")
	 (not-comment (concat optional-white-spaces "[^.*/]*"))
	 (anything ".*")

	 (primitive-type 
	  (concat "\\<\\(b\\(oolean\\|yte\\)"
		  "\\|char\\|double\\|float\\|int"
		  "\\|long\\|short\\|void\\)\\>"))
	 (primitive-type-count 2)

	 (primitive-type-no-void 
	  (concat "\\<\\(b\\(oolean\\|yte\\)"
		  "\\|char\\|double\\|float\\|int"
		  "\\|long\\|short\\)\\>"))
	 (primitive-type-no-void-count 2)

	 (identifier
	  (concat "\\<\\([" letter "][" letter digit "]*\\)\\>"))
	 (identifier-count 1)

	 ;; Class types are assumed to begin with a capital letter.
	 (class-type
	  (concat
	   "\\<\\([" capital "][a-zA-Z_" digit "]*\\)\\>"))
	 (class-type-count 1)

	 (modifier
	  (concat 
	   "\\<\\(abstract\\|const\\|final\\|native\\|"
	   "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|"
	   "s\\(tatic\\|ynchronized\\)\\|transient\\|volatile\\)\\>"))
	 (modifier-count 4)

	 (optional-modifiers
	  (concat
	   "\\(" modifier optional-white-spaces "\\)*"))
	 (optional-modifiers-count 5)

	 (modifiers
	  (concat
	   "\\(" modifier optional-white-spaces "\\)+"))
	 (modifiers-count 5)

	 (optional-array-modifier
	  (concat
	   "\\(\\[" optional-white-spaces "\\]" optional-white-spaces "\\)*"))
	 (optional-array-modifier-count 1)

	 (class
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   "\\<class\\>"
	   optional-white-spaces
	   identifier))

	 (class-count (+ optional-modifiers-count
			  identifier-count))
	   
	 (interface
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   "\\<interface\\>"
	   optional-white-spaces
	   identifier))

	 (interface-count (+ optional-modifiers-count
			      identifier-count))

	 (constructor
	  (concat
	   bol
	   optional-white-spaces
	   modifiers                 ;; e.g., public
	   class-type                ;; e.g., Foo
	   optional-white-spaces
	   "("))

	 (constructor-count (+ optional-modifiers-count
			       class-type-count))

	 ;; Pattern for methods that return a primitive type
	 (method1
	  (concat
	   bol
	   not-comment
	   primitive-type          ;; e.g., int
	   optional-white-spaces
	   optional-array-modifier ;; e.g., []
	   identifier              ;; e.g., foo
	   optional-white-spaces
	   "("))

	 (method1-count (+ primitive-type-count
			   optional-array-modifier-count
			   identifier-count))
	
	 ;; Pattern for methods that return a class type
	 (method2
	  (concat
	   bol
	   not-comment
	   class-type
	   optional-white-spaces
	   optional-array-modifier
	   identifier
	   optional-white-spaces
	   "("))

	 (method2-count (+ class-type-count
			   optional-array-modifier-count
			   identifier-count))

	 (variable1
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   optional-white-spaces
	   class-type
	   optional-white-spaces
	   optional-array-modifier
	   optional-white-spaces
	   identifier
	   optional-white-spaces
	   "\\(;\\|=\\)"))

	 (variable1-count (+ optional-modifiers-count
			     class-type-count
			     optional-array-modifier-count
			     identifier-count))

	 (variable2
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   optional-white-spaces
	   primitive-type-no-void
	   optional-white-spaces
	   optional-array-modifier
	   optional-white-spaces
	   identifier
	   optional-white-spaces
	   "\\(;\\|=\\)"))

	 (variable2-count (+ optional-modifiers-count
			     primitive-type-no-void-count
			     optional-array-modifier-count
			     identifier-count))

	 (exp 
	  (`
	   (
	    (nil ;; methods index
	     (,  method1) (, method1-count))
	    (nil ;; methods index
	     (,  method2) (, method2-count))
	    ("Constructors" ;; constructors index
	     (,  constructor) (, constructor-count))
	    ("Variables"
	     (, variable1) (, variable1-count))
	    ("Variables"
	     (, variable2) (, variable2-count))
	    ("Classes"
	     (, class) (, class-count))
	    ("Interfaces"
	     (, interface) (, interface-count))
	    )
	   ))
	 )
    exp))


;;;
;;; Java index gathering function.
;;;

(defun jde-create-imenu-index ()
;; Based on imenu--generic-function
  "Return an index of the current buffer as an alist.

PATTERN is an alist with elements that look like this: (MENU-TITLE
REGEXP INDEX).

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

For emacs-lisp-mode for example PATTERN would look like:

'((nil \"^\\\\s-*(def\\\\(un\\\\|subst\\\\|macro\\\\|advice\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2)
  (\"*Vars*\" \"^\\\\s-*(def\\\\(var\\\\|const\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2)
  (\"*Types*\" \"^\\\\s-*(def\\\\(type\\\\|struct\\\\|class\\\\|ine-condition\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2))'

Returns an index of the current buffer as an alist.  The elements in
the alist look like: (INDEX-NAME . INDEX-POSITION).  They may also be
nested index lists like (INDEX-NAME . INDEX-ALIST) depending on
pattern.

\(imenu--generic-function PATTERN\)."

  (let* ((patterns (jde-make-imenu-patterns))
	 (case-fold-search nil)
	 (index-alist (list 'dummy))
	 (found nil)
	 (global-regexp 
	  (concat "\\(" 
		  (mapconcat
		   (function (lambda (pattern) (identity (cadr pattern)))) 
		   patterns "\\)\\|\\(") 
		  "\\)"))
	 prev-pos
	 (ppos (point-max)))

    (goto-char (point-max))

    (imenu-progress-message prev-pos 0 t)
    (save-match-data
      (while (re-search-backward global-regexp nil t)
	(imenu-progress-message prev-pos nil t)
        (setq found nil)
	(save-excursion
	  (goto-char (match-beginning 0))
	  (mapcar 
	   (function 
	    (lambda (pat) 
	      (let ((menu-title (car pat))
		    (regexp (cadr pat))
		    (index (caddr pat)))
		    (if (and (not found) ; Only allow one entry;
			     (re-search-forward regexp ppos t))
			(let ((beg (match-beginning index))
			      (end (match-end index)))
			  (setq found t)
			  (setq ppos beg)
			  (push 
			   (cons (buffer-substring-no-properties beg end) beg)
			   (cdr 
			    (or (assoc menu-title index-alist)
				(car (push 
				      (cons menu-title '()) 
				      index-alist))))))))))
	   patterns))))
    (imenu-progress-message prev-pos 100 t)
    (let ((main-element (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist)) main-element))))

(provide 'jde)

;; Change History
;;
;; $Log: jde.el $
;; Revision 1.63  1998/07/10 00:49:24  paulk
;; Changed jde-save-variables to mark variables that have been customized
;; in the current session. Changed jde-set-variables to store the value
;; of a customized variable in the customized-value property of the
;; variable. This enables Custom to recognize the variable as customized.
;;
;; Fixed a bug in the function that finds the JDE documentation.
;;
;; Revision 1.62  1998/07/09 04:33:57  paulk
;; Change the way that the JDE saves and restores project-specific values of
;; customization variables to be compatible with custom. This fixes the bug
;; that caused errors when loading customized JDE variables from a .emacs file.
;;
;; Revision 1.61  1998/07/04 05:25:15  paulk
;; Should have been does not turn on font-lock if XEmacs 21.0.
;;
;; Revision 1.60  1998/07/04 00:53:58  paulk
;; Now does not turn on font-lock if XEmacs 20.1.
;;
;; Revision 1.59  1998/07/02 05:33:13  paulk
;; Fixed bugs in the jde-show-help function that prevented display
;; of help on XEmacs and NT/Emacs.
;;
;; Revision 1.58  1998/06/30 03:35:10  paulk
;; Added the customization variable `jde-read-compile-args'. If non-nil,
;; this variable causes the jde-compile command to read compilation options
;; from the minibuffer and append them to the options specified by
;; the `jde-compile-option group of customization' variables. The JDE
;; maintains a history of compiler options entered in the minibuffer.
;;
;; Revision 1.57  1998/06/29 03:18:11  paulk
;; Use fboundp instead of Emacs version to determine whether
;; easy-menu-create-menu is bound.
;;
;; Revision 1.56  1998/06/27 03:43:10  paulk
;; Updated release to 2.0.3
;;
;; Revision 1.55  1998/06/27 03:40:20  paulk
;; Fixed bug where the JDE was invoking global-font-lock-mode on XEmacs,
;; where it is not defined.
;;
;; Updated JDE to call easy-menu-create-menu instead of easy-menu-create-keymaps
;; on Emacs 20. (The former replaces the latter as of Emacs 20.x);
;;
;; Revision 1.54  1998/06/21 05:23:56  paulk
;; Updated JDE version number to 2.0.2.
;;
;; Revision 1.53  1998/06/21 05:22:59  paulk
;; Changed buffer change code to reload a project file
;; when a user changed jde-mode buffers, not just .java
;; buffers. This allows using extensions other than .java
;; for Java source code files.
;;
;; Made post-command-hook buffer local to improve performance.
;;
;; Thanks to David J. Biesack( sasdjb@unx.sas.com) for the above changes.
;;
;; Revision 1.52  1998/06/18 18:14:08  paulk
;; Added XEmacs compatibility changes requested by xemacs.org.
;;
;; Revision 1.51  1998/06/17 03:49:58  paulk
;; Added support for abbreviations.
;;
;; Revision 1.50  1998/05/29 01:42:08  paulk
;; Added no-op function for jde-mode to facilitate autoloading.
;; Thanks to Andy Piper <andyp@parallax.co.uk> for the suggestion.
;;
;; Revision 1.49  1998/05/27 06:33:29  paulk
;; Updated JDE version number to 2.01.
;;
;; Revision 1.48  1998/05/27 05:49:12  paulk
;; Added autoload comments for JDE functions.
;;
;; Revision 1.47  1998/05/17 06:21:58  paulk
;; Changed names of the Files->JDE New->Custom and JDE-Autocode->Custom
;; to Other...
;;
;; Revision 1.46  1998/04/19 13:07:19  kinnucan
;; Updated version number.
;;
;; Revision 1.45  1998/04/19 13:05:56  kinnucan
;; Updated version number.
;;
;; Revision 1.44  1998/04/18 14:06:43  kinnucan
;; Replace imenu--generic-function with jde-create-imenu-index
;; as the indexing function for Java source buffers.
;; jde-create-imenu-index is basically imenu--generic-function
;; with a fix for a bug that generates multiple indexes for
;; the same symbol.
;;
;; Revision 1.43  1998/04/09 04:52:47  kinnucan
;; * Added menu items for inserting custom code templates in buffers.
;;   The items are:
;;
;;   Files->JDE New->Custom
;;
;;   JDE->Generate->Custom
;;
;; Revision 1.42  1998/04/08 04:40:01  kinnucan
;; * Fixed jde-save-variables and jde-set-variables so that they
;;   operate more like the corresponding custom functions. This
;;   was necessary to support project-by-project customization
;;   of autocode templates.
;;
;; Revision 1.41  1998/04/06 05:57:25  kinnucan
;; * Removed extraneous New option from JDE menu.
;;
;; Revision 1.40  1998/04/06 03:44:36  kinnucan
;; * Added JDE New submenu to the Emacs Files menu. The new submenu has
;;   commands for creating buffers with skeleton code for the following
;;   types of classes:
;;
;;   - Generic class
;;   - Main class for a console application
;;
;; Revision 1.39  1998/04/01 05:32:55  kinnucan
;; * Added code generation for
;;
;;   - new Java source file
;;   - Get/set variable method pair
;;   - Action listener
;;   - Window listener
;;   - Mouse listener
;;
;; Revision 1.38  1998/03/30 22:20:24  kinnucan
;; * Fixed separator code in JDE menu definition.
;;
;;   Thanks to Kai Grossjohann <grossjohann@ls6.cs.uni-dortmund.de>
;;   for providing this fix.
;;
;; Revision 1.37  1998/03/27 04:46:19  kinnucan
;; Added the jde-build command.
;;
;; Revision 1.36  1998/03/23 06:44:23  kinnucan
;; * Set up to activate project tracking when the first Java buffer
;;   is loaded and deactivate project tracking when the last
;;   Java buffer is closed.
;;
;; * Removed update buffer command as it is no longer necessary
;;   because all customization variables are now global.
;;
;; * Changed save project command to prompt for a project name,
;;   which is required for automatic project tracking.
;;
;; Revision 1.35  1998/03/22 07:21:07  kinnucan
;; * Changed the way the JDE maintains project settings. Previously
;;   most JDE customization variables were buffer local. This was basically
;;   a way of letting Emacs manage project-dependent customization
;;   settings. However, this approach conflicts with the new (as of
;;   Emacs 20) Emacs customization feature. To avoid the conflict,
;;   the JDE now manages the task of keeping buffers up-to-date.
;;   In particular, all variables are global to permit easy
;;   customization. Whenever a user switches from one Java buffer to
;;   another, the JDE checks to see if the "to" buffer is part of the
;;   same project as the "from" buffer. If not the JDE loads the
;;   project file for the "to" buffer, thus updating the customization
;;   variables to the specific JDE settings for the "to" buffer's project.
;;
;; * Fixed bug that prevented jde-compile-option-command-line-args
;;   from working correctly.
;;
;; Revision 1.34  1998/03/19 20:55:59  kinnucan
;; - Fixed bug that prevented JDE->Options->Debug from working.
;;
;; - Updated version number.
;;
;; Revision 1.33  1998/03/05 07:49:40  kinnucan
;; Made jde-db-source-directories non-global again to
;; eliminate the problem of project files changing
;; its value.
;;
;; Revision 1.32  1998/03/05 07:14:36  kinnucan
;; Updated version number to 1.9.5
;;
;; Revision 1.31  1998/03/03 23:10:43  kinnucan
;; - Fixed bug in imenu regexp for speedbar that falsely taggex
;;   method-like constructs in comments
;;
;; - Added file: prefix to path to User's Guide.
;;
;; - Fixed bug that caused setting jde-compile-option-vm-args to wipe
;;   out all other compile options.
;;
;; Revision 1.30  1998/02/25 17:11:47  paulk
;; Added jde-show-help command. This command displays the
;; JDE User's Guide in a browser.
;;
;; Revision 1.29  1998/02/23 23:35:34  kinnucan
;; * Reorganized JDE menu. Eliminated the Compile Options item
;;   and added the following items:
;;
;;   JDE->Options->Compile       Shows Compile Options buffer
;;   JDE->Options->Run           Shows Run Options Buffer
;;   JDE->Options->Debug         Shows Debug Options Buffer
;;   JDE->Options->Project       Show Project Options Buffer
;;   JDE->Options->Update Buffer Updates buffer to global options values
;;
;; * Added the jde-save-project command.
;;
;;   This command saves the values of all local JDE options (i.e.,
;;   customization) variables in the project file. This provides
;;   an easy way of creating a project file for a project.
;;   Simply set the desired options, using the JDE Options menu.
;;   Then, save the results in the project file.
;;
;; Revision 1.28  1998/02/18 03:14:04  kinnucan
;; Corrected some doc strings.
;;
;; Revision 1.27  1998/02/18 02:33:07  kinnucan
;; * Added customization support by redefining all customization
;;   variables, using defcustom.
;;
;; * Defined two customization groups: jde and jde-compile-options.
;;
;; * Replaced the jde-classpath variable with jde-global-classpath.
;;
;; * Added customization option jde-quote-classpath.
;;
;; * Added variable jde-project-name.
;;
;; * Replace variable jde-compile-options with
;;   jde-compile-option-command-line-args.
;;
;; * Added the following compile option variables:
;;
;;   jde-compile-option-classpath
;;   jde-compile-option-directory
;;   jde-compile-option-deprecation
;;   jde-compile-option-debug
;;   jde-compile-option-nodebug
;;   jde-compile-option-optimize
;;   jde-compile-option-optimize-interclass
;;   jde-compile-option-option-depend
;;   jde-compile-option-vm-args
;;   jde-compile-option-verbose
;;   jde-compile-option-nowarn
;;   jde-compile-option-encoding
;;
;;   All of these variables are made buffer local.
;;
;; * Replaced jde-set-classpath function with
;;   jde-set-global-classpaht function.
;;
;; * Added the following functions
;;
;;   jde-path-string-to-list
;;   jde-build-classpath-arg
;;   jde-build-compile-vm-args
;;   jde-get-compile-options
;;
;; Revision 1.26  1998/02/13 10:23:52  kinnucan
;; Fixed so that the JDE menu appears in the XEmacs menu bar
;; only when a Java buffer is active.
;;
;; Revision 1.25  1998/02/13 09:36:59  kinnucan
;; Added jde-use-font-lock variable. If t (the default), jde turns on
;; font-locking for java files.
;;
;; Revision 1.24  1998/02/12 21:28:47  kinnucan
;; Advised imenu-default-create-index-function to set case-fold-search
;; to nil (case-sensitive) when creating indexes.
;;
;; Revision 1.23  1998/02/12 06:34:37  kinnucan
;; Fixed some bugs in imenu regular expressions, including lack of a re
;; for indexing primitive type variables. Thanks to
;; David J. Biesack <sasdjb@unx.sas.com> for spotting some bugs.
;;
;; Revision 1.22  1998/02/12 05:46:45  kinnucan
;; Added fix to bug that prevented fontlocking on Emacs 20.2
;;
;; Revision 1.21  1998/01/29 11:24:43  paulk
;; Fixed typo.
;;
;; Revision 1.20  1998/01/29 11:23:10  paulk
;; Made various changes to ensure compatibility with XEmacs.
;;
;; Revision 1.19  1998/01/20 13:35:51  paulk
;; Use browse-url instead of browse-url-of-file.
;;
;; Revision 1.18  1998/01/20 12:45:40  paulk
;; Require cc-mode.
;;
;; Revision 1.17  1998/01/20 05:19:26  kinnucan
;; Added code to set up andersl font locking. Necessary because
;; andersl assumes that the buffer is in java-mode.
;;
;; Revision 1.16  1998/01/19 05:13:54  kinnucan
;; * Made JDE into a major mode (jde-mode) derived from java-mode.
;; * The JDE now uses the browse-url package to display JDK documentation.
;; * Deleted the variable jde-hook (it is replaced by jde-mode-hook).
;; * Deleted the variables jde-web-browser and jde-doc-dir as they duplicate
;;   functionality provided by browse-url.
;;
;; Revision 1.15  1998/01/19 01:34:04  kinnucan
;; *** empty log message ***
;;
;; Revision 1.14  1997/10/30 05:38:00  kinnucan
;; 1) Made configuration variables settable.
;; 2) Made jde-db-source-directories buffer local.
;;
;; Revision 1.13  1997/10/20 05:21:20  kinnucan
;; Now requires andersl-java-font-lock only for Emacs versions < 20.
;;
;; Revision 1.12  1997/10/18 05:24:52  kinnucan
;; 1. Changed key bindings to use the two prefix keys C-c C-v.
;;
;; 2. Fixed infinite recursion bug in jde-find-project-file.
;;
;; Revision 1.11  1997/10/07 03:44:24  kinnucan
;; Required cl.
;;
;; Revision 1.10  1997/10/06 13:17:25  kinnucan
;; Removed last usage of obsolete bashify function.
;;
;; Revision 1.9  1997/10/06 04:02:27  kinnucan
;; Added jde-compiler variable and associated set command. Lets you
;; configure the JDE to use the compiler of your choice on a buffer
;; by buffer basis.
;;
;; Revision 1.8  1997/10/04 10:13:10  kinnucan
;; Added key bindings for menu commands.
;;
;; Revision 1.7  1997/10/03 05:57:54  kinnucan
;; 1. Revamped imenu regular expressions.
;; 2. Stopped quoting compile command arguments for bash under Win32.
;;
;; Revision 1.6  1997/10/01 03:13:04  kinnucan
;; Changed name of JDE menu from "Java" to "JDE" to avoid conflict
;; with cc-mode 5.18 menu, which is named "Java".
;;
;; Revision 1.5  1997/09/04 03:40:01  kinnucan
;; Updated version number.
;;
;; Revision 1.4  1997/09/04 03:38:13  kinnucan
;; 1. Made jde configuration variables buffer local to support automatic
;;    loading of project files.
;;
;; 2. Added Run Applet command to the jde menu.
;;
;; Revision 1.3  1997/08/28 02:54:17  kinnucan
;; Eliminated single quotes around path in jde-browse-jdk-doc.
;;
;; Revision 1.2  1997/08/26 08:50:29  kinnucan
;; Added jde-set-classpath command, which lets you set the classpath for
;; compiling and running applications.
;;
;; Revision 1.1  1997/06/18 17:25:57  paulk
;; Initial revision
;;
;; Revision 1.8  1997/06/18 17:20:00  paulk
;; Initial checkin.
;;


;;; jde.el ends here.







