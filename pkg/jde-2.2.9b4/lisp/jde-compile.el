;;; jde-compile.el -- Integrated Development Environment for Java.
;; $Revision: 1.26 $ $Date: 2001/10/19 10:10:59 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001 Paul Kinnucan.

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

;;; Commentary:

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

(require 'eieio)
(require 'cl)

;; Define XEmacs- and Emacs-only variables 
;; and functions to avoid compiler warnings.
(eval-when (compile)
  (if jde-xemacsp
      (progn
	(defvar compilation-enter-directory-regexp-alist)
	(defvar compilation-leave-directory-regexp-alist)
	(defvar compilation-file-regexp-alist)
	(defvar compilation-nomessage-regexp-alist)
	(defvar compilation-scroll-output)
	(defvar compilation-process-setup-function)
	(defvar last-nonmenu-event))))


;; (makunbound 'jde-compiler)
(defcustom jde-compiler '("javac server" "")
  "Specify the type, and if necessary, the location of the compiler to
be used to compile source files for the current project. The JDE
supports three compilers: javac server, javac executable, and
jikes. The javac server runs the com.sun.tools.javac package included
with the JDK in the Beanshell. The javac executable shipped with the
JDK also uses this package. The advantage of the javac server is that
it avoids the vm startup time that accounts for most of the
compilation time consumed by the javac executable. The javac server
uses the version of com.sun.tools.javac included in the JDK for the
current project. See `jde-jdk' for more information. If you want to
use the javac executable to compile your project's source files,
select \"javac\" as the compiler type and, optionally, specify
the path to the executable in the \"Path\" field. If you do
not specify a path, the JDE uses the javac executable included in the
JDK for the current project. Similarly, to use jikes, select \"jikes\"
and optionally specify the path of the jikes executable."
  :group 'jde-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Compiler type"
	   (item "javac server")
	   (item "javac")
	   (item "jikes"))
	  (file 
	   :tag "Path")))
	   
	   
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

(defcustom jde-compile-finish-hook 
  '(jde-compile-finish-refresh-speedbar jde-compile-finish-flush-completion-cache)
  "List of functions to be invoked when compilation of a 
Java source file terminates. Each function should accept
two arguments: the compilation buffer and a string 
describing how the compilation finished."
  :group 'jde
  :type 'hook)


(defun jde-compile-finish-flush-completion-cache (buf msg) 
  "Flush the classinfo cache at the end of compilation.
Flush the entire cache as we don't know which classes
were recompiled."
  ;;Setting the last java buffer as the current buffer
  (condition-case nil
      (progn
	(if jde-xemacsp
	    (set-buffer (cadr (buffer-list)))
	  (set-buffer (car (buffer-list))))
	(if (eq major-mode 'jde-mode)
	    (progn
	      (setq jde-complete-last-compiled-class (jde-complete-get-name-of-this-class))
	      (jde-complete-flush-classes-in-cache (list jde-complete-last-compiled-class))
	      (setq jde-complete-last-compiled-class nil)
	      (message "Flushed completion cache."))))
    (error nil)))

(defun jde-compile-finish-refresh-speedbar (buf msg) 
  "Refresh speedbar at the end of a compilation."
  (if (and (frame-live-p speedbar-frame)
	    (frame-visible-p speedbar-frame))
       (speedbar-refresh)))


(defgroup jde-compile-options nil
  "JDE Compiler Options"
  :group 'jde
  :prefix "jde-compile-option-")

(defcustom jde-compile-option-command-line-args nil
  "*Specify options as a string of command-line arguments.
The value of this variable should be a list of switches understood
by the compiler, for example, -depend -g. This variable is intended to
be used to set compile options not otherwise defined by the JDE, in
particular, options not defined by javac but used by another compiler
that you might want to use with the JDE."
  :group 'jde-compile-options
  :type '(repeat (string :tag "Argument:")))

(defcustom jde-compile-option-classpath nil
"*Specify paths of classes required to compile this project.
The JDE uses the specified paths to construct a -classpath
argument to pass to the compiler. This option overrides the
`jde-global-classpath' option."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-sourcepath nil
"*Specify the source code path to search for class or interface definitions.

As with the user class path, source path entries  can be directories, JAR 
archives, or ZIP archives. If packages are used, the local path name within 
the directory or archive must reflect the package name. 

Note that classes found through the classpath are subject to automatic 
recompilation if their sources are found."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

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
  :type 'directory)

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


(defcustom jde-compile-option-debug 
  (list "selected" (list t nil nil))
  "*Include debug information in classes.
The compiler includes line number information by default.

Before JDK 1.2, the the debug and optimize options were
mutually exclusive. In JDK 1.2, it is possible to combine debug and
optimize, but the shortcuts taken by optimized code may occasionally
produce surprising debugging results. For example, declared variables
may not exist and code may appear to move or not be executed at all.

The JDK 1.1.x versions of javac do not support inclusion of selected
debug information."
  :group 'jde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Debug info to include in class:"
	   (const "all")
	   (const "none")
	   (const "selected"))
	  (list
	   :tag "    info"
	   :indent 4
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Line Numbers")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Variables")
	   (checkbox :format "%[%v%] %t \n"
		     :tag "Source")))
	   
)


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


(defcustom jde-compile-option-depend nil
"*Analyze dependencies.
Causes recompilation of class files on which the source files given as
command line arguments recursively depend. Without this option, only
files that are directly depended on and missing or out-of-date will be
recompiled. Recompilation does not extend to missing or out-of-date
files only depended on by already up-to-date class files.

Note: if you are using a compiler other than post JDK 1.1.6 versions
of javac, you may need to specify the command-line switch used by
the compiler to specify dependency checking. See 
`jde-compile-option-depend-switch' for more information."
  :group 'jde-compile-options
  :type 'boolean)

(defcustom jde-compile-option-depend-switch (list "-Xdepend")
"*Specify command line switch for depend option.
This option is necessary because the command-line switch for
dependency checking differs among Java compilers. Choose
from the following options:

  -Xdepend  Full dependency checking (post JDK 1.1.6)
  -depend   Full dependency checking (jikes and pre-JDK 1.1.6)
  +F        Check everything except jar and zip files (jikes only)
  +U        Check everything including jar and zip files (jikes only)"
  :group 'jde-compile-options
  :type '(list 
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Select -Xdepend (javac) or -depend (jikes):"
	   (const "-Xdepend")
	   (const "-depend")
	   (const "+F")
	   (const "+U"))))

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

(defcustom jde-compile-option-encoding ""
"*Specify the source file encoding name, such as EUCJIS\\SJIS.
If this option is not specified, then the platform default converter
is used."
  :group 'jde-compile-options
  :type 'string)

;;(makunbound 'jde-compile-option-target)
(defcustom jde-compile-option-target (list "1.1")
"*Generate class files that will work on VMs with the specified version.
 
The default is to generate class files to be compatible with both
1.1 and 1.2 VMs. The versions supported by javac in JDK1.2 are: 

  1.1     Ensure that generated class files will be compatible 
          with 1.1 and 1.2 VMs. This is the default.
  
  1.2     Generate class files that will run on 1.2 VMs, but 
          not on 1.1 VMs.

  1.3     Generate class files that will run on VMs in the 
          Java 2 SDK, v 1.3 and later, but will not run 
          on 1.1 or 1.2 VMs

By default, classes are compiled against the bootstrap and extension classes
of the JDK that javac shipped with. But javac also supports cross-compiling, 
where classes are compiled against a bootstrap and extension classes of a 
different Java platform implementation. It is important to use 
`jde-compile-option-bootclasspath' and `jde-compile-option-extdirs' when 
cross-compiling."
  :group 'jde-compile-options
  :type '(list
	  (radio-button-choice 
	   :format "%t \n%v"
	   :tag "Target VM:"
	   (const "1.1")
	   (const "1.2")
	   (const "1.3"))))

(defcustom jde-compile-option-bootclasspath nil
"*Cross-compile against the specified set of boot classes.
As with the user class path, boot class path entries can be 
directories, JAR archives, or ZIP archives."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-compile-option-extdirs nil
"*Cross-compile against the specified extension directories. 
Each JAR archive in the specified directories is searched for class files."
  :group 'jde-compile-options
  :type '(repeat (file :tag "Path")))

;;(makunbound 'jde-compile-option-verbose-path)
(defcustom jde-compile-option-verbose-path nil
"*Describe how paths and standard extensions were searched to find
source and class files.

   ***NOTE***

   This option is supported only by the versions of javac shipped
   with JDK 1.1.x and 1.2.x and oldjavac in JDK 1.3."

  :group 'jde-compile-options
  :type 'boolean)

(defun jde-compile-show-options-buffer ()
  "Show the JDE Compile Options panel."
  (interactive)
  (customize-apropos "jde-compile-options" 'groups))


(defclass jde-compile-compiler ()
  ((name             :initarg :name
		     :type string
		     :documentation
		     "Name of compiler")
   (version          :initarg :version
		     :type string
		     :documentation
		     "Compiler version.")
   (path             :initarg :path
		     :type string
		     :documentation
		     "Path of the compiler executable.")
   (buffer           :initarg :buffer
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
		     "Arguments entered in the minibuffer.")
   (use-server-p     :initarg :use-server-p
		     :type boolean
		     :documentation
		     "Run as a compile server in the Beanshell."))
  "Class of Java compilers.")

(defmethod jde-compile-classpath-arg ((this jde-compile-compiler))
  "Returns the classpath argument for this compiler."
  (let ((classpath
	 (if jde-compile-option-classpath
	     jde-compile-option-classpath
	   jde-global-classpath))
	(symbol
	 (if jde-compile-option-classpath
	     'jde-compile-option-classpath
	   'jde-global-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jde-build-classpath
	  classpath symbol)))))

(defmethod jde-compile-sourcepath-arg ((this jde-compile-compiler))
  "Get the source path argument for this compiler."
    (if jde-compile-option-sourcepath
	(list
	 "-sourcepath"
	 (jde-build-classpath
	  jde-compile-option-sourcepath
	  'jde-compile-option-sourcepath))))

(defmethod jde-compile-bootclasspath-arg ((this jde-compile-compiler))
  "Get the boot classpath argument for this compiler."
  (if jde-compile-option-bootclasspath
      (list
       "-bootclasspath"
       (jde-build-classpath jde-compile-option-bootclasspath 
			    'jde-compile-option-bootclasspath))))

(defmethod jde-compile-extdirs-arg ((this jde-compile-compiler))
  "Get the extdirs argument for this compiler."
  (if jde-compile-option-extdirs
      (list
       "-extdirs"
       (jde-build-classpath 
	jde-compile-option-extdirs
	'jde-compile-option-extdirs))))


(defmethod jde-compile-encoding-arg ((this jde-compile-compiler))
  (if (not (string= jde-compile-option-encoding ""))
      (list jde-compile-option-encoding)))

(defmethod jde-compile-debug-arg ((this jde-compile-compiler))
  "Get the debug arg for this compiler."
  (let* ((include-option (nth 0 jde-compile-option-debug))
	 (selected (nth 1 jde-compile-option-debug))
	 (lines (nth 0 selected))
	 (vars (nth 1 selected))
	 (src (nth 2 selected)))
    (cond
     ((and
       (string= include-option "selected")
       lines
       (not vars)
       (not src))
      nil)
     ((string= include-option "all")
      (list "-g"))
     ((string= include-option "none")
      (list "-g:none"))
     ((and
       (string= include-option "selected")
       (or lines vars src))
      (list 
       (concat 
	"-g:"
	(if lines
	    (if (or vars src) "lines,"
	      "lines"))
	(if vars
	    (if vars
		(if src "vars," "vars")))
	(if src "source")))))))

(defmethod jde-compile-output-dir-arg ((this jde-compile-compiler))
  "Get the ouput directory arg for this compiler."
    (if (not (string= jde-compile-option-directory ""))
	(list
	 "-d"
	 (jde-normalize-path 'jde-compile-option-directory))))

(defmethod jde-compile-deprecation-arg ((this jde-compile-compiler))
  "Get deprecation argument for this compiler."
    (if jde-compile-option-deprecation
	(list "-deprecation")))

(defmethod jde-compile-optimize-arg ((this jde-compile-compiler))
  "Get optimization argument for this compiler."
    (if jde-compile-option-optimize
	(list "-O")))

(defmethod jde-compile-depend-arg ((this jde-compile-compiler))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list (car jde-compile-option-depend-switch))))

(defmethod jde-compile-vm-args ((this jde-compile-compiler))
  "Get arguments to pass to the vm used to run this compiler."
    (if jde-compile-option-vm-args
	(mapcan
	 (lambda (arg)
	   (list "-J" arg))
	 jde-compile-option-vm-args)))

(defmethod jde-compile-verbose-arg ((this jde-compile-compiler))
  "Get verbosity level argument for this compiler."
    (if jde-compile-option-verbose
	(list "-verbose")))

(defmethod jde-compile-verbose-path-arg ((this jde-compile-compiler))
  "Get verbose path argument for this compiler."
    (if jde-compile-option-verbose-path
	(list "-Xverbosepath")))

(defmethod jde-compile-nowarn-arg ((this jde-compile-compiler))
  "Get no warning argument for this compiler."
    (if jde-compile-option-nowarn
	(list "-nowarn")))

(defmethod jde-compile-command-line-args ((this jde-compile-compiler))
  "Get additional command line arguments for this compiler."
	jde-compile-option-command-line-args)

(defmethod jde-compile-target-arg ((this jde-compile-compiler))
  "Get compiler target argument for this compiler."
    (let ((target (car jde-compile-option-target)))
      (if (not (string= target "1.1"))
	  (list "-target" target))))

(defmethod jde-compile-get-args ((this jde-compile-compiler))
  (append
   (jde-compile-classpath-arg this)
   (jde-compile-sourcepath-arg this)
   (jde-compile-bootclasspath-arg this)
   (jde-compile-extdirs-arg this)
   (jde-compile-encoding-arg this)
   (jde-compile-debug-arg this)
   (jde-compile-output-dir-arg this)
   (jde-compile-deprecation-arg this)
   (jde-compile-optimize-arg this)
   (jde-compile-depend-arg this)
   (jde-compile-vm-args this)
   (jde-compile-verbose-arg this)
   (jde-compile-verbose-path-arg this)
   (jde-compile-nowarn-arg this)
   (jde-compile-target-arg this)
   (jde-compile-command-line-args this)))

(defmethod jde-compile-create-compilation-buffer ((this jde-compile-compiler))
  (save-excursion
    (let ((buf (get-buffer-create "*compilation*"))
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

      ;; Make sure a compiler process is not
      ;; already running.
      (if (not (oref this :use-server-p))
	  (let ((comp-proc (get-buffer-process (current-buffer))))
	    (if comp-proc
		(if (or (not (eq (process-status comp-proc) 'run))
			(yes-or-no-p
			 "A compilation process is running; kill it?"))
		    (condition-case ()
			(progn
			  (interrupt-process comp-proc)
			  (sit-for 1)
			  (delete-process comp-proc))
		      (error nil))
		  (error "Cannot have two processes in `%s' at once"
			 (buffer-name))))))

      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)

      ;; Clear out the compilation buffer and make it writable.
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))

      (compilation-mode "Compilation")

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
;       (set (make-local-variable 'compilation-arguments)
; 	   (list output error-message))
      (setq default-directory thisdir
	    compilation-directory-stack (list default-directory))
	)))


(defmethod jde-compile-run-exec ((this jde-compile-compiler))
  (let* ((outbuf (oref this :buffer))
	 (compiler-path (oref this :path))
	 (source-file (file-name-nondirectory buffer-file-name))
	 (args (append
		(jde-compile-get-args this)
		(oref this :interactive-args)
		(list source-file))))

    (save-excursion
      (set-buffer outbuf)

      (insert (format "cd %s\n" default-directory))
      (insert (concat
	       compiler-path
	       " "
	       (mapconcat (lambda (x) x) args " ")
	       "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))	   
	     (proc (apply 'start-process 
			  (downcase mode-name)
			  outbuf
			  compiler-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress))))))

(defmethod jde-compile-run-server ((this jde-compile-compiler))
  (let* ((directory-sep-char ?/)
	 (args
	  (mapconcat
	   (lambda (arg) arg)
	   (append
	    (jde-compile-get-args this)
	    (oref this :interactive-args))
	   " "))
	 (source-path 
	  ;; This is to hack to get around 
	  ;; XEmacs defaulting to backslash
	  ;; as the directory sep character.
	  (expand-file-name buffer-file-name)))
    (while (string-match "\"" args)
      (setq args (replace-match "" nil nil args)))
     
    (let* ((output
	    (jde-jeval 
	     (format "jde.util.CompileServer.compile(\"%s %s\");"
		     source-path
		     args)))

	   (len (length output))
	   (status (substring output (- len 2) (- len 1))))

      (setq output (substring output 0 (- len 2)))

      (save-excursion
	(set-buffer (oref this :buffer))
	(insert "CompileServer output:\n")
	(insert args " " source-path "\n")
	(insert "\n" output "\n")

	(compilation-handle-exit 
	 'exit status
	 (if (string= "0" status)
	     "finished\n"
	   (format "exited abnormally with code %s\n"
		   status)))))))


(defmethod jde-compile-launch ((this jde-compile-compiler))

  (if (oref this :use-server-p)
      (jde-compile-run-server this)
    (jde-compile-run-exec this))

  (set-buffer-modified-p nil))

(defmethod jde-compile-scroll-output ((this jde-compile-compiler))
  (let ((outwin (oref this :window)))
    (if compilation-scroll-output
      (save-selected-window
            (select-window outwin)
            (goto-char (point-max))
            (let* ((result (count-lines-page));;Getting the number of lines
                   (lines (substring result (+ (string-match "+" result) 1)
                                     (string-match ")" result)))
                   ;;number to offsett the scroll up.(formatting reasons)
                   (offset (if compilation-window-height
                               (abs (- compilation-window-height 5))
                             (/ (window-height) 2))))
              (scroll-up (- (string-to-number lines);;reducing the number of 
                            offset))))))) ;;lines to scroll

(defmethod jde-compile-compile ((this jde-compile-compiler))

  (jde-compile-create-compilation-buffer this)

  ;; Pop to compilation buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (or (eq outwin (selected-window))
	(set-window-point outwin (point-min)))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if (not jde-xemacsp)
      (if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))     
    
  (jde-compile-launch this)

  (if (not jde-xemacsp)
   (jde-compile-scroll-output this))

  (setq compilation-last-buffer (oref this :buffer)))
  

(defclass jde-compile-javac (jde-compile-compiler)
  ()
  "Class of javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler name.
  (oset this name "javac")  

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.1 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-11 (jde-compile-compiler)
  ()
  "Class of JDK 1.1 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-11) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.1"))

(defmethod jde-compile-debug-arg ((this jde-compile-javac-11))
  "Get the debug arg for this compiler."
   (let ((include-option (nth 0 jde-compile-option-debug)))
     (cond
      ((string= include-option "all")
       (list "-g"))
      ((string= include-option "selected")
       (error "JDK 1.1 version of javac does not support selected debug info.")))))

(defmethod jde-compile-depend-arg ((this jde-compile-javac-11))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list "-depend")))

(defmethod jde-compile-get-args ((this jde-compile-javac-11))
  (append
   (jde-compile-classpath-arg this)
   (jde-compile-encoding-arg this)
   (jde-compile-debug-arg this)
   (jde-compile-output-dir-arg this)
   (jde-compile-deprecation-arg this)
   (jde-compile-optimize-arg this)
   (jde-compile-depend-arg this)
   (jde-compile-vm-args this)
   (jde-compile-verbose-arg this)
   (jde-compile-nowarn-arg this)
   (jde-compile-command-line-args this)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.2 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-12 (jde-compile-compiler)
  ()
  "Class of JDK 1.2 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-12) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.2"))

(defmethod jde-compile-depend-arg ((this jde-compile-javac-12))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list "-Xdepend")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.3 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-13 (jde-compile-javac-12)
  ()
  "Class of JDK 1.3 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-13) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.3"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.4 Compiler                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-javac-14 (jde-compile-javac-13)
  ()
  "Class of JDK 1.4 javac compilers.")

(defmethod initialize-instance ((this jde-compile-javac-14) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler version.
  (oset this version "1.4"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Jikes Compiler                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defclass jde-compile-jikes (jde-compile-compiler)
  ()
  "Class of jikes compilers.")

(defmethod initialize-instance ((this jde-compile-jikes) &rest fields)
 ;; Call parent initializer.

  (call-next-method)

  ;; Set compiler name.
  (oset this :name "jikes")

  ;; Set compiler version.
  (oset this version "1.14"))

(defmethod jde-compile-debug-arg ((this jde-compile-jikes))
  "Get the debug arg for this compiler."
  (let ((include-option (nth 0 jde-compile-option-debug)))
    (cond
     ((string= include-option "all")
      (list "-g"))
     ((string= include-option "selected")
      (error "JDK 1.1 version of javac does not support selected debug info.")))))

(defmethod jde-compile-depend-arg ((this jde-compile-jikes))
  "Get dependency-checking argument for this compiler."
  (if jde-compile-option-depend
    (list "-depend")))

(defmethod jde-compile-command-line-args ((this jde-compile-jikes))
  "Get additional command line arguments for this compiler."
	(append
	 (list "+E")
	 jde-compile-option-command-line-args))

(defmethod jde-compile-classpath-arg ((this jde-compile-jikes))
  "Returns the classpath argument for this compiler."
  (let ((classpath (call-next-method))
        (rt        (expand-file-name "jre/lib/rt.jar" (jde-get-jdk-dir))))
    (if (file-exists-p rt)
        (or (string-match "jre/lib/rt\.jar" (cadr classpath))
            (setcar (cdr classpath)
                    (concat (cadr classpath)
                            jde-classpath-separator
                            rt))))
    classpath))

(defmethod jde-compile-get-args ((this jde-compile-jikes))
  (append
   (jde-compile-classpath-arg this)
   (jde-compile-sourcepath-arg this)
   (jde-compile-bootclasspath-arg this)
   (jde-compile-extdirs-arg this)
   (jde-compile-encoding-arg this)
   (jde-compile-debug-arg this)
   (jde-compile-output-dir-arg this)
   (jde-compile-deprecation-arg this)
   (jde-compile-optimize-arg this)
   (jde-compile-depend-arg this)
   (jde-compile-verbose-arg this)
   (jde-compile-verbose-path-arg this)
   (jde-compile-nowarn-arg this)
   (jde-compile-target-arg this)
   (jde-compile-command-line-args this)))


(defvar jde-compile-compilers
  (list
   (jde-compile-javac-11 "javac 1.1.x")
   (jde-compile-javac-12 "javac 1.2.x")
   (jde-compile-javac-13 "javac 1.3.x")
   (jde-compile-javac-14 "javac 1.4.x")
   (jde-compile-jikes "Jikes"))
  "List of supported Java compilers.")

(defun jde-compile-get-javac ()
  (let* ((jdk-version (jde-java-version))
	 (compiler
	  (find-if 
	   (lambda (compiler-x)
	     (string-match 
	      (oref compiler-x :version) 
	      jdk-version))
	   jde-compile-compilers)))
    (if (string= (car jde-compiler) "javac server")
	(oset compiler :use-server-p t)
      (progn
	(oset compiler :use-server-p nil)
	(oset compiler 
	      :path 
	      (let ((compiler-path 
		     (substitute-in-file-name (nth 1 jde-compiler))))
		(if (string= compiler-path "")
		    (let ((jdk-dir (jde-get-jdk-dir)))
		      (if jdk-dir
		       (progn
			 (setq compiler-path 
			       (expand-file-name 
				(if (eq system-type 'windows-nt) 
				    "bin/javac.exe" 
				  "bin/javac") 
				jdk-dir))
			 (if (file-exists-p compiler-path)
			     compiler-path
			   (error (format "Invalid compiler path %s" 
					  compiler-path))))
		       (if (executable-find "javac")
			   "javac"
			 (error "Cannot find javac."))))
		  (if (file-exists-p
		       (if (and
			    (eq system-type 'windows-nt)
			    (not (string-match "[.]exe$" compiler-path)))
			   (concat compiler-path ".exe")
			 compiler-path))
		      compiler-path
		    (if (executable-find compiler-path)
			compiler-path
		      (error "Invalid compiler path: %s"
			     compiler-path))))))))
    compiler))
	   
	     
(defun jde-compile-get-jikes ()
  (jde-compile-jikes
   "jikes"
   :use-server-p nil
   :path (let ((compiler-path 
		(substitute-in-file-name (nth 1 jde-compiler))))
	   (if (string= compiler-path "")
	       (if (executable-find "jikes")
		   "jikes"
		 (error "Cannot find jikes."))
	     (if (file-exists-p
		  (if (and
		       (eq system-type 'windows-nt)
		       (not (string-match "[.]exe$" compiler-path)))
		      (concat compiler-path ".exe")
		    compiler-path))
		 (if (executable-find compiler-path)
		     compiler-path
		   (error "Invalid compiler path: %s"
			  compiler-path)))))))

(defun jde-compile-get-the-compiler ()
  "Get a compiler object that represents the compiler specified
by `jde-compiler'."
   (if (string-match "javac" (car jde-compiler))
       (jde-compile-get-javac)
     (jde-compile-get-jikes)))
     

;;;###autoload
(defun jde-set-compile-options (options)
  "Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose."
  (interactive
   "sEnter options: ")
  (setq jde-compile-option-command-line-args (split-string options " ")))

;;;###autoload
(defun jde-compile ()
  "Compile the Java program in the current buffer.
This command invokes the compiler specified by `jde-compiler'
with the options specified by the JDE customization variables
that begin with `jde-compile'. If the variable
`jde-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled. If `jde-compiler' specifies the JDE compile
server, this command uses the compile server. Otherwise, it
uses the compiler executable specified by
`jde-compiler' to compile."
  (interactive)

  (if jde-read-compile-args
      (setq jde-interactive-compile-args
	      (read-from-minibuffer 
	       "Compile args: "
	       jde-interactive-compile-args
	       nil nil
	       '(jde-interactive-compile-arg-history . 1))))

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-compile from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not jde-xemacsp))	
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-compile
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (setq compilation-finish-function 
      (lambda (buf msg) 
	(run-hook-with-args 'jde-compile-finish-hook buf msg)
	(setq compilation-finish-function nil)))

    (let ((compiler (jde-compile-get-the-compiler)))

      (oset compiler 
	  :interactive-args 
	  (split-string jde-interactive-compile-args " "))

      (jde-compile-compile compiler)))
    


(provide 'jde-compile)

;; Change History
;; $Log: jde-compile.el,v $
;; Revision 1.26  2001/10/19 10:10:59  paulk
;; Removed space in front of -g option that made it unrecognizable to compiler.
;; Thanks to bert van vreckem.
;;
;; Revision 1.25  2001/10/19 04:14:20  paulk
;; Bug fix: compile command once again supports the jde-read-compile-args option.
;; Thanks to Luis Novais for reporting this bug.
;;
;; Revision 1.24  2001/10/16 05:10:50  paulk
;; - Fixed Lisp error that occurred when the default setting
;;   for jde-compile-option-debug is in effect.
;;
;; - XEmacs compatibility fix. Fixed undefined variable and
;;   function Lisp errors that occur when trying to compile
;;   a Java source file.
;;
;; - XEmacs compatibility fix. Fixed bug that produced a Beanshell
;;   error when trying to run the compile server in XEmacs.
;;
;; Revision 1.23  2001/10/08 13:06:11  paulk
;; Minor restructuring of compile server code.
;;
;; Revision 1.22  2001/10/05 11:02:27  paulk
;; Reordered the compile server output message so that the file name follows the other arguments.
;;
;; Revision 1.21  2001/10/05 04:08:01  paulk
;; - Fixed compile error that occurs when jde-compile-option-debug is set.
;;   This was due to the -g option being wrapped in an extra list.
;; - Now includes rt.jar in the classpath when jikes is the compiler.
;;   Thanks to David Ponce.
;;
;; Revision 1.20  2001/10/01 20:48:58  jslopez
;; Added support for jdk1.4.
;;
;; Revision 1.19  2001/10/01 12:02:34  paulk
;; - Now uses the version of javac shipped with the JDK specified by jde-jdk.
;; - Now generates the correct options for the various versions of javac.
;; - Now includes the +E options if jikes is the selected compiler.
;;
;; Revision 1.18  2001/10/01 02:24:49  paulk
;; - Created jde-compile-javac, jde-compile-jikes, jde-compile-javac-11x classes.
;;
;; - Fixed missing slot names in jde-compile-compiler class. Thanks to Eric Friedman.
;;
;; Revision 1.17  2001/09/28 05:10:10  paulk
;; - Redefined jde-compiler to allow selection of the
;;   following options: javac executable, javac server,
;;   and jikes executable.
;;
;; - Redefinded jde-compile-option-command-line-args to
;;   accept a list of strings, each corresponding to one
;;   argument.
;;
;; - Created a new class jde-compile-compiler to serve
;;   as the root class for all compilers supported by
;;   the JDE. This should promote sharing code among
;;   the compilers.
;;
;; Revision 1.16  2001/08/30 13:21:57  jslopez
;; Fixed bug in jde-compile-internal that did not scroll the compilation output
;; when the compile server and compilation-scroll-output were enable.
;;
;; Revision 1.15  2001/08/30 01:31:54  paulk
;; Adds support for compile server. Thanks to Javier Lopez.
;;
;; Revision 1.14  2001/04/16 05:47:33  paulk
;; Normalized paths. Thanks to Nick Sieger.
;;
;; Revision 1.13  2001/04/11 03:23:18  paulk
;; Updated to resolve relative paths relative to the project file that defines them. Thanks to Nick Sieger.
;;
;; Revision 1.12  2001/04/02 02:42:58  paulk
;; Remove extraneous definition of jde-build-classpath-arg.
;;
;; Revision 1.11  2001/03/13 04:03:47  paulk
;; Changed the type of jde-compile-option-directory from string to directory to permit path completion.
;;
;; Revision 1.10  2001/02/20 05:15:10  paulk
;; You can now use environment variables, tilde notation, and cygwin syntax in jde-compile-option-directory path.
;;
;; Revision 1.9  2001/02/17 17:43:34  paulk
;; Added support for JDK 1.3 targets.
;;
;; Revision 1.8  2001/02/03 08:18:44  paulk
;; Changed declarations of customized variables so that you can now use completion (M tab) to complete path names.
;;
;; Revision 1.7  2000/09/21 02:05:11  paulk
;; Fixes bug in formatting jde-compile-option-vm-args for the command line.
;;
;; Revision 1.6  2000/08/19 07:04:28  paulk
;; Adds compile finish hook.
;;
;; Revision 1.5  2000/08/11 05:04:45  paulk
;; Added jde-compile-finish-hook variable.
;;
;; Revision 1.4  2000/04/10 05:27:30  paulk
;; Compile command now supports Cygwin-style class paths.
;;
;; Revision 1.3  1999/01/15 22:04:15  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.2  1998/12/07 01:35:28  paulk
;; Updated compile options to reflect changes in command-line options
;; accepted by javac.
;;
;; Revision 1.1  1998/12/06 02:37:54  paulk
;; Initial revision
;;

;; End of jde-compile.el
