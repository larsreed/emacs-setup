;; jde-run.el --- runs the Java app in the current buffer.
;; $Revision: 1.73 $ $Date: 2002/09/10 13:51:47 $

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: tools, processes

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002 Paul Kinnucan

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
;; <URL:http://jde.sunsite.dk>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

(require 'eieio)

(defcustom jde-run-mode-hook nil
  "*List of hook functions run by `jde-run-mode' (see `run-hooks')."
  :group 'jde-project
  :type 'hook)

(defcustom jde-run-application-class ""
  "*Name of the Java class to run. 
This is the class that is run if you select JDE->Run App from the JDE
menu or type C-c C-v C-r. If this option is the empty string, the JDE
runs the class corresponding to the source file in the current
buffer. Note that the specified class must have a static public main
method."
  :group 'jde-project
  :type 'string)

(defcustom jde-run-working-directory ""
  "*Path of the working directory for this application.
If you specify a path, the JDE launches the application from the
directory specified by the path."
  :group 'jde-project
  :type 'file)


(defcustom jde-vm-path ""
  "*Path of the Java virtual machine executable. The path
can include environment variables, e.g.,
$JDK_HOME/bin/java. If the value of this variable is the
empty string, the JDE uses the vm that comes with the
version of the JDK specified by jde-jdk or, if jde-jdk
is not set, the vm on the system command path."
  :group 'jde-project
  :type 'file)


(defcustom jde-run-classic-mode-vm nil
"Runs applications in the classic (i.e., not HotSpot) mode."
  :group 'jde-project
  :type 'boolean)


(defcustom jde-run-read-vm-args nil
"*Read vm arguments from the minibuffer.
If this variable is non-nil, the jde-run command reads vm arguments
from the minibuffer and appends them to those specified by
the `jde-run-option' variable group."
  :group 'jde-project
  :type 'boolean)

(defvar jde-run-interactive-vm-arg-history nil
"History of vm arguments read from the minibuffer")

(defcustom jde-run-read-app-args nil
"*Read arguments to be passed to application from the minibuffer."
  :group 'jde-project
  :type 'boolean)

(defvar jde-run-interactive-app-arg-history nil
"History of application arguments read from the minibuffer")

(defgroup jde-run-options nil
  "JDE Interpreter Options"
  :group 'jde
  :prefix "jde-run-option-")

(defcustom jde-run-option-classpath nil
"*Specify paths of classes required to run this application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-run-options
  :type '(repeat (file :tag "Path")))
 
(defcustom jde-run-option-verbose (list nil nil nil)
  "*Print messages about the running process.
The messages are printed in the run buffer."
  :group 'jde-run-options
  :type '(list :indent 2
	       (checkbox :format "\n  %[%v%] %h \n"
			 :doc "Print classes loaded.
Prints a message in the run buffer each time a class is loaded.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print memory freed.
Prints a message in the run buffer each time the garbage collector
frees memory.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print JNI info.
Prints JNI-related messages including information about which native
methods have been linked and warnings about excessive creation of
local references.")))

(defcustom jde-run-option-properties nil
  "*Specify property values.
Enter the name of the property, for example, awt.button.color, in the
Property Name field; enter its value, for example, green, in the
Property Value field. You can specify as many properties as you like."
  :group 'jde-run-options
  :type '(repeat (cons 
		  (string :tag "Property Name") 
		  (string :tag "Property Value"))))

(defcustom jde-run-option-heap-size (list
				     (cons 1 "megabytes")
				     (cons 16 "megabytes"))
"*Specify the initial and maximum size of the interpreter heap."
:group 'jde-run-options
:type '(list
	(cons (integer :tag "Start")
	     (radio-button-choice (const "bytes")
				  (const "kilobytes")
				  (const "megabytes")))
	(cons (integer :tag "Max")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))


(defcustom jde-run-option-stack-size (list
				      (cons 128 "kilobytes")
				      (cons 400 "kilobytes"))
  "*Specify size of the C and Java stacks."
  :group 'jde-run-options
  :type '(list
	  (cons (integer :tag "C Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))
	  (cons (integer :tag "Java Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))

(defcustom jde-run-option-garbage-collection (list t t)
  "*Specify garbage collection options."
  :group 'jde-run-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect garbage asynchronously.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect unused classes.")))

(defcustom jde-run-option-java-profile (cons nil "./java.prof")
  "*Enable Java profiling."
  :group 'jde-run-options
  :type '(cons boolean
	       (file :tag "File"
		     :help-echo 
"Specify where to put profile results here.")))

(defcustom jde-run-option-heap-profile (cons nil
						   (list "./java.hprof"
							 5
							 20
							 "Allocation objects"))
"*Output heap profiling data."
  :group 'jde-run-options
  :type '(cons boolean
	       (list
		(string :tag "Output File Path")
		(integer :tag "Stack Trace Depth")
		(integer :tag "Allocation Sites")
		(radio-button-choice :format "%t \n%v"
				     :tag "Sort output based on:"
		 (const "Allocation objects")
		 (const "Live objects")))))
		 
(defcustom jde-run-option-verify (list nil t)
  "*Verify classes."
  :group 'jde-run-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Executed code in all classes.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Classes loaded by a classloader.")))

;; (makunbound 'jde-run-option-connect-to-debugger)
(defcustom jde-run-option-connect-to-debugger (list t "Attach" nil)
  "*If checked, this option allows the application to start and then
connect to a debugger, for example, jdb or JDEbug. \"DebuggerConnect
Mode\" specifies the method for establishing the connection. The
options are \"Attach\" (allow a debugger to attach to the application
after the applicaiton starts) or \"Client\" (start the application and
then try to connect the application to a listening debugger). On Unix
 systems, the \"Address\" option specifies the port number
of the socket used to connect the debugger and the application. On
Windows systems, it specifies an arbitrary identifier. If \"Address\"
is nil, the JDEE uses a default value. The default is \"javadebug\" on
Windows systems and 4444 on Unix systems."
  :group 'jde-run-options
  :type  '(list
	   (checkbox :format "%[%v%] %t \n\n"
                     :tag "Enabled")
	   (radio-button-choice :format "  %t\n%v" 
				:indent 2
                                :tag "Debugger Connect Mode:"
	    (const "Attach")
	    (const "Listen"))
	   (choice :format "\n  %[Address%] %v\n\n"
	    (const :menu-tag "Use Default"
                   nil)
	    (string :menu-tag "Specify" :tag "Value"))))
	  


(defcustom jde-run-option-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java interpreter. It is an alternative to using JDE Run Option
variables, such as `jde-run-option-stack-size', to specify Java
interpreter options. Also, it makes it possible to use the JDE with
interpreters that accept command line arguments not supported by 
the JDE Run Option variable set."
  :group 'jde-run-options
  :type '(repeat (string :tag "Argument")))

(defcustom jde-run-option-application-args nil
  "*Specify command-line arguments to pass to the application.
The JDE passes the specified arguments to the application on
the command line."
  :group 'jde-run-options
  :type '(repeat (string :tag "Argument")))

(defcustom jde-run-applet-viewer ""
  "*Specify name of viewer to use to display page containing the applet."
  :group 'jde-project
  :type 'file)

(defcustom jde-run-applet-doc ""
  "*Specify name of document containing applet to be viewed.
If no document is specified, JDE assumes that the document name is
APPLET.html, where APPLET is the name of the applet to be viewed."
  :group 'jde-project
  :type 'file)


(defcustom jde-appletviewer-option-encoding ""
"*Specify encoding of the HTML file displayed by the appletviewer."
  :group 'jde-run-options
  :type 'string)


(defcustom jde-appletviewer-option-vm-args nil
  "*Specify arguments (e.g., -Xmx16m) to the vm that runs appletviewer.
This option allows you to set the environment of the
virtual machine that runs appletviewer."
  :group 'jde-run-options
  :type '(repeat (string :tag "Argument")))


(defcustom jde-run-executable ""
  "*Specifies the executable to be run by the JDE's run command.
If you do not specify an executable, the JDE runs the vm specified
by `jde-run-get-vm'."
  :group 'jde-project
  :type 'file)

(defcustom jde-run-executable-args nil
  "*Specify arguments to be passed to the application executable.
This option allows you to specify one or more arguments to be passed
to the executable specified by `jde-run-executable'."
  :group 'jde-run-options
  :type '(repeat (string :tag "Argument")))


(defun jde-run-parse-args (s)
 "Converts a string of command-line arguments to a list of arguments.
Any substring that is enclosed in single or double quotes or does not include
whitespace is considered a parameter."
   (let ((n (string-match "[^\" ][^ ]*\\|\"[^\"]*\"\\|'[^']*'" s))
 	(i 0)
 	(tokens '()))
     (while n
       (setq tokens (append tokens (list (match-string 0 s))))
       (setq n (match-end 0))
       (setq n (string-match "[^\" ][^ ]*\\|\"[^\"]*\"\\|'[^']*'" s n)))
     tokens))

(defun jde-run-make-arg-string (args)
"Converts a list of command-line arguments to a string of arguments."
  (let ((str "")
	(n (length args))
	(i 0))
    (while (< i n)
      (if (not (string= str ""))
	  (setq str (concat str " ")))
      (setq str (concat str (nth i args)))
      (setq i (+ i 1)))
    str))

;;;###autoload
(defun jde-run-set-app (app)
  "Specify the name of the application class to run."
  (interactive 
   "sEnter application class: ")
  (setq jde-run-application-class app))

;;;###autoload
(defun jde-run-set-args (args)
  "Specify arguments to be passed to the Java vm.
This command serves as an alternative to using the JDE Run Options
panel to specify command-line arguments for the Java interpreter."
  (interactive 
   "sEnter arguments: ")
  (setq jde-run-option-vm-args (jde-run-parse-args args)))


;;;###autoload
(defun jde-run-set-app-args (args)
  "Specify the arguments to be passed to the Java application class.
This command provides an alternative to using the JDE Run Options panel
to specify command-line arguments to pass to the application when starting
the application."
  (interactive 
   "sEnter arguments: ")
  (setq jde-run-option-application-args (jde-run-parse-args args)))

;;;###autoload
(defun jde-run-set-applet-viewer (viewer)
  "Sets the viewer to be used to view an applet. The default is 
appletviewer."
  (interactive
   "sEnter viewer name: ")
  (setq jde-run-applet-viewer viewer))

;;;###autoload
(defun jde-run-set-applet-doc (doc)
  "Specify the doc to be used to view an applet.
This command provides an alternative to using the JDE Options
panel to specifying the applet document."
  (interactive
   "sEnter applet doc name: ")
  (if (string= doc "")
      (setq jde-run-applet-doc nil)
    (setq jde-run-applet-doc doc)))


(defclass jde-run-vm ()
  ((version          :initarg :version
		     :type string
		     :initform ""
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
   (main-class        :initarg :main-class
		     :type string
		     :documentation
		     "Name of main class."))
    "Class of Java virtual machines.")

(defmethod jde-run-classpath-arg ((this jde-run-vm))
  "Returns the classpath argument for this vm."
  (let ((classpath
	 (if jde-run-option-classpath
	     jde-run-option-classpath
	   jde-global-classpath))
	(symbol
	 (if jde-run-option-classpath
	     'jde-run-option-classpath
	   'jde-global-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jde-build-classpath
	  classpath symbol)))))

(defmethod jde-run-classic-mode-arg ((this jde-run-vm))
  "Get classic-mode option>"
    (if jde-run-classic-mode-vm
	(list "-classic")))

    
(defmethod jde-run-verbose-arg ((this jde-run-vm))
  "Set the verbose options."
    (let ((print-classes-loaded
	   (nth 0 jde-run-option-verbose))
	  (print-memory-freed
	   (nth 1 jde-run-option-verbose))
	  (print-jni-info
	   (nth 2 jde-run-option-verbose)))
      (append
       (if print-classes-loaded (list "-v"))
       (if print-memory-freed (list "-verbosegc"))
       (if print-jni-info (list "-verbosejni")))))

(defmethod jde-run-property-args ((this jde-run-vm))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jde-run-option-properties))

(defmethod jde-run-heap-size-args ((this jde-run-vm))
   "Get heap size arguments."
    (let* ((memory-unit-abbrevs
	    (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")))
	   (start-cons (nth 0 jde-run-option-heap-size))
	   (start-size (format "%d%s" (car start-cons) 
			       (cdr (assoc (cdr start-cons)
				      memory-unit-abbrevs))))
	   (max-cons (nth 1 jde-run-option-heap-size))
	   (max-size (format "%d%s" (car max-cons) 
			     (cdr (assoc (cdr max-cons)
				    memory-unit-abbrevs)))))
      (append
       (if (not (string= start-size "1m"))
	   (list (concat "-Xms" start-size)))
       (if (not (string= max-size "16m"))
	  (list (concat "-Xmx" max-size))))))

(defmethod jde-run-stack-size-args ((this jde-run-vm))
  "Get stack size arguments."
    (let* ((memory-unit-abbrevs
	    (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")))
	   (c-cons (nth 0 jde-run-option-stack-size))
	   (c-size (format "%d%s" (car c-cons) 
			       (cdr (assoc (cdr c-cons)
				      memory-unit-abbrevs))))
	   (java-cons (nth 1 jde-run-option-stack-size))
	   (java-size (format "%d%s" (car java-cons) 
			     (cdr (assoc (cdr java-cons)
				    memory-unit-abbrevs)))))
      (append
       (if (not (string= c-size "128k"))
	   (list (concat "-Xss" c-size)))
       (if (not (string= java-size "400k"))
           (list (concat "-Xoss" java-size))))))

(defmethod jde-run-gc-args ((this jde-run-vm))
  "Get garbage collection arguments."
    (let ((no-gc-asynch (not 
			 (nth 0 jde-run-option-garbage-collection)))
	  (no-gc-classes (not 
			  (nth 1 jde-run-option-garbage-collection))))
      (append
       (if no-gc-asynch
	  '("-Xnoasyncgc"))
       (if no-gc-classes
	   '("-Xnoclassgc")))))

(defmethod jde-run-java-profile-arg ((this jde-run-vm))
   "Get Java profile option."
    (let ((profilep (car jde-run-option-java-profile))
	  (file (cdr jde-run-option-java-profile)))
      (if profilep
	  (if (string= file "./java.prof")
	      '("-Xprof")
	    (list (concat "-Xprof:" file))))))

(defmethod jde-run-heap-profile-arg ((this jde-run-vm))
  "Get heap profile argument."
    (let* ((profilep (car jde-run-option-heap-profile))
	   (prof-options (cdr jde-run-option-heap-profile))
	   (file (nth 0 prof-options))
	   (depth (nth 1 prof-options))
	   (top (nth 2 prof-options))
	   (sort 
	    (downcase (substring (nth 3 prof-options) 0 1))))
      (if profilep
	  (if (and (string= file "./java.hprof")
		   (equal depth 5)
		   (equal top 20)
		   (string= sort "a"))
	      '("-Xhprof")
	    (list
	     (format 
	      "-Xhprof:file=%s,depth=%d,top=%d,sort=%s"
	      file depth top sort))))))

(defmethod jde-run-verify-args ((this jde-run-vm))
  "Get verify arguments."
    (let ((verify-all (nth 0 jde-run-option-verify))
	  (verify-remote (nth 1 jde-run-option-verify)))
      (append
       (if verify-all
	  '("-Xverify"))
;;       (if verify-remote
;;	   '("-Xverifyremote"))
       (if (and
	   (not verify-all)
	   (not verify-remote))
	   '("-Xnoverify")))))

(defmethod jde-run-debugger-connect-args ((this jde-run-vm))
  "Get arguments required to allow process to connect
to a debugger."
  (if (car jde-run-option-connect-to-debugger)
      (let ((mode (nth 1 jde-run-option-connect-to-debugger))
	     (address (nth 2 jde-run-option-connect-to-debugger))
	     (ms-windows (eq system-type 'windows-nt)))
	(list "-Xdebug"
	      (format 
	       "-Xrunjdwp:transport=%s,address=%s,server=%s,suspend=n"
	       (if ms-windows "dt_shmem" "dt_socket")
	       (if address address (if ms-windows "javadebug" "4444"))
	       (if (string= mode "Attach") "y" "n"))))))

(defmethod jde-run-vm-args ((this jde-run-vm))
  "Get command line args."
  jde-run-option-vm-args)

(defmethod jde-run-get-vm-args ((this jde-run-vm))
  (append
   ;; Classic mode argument must come first.
   (jde-run-classic-mode-arg this)
   (jde-run-classpath-arg this)
   (jde-run-verbose-arg this)
   (jde-run-property-args this)
   (jde-run-heap-size-args this)
   (jde-run-stack-size-args this)
   (jde-run-gc-args this)
   (jde-run-java-profile-arg this)
   (jde-run-heap-profile-arg this)
   (jde-run-verify-args this)
   (jde-run-debugger-connect-args this)
   (jde-run-vm-args this)))

(defmethod jde-run-vm-launch ((this jde-run-vm))
  (let ((run-buf-name (concat "*" (oref this :main-class) "*"))
	(source-directory default-directory)
	(working-directory (if (string= jde-run-working-directory "")
			       default-directory
			     (jde-normalize-path 'jde-run-working-directory))))
    (if (not (comint-check-proc run-buf-name))
	(let* ((run-buffer (get-buffer-create run-buf-name))
	       (win32-p (eq system-type 'windows-nt))
	       (prog (oref this :path))
	       (prog-args (append
			   (jde-run-get-vm-args this)
			   (if jde-run-read-vm-args
			       (jde-run-parse-args
				(read-from-minibuffer
				 "Vm args: "
				 (car jde-run-interactive-vm-arg-history)
				 nil nil
				 'jde-run-interactive-vm-arg-history)))
			   (list (oref this :main-class))
			   jde-run-option-application-args
			   (if jde-run-read-app-args
			       (jde-run-parse-args
				(read-from-minibuffer
				 "Application args: "
				 (car jde-run-interactive-app-arg-history)
				 nil nil
				 'jde-run-interactive-app-arg-history)))
			   ))
	       (command-string (concat prog " " 
				       (jde-run-make-arg-string
					prog-args)
				       "\n\n")))
	  (save-excursion
	    (set-buffer run-buffer)
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (jde-run-mode))
	  (let ((win32-start-process-show-window t)
		(w32-start-process-show-window t)
		(w32-quote-process-args ?\")
		(win32-quote-process-args ?\") ;; XEmacs
		(windowed-process-io t))
	    (comint-exec run-buffer (oref this :main-class) prog nil prog-args))
	  (pop-to-buffer run-buffer)
          (save-excursion
            (goto-char (point-min))
          (jde-run-etrace-update-current-marker))
	  (cd source-directory))
      (message "An instance of %s is running." (oref this :main-class))
      (pop-to-buffer run-buf-name))))

;;;###autoload
(defun jde-run (main-class)
  "Run the Java application specified by `jde-run-executable', if
not the null string. Otherwise run the class specified by 
`jde-run-application-class', if non-null; otherwise the class in
the current buffer. Specifying a prefix argument, e.g.,
C-u C-c C-v C-r, causes this command to prompt you to enter
the name of the application's main class This command 
creates a comint buffer to allow you to interact with the program."
  (interactive "P")
  (if (equal major-mode 'jde-mode)
      (if (string= jde-run-executable "")
	  (let ((vm (jde-run-get-vm)))
	    (oset
	     vm
	     :main-class
	     (if main-class
		 (read-from-minibuffer
		  "Main class: "
		  (concat (jde-db-get-package)
		      (file-name-sans-extension 
		       (file-name-nondirectory (buffer-file-name)))))
	       (jde-run-get-main-class)))
	    (jde-run-vm-launch vm))
	(jde-run-executable))
    (error "The jde-run command works only in a Java source buffer.")))

(defun jde-run-get-main-class () 
  "Gets the main class for the application to which the current
source buffer belongs."
  (let ((main-class jde-run-application-class))
    (if (or
	 (not main-class)
	 (string= main-class ""))
	(setq main-class
	      (concat (jde-db-get-package)
		      (file-name-sans-extension 
		       (file-name-nondirectory (buffer-file-name))))))
    main-class))

(defvar jde-run-virtual-machines
  (list
   (jde-run-vm "JDK vm"))
  "*List of supported virtual machines.")

(defun jde-run-get-vm ()
  "Gets the vm for the current JDK."
  (let* ((jdk-version (jde-java-version))
	 (vm
	  (find-if
	   (lambda (vm-x)
	     (string-match
	      (oref vm-x :version)
	      (if jdk-version jdk-version "")))
	   jde-run-virtual-machines)))
    (if (not vm)
	(setq vm (car jde-run-virtual-machines)))
    (oset vm 
	  :path 
	  (let ((vm-path 
		 (substitute-in-file-name jde-vm-path)))
	    (if (string= vm-path "")
		(let ((jdk-dir (jde-get-jdk-dir)))
		  (if jdk-dir
		      (progn
			 (setq vm-path 
			       (expand-file-name 
				(if (eq system-type 'windows-nt) 
				    "bin/javaw.exe" 
				  "bin/java") 
				jdk-dir))
			 (if (file-exists-p vm-path)
			     vm-path
			   (error (format "Invalid vm path %s" 
					  vm-path))))
		       (if (executable-find "java")
			   (if (eq system-type 'windows-nt) "javaw" "java")
			 (error "Cannot find a Java vm."))))
		  (if (file-exists-p
		       (if (and
			    (eq system-type 'windows-nt)
			    (not (string-match "[.]exe$" vm-path)))
			   (concat vm-path ".exe")
			 vm-path))
		      vm-path
		    (if (executable-find vm-path)
			vm-path
		      (error "Invalid vm path: %s"
			     vm-path))))))
    vm))

(defun jde-run-main-class()
  "Runs the Java program named by `jde-run-application-class' in
a buffer, piping output from the program to the buffer and 
input from the buffer to the program."
  (interactive)
  (let ((vm (jde-run-get-vm)))
    (oset vm :main-class (jde-run-get-main-class))
    (jde-run-vm-launch vm)))


(defmacro save-w32-show-window (&rest body)
  "Saves the value of the w32-start-process-show-window variable
before evaluating body and restores the value afterwards."
  `(let ((win32-start-process-show-window t)
	 (w32-start-process-show-window t)
	 (windowed-process-io t))		 
     ,@body))

(defun jde-run-unquote (string)
  (if (eq (aref string 0) ?\")
      (substring string 1 (- (length string) 1))
    string))

(defun jde-run-application-running-p ()
  "*Returns t if the application to which the current
buffer belongs is running."
  (let ((run-buf-name (concat "*" (jde-run-get-main-class) "*")))
    (comint-check-proc run-buf-name)))


(defun jde-run-executable()
  (let* ((prog-path
	  (jde-normalize-path 
	   jde-run-executable
	   'jde-run-executable))
	 (prog-name (file-name-sans-extension
		     (file-name-nondirectory 
		      prog-path)))
	 (run-buf-name (concat "*" prog-name "*"))
	 (source-directory default-directory)
	 (working-directory (if (string= jde-run-working-directory "")
				default-directory
			      (jde-normalize-path 'jde-run-working-directory))))
    (if (not (comint-check-proc run-buf-name))
	(let* ((w32-quote-process-args ?\")
	       (win32-quote-process-args ?\") ;; XEmacs
	       (run-buffer (get-buffer-create run-buf-name))
	       (prog-args (append
			   jde-run-executable-args
			   (if jde-run-read-app-args
			       (jde-run-parse-args
				(read-from-minibuffer
				 "Application args: "
				 nil
				 nil nil
				 '(jde-run-interactive-app-arg-history . 1))))
			   ))
	       (command-string (concat jde-run-executable " " 
				       (mapconcat (lambda (arg) arg)
						  prog-args " ")
				       "\n\n")))
	  (save-excursion
	    (set-buffer run-buffer)
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (jde-run-mode))
	  (save-w32-show-window
	   (comint-exec run-buffer prog-name prog-path nil prog-args))
	  (pop-to-buffer run-buffer)
          (save-excursion
            (goto-char (point-min))
            (jde-run-etrace-update-current-marker))
	  (cd source-directory))
      (message "An instance of %s is running." prog-name)
      (pop-to-buffer run-buf-name))))

(defun jde-run-mode-internal()
  "Mode for running Java programs."
  (define-key (current-local-map) "\C-c\C-v\C-[" 'jde-run-etrace-prev)
  (define-key (current-local-map) "\C-c\C-v\C-]" 'jde-run-etrace-next)
  (define-key (current-local-map) [mouse-2] 'jde-run-etrace-show-at-mouse)
  (font-lock-mode 1)
  (jde-run-etrace-setup-font-lock))

(define-derived-mode 
  jde-run-mode comint-mode "JDE Run Mode"
  "Major mode for running Java applications and applets.
  \\{jde-run-mode-map}"
  (jde-run-mode-internal))

(defun jde-get-appletviewer-options ()
  (let (options)
    (if (not (string= jde-appletviewer-option-encoding ""))
	(setq options (list 
			"-encoding"
		        jde-appletviewer-option-encoding)))
    (if jde-appletviewer-option-vm-args
	(let ((len (length jde-appletviewer-option-vm-args))
	      (n 0))
	  (while (< n len)
	    (setq options
		  (nconc 
		   options
		   (list 
		    (concat "-J"
			    (nth n jde-appletviewer-option-vm-args)))))
	    (setq n (1+ n)))))
    options))
 
(defun jde-run-applet-exec (buffer name command startfile switches)
  "A version of comint-exec patched to start an applet viewer as
a command shell subprocess rather than as a subprocess of Emacs. This
is necessary to avoid displaying a DOS window when starting a viewer
under Windows."
  (save-excursion
    (set-buffer buffer)
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc (jde-run-applet-exec-1 name buffer command switches)))
      (set-process-filter proc 'comint-output-filter)
      (make-local-variable 'comint-ptyp)
      (setq comint-ptyp process-connection-type) ; T if pty, NIL if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
    (run-hooks 'comint-exec-hook)
    buffer)))

;; This auxiliary function cranks up the process for jde-run-applet-exec in
;; the appropriate environment.

(defun jde-run-applet-exec-1 (name buffer command switches)
  (let ((w32-quote-process-args ?\")
	(win32-quote-process-args ?\") ;; XEmacs
	(process-environment
	 (nconc
	  ;; If using termcap, we specify `emacs' as the terminal type
	  ;; because that lets us specify a width.
	  ;; If using terminfo, we specify `dumb' because that is
	  ;; a defined terminal type.  `emacs' is not a defined terminal type
	  ;; and there is no way for us to define it here.
	  ;; Some programs that use terminfo get very confused
	  ;; if TERM is not a valid terminal type.
	  (if (and (boundp 'system-uses-terminfo) system-uses-terminfo)
	      (list "TERM=dumb"
		    (format "COLUMNS=%d" (frame-width)))
	    (list "TERM=emacs"
		  (format "TERMCAP=emacs:co#%d:tc=unknown:" (frame-width))))
	  (if (getenv "EMACS") nil (list "EMACS=t"))
	  process-environment))
	(default-directory
	  (if (file-directory-p default-directory)
	      default-directory
	    "/")))
    (apply 'start-process-shell-command name buffer command switches)))

(defun jde-run-applet-internal (doc)
  (let* ((doc-file-name (file-name-nondirectory doc))
	 (doc-directory (file-name-directory doc))
	 (doc-name (file-name-sans-extension doc-file-name))
	 (run-buf-name (concat "*" doc-name "*")))

    (if (not (comint-check-proc run-buf-name))
	(let* ((run-buffer (get-buffer-create run-buf-name))
	       (win32-p (eq system-type 'windows-nt))
	       (prog jde-run-applet-viewer)
	       (prog-args
		(append (jde-get-appletviewer-options)
			(list doc-file-name)))
	       (command-string (concat prog " "
				       (jde-run-make-arg-string
					prog-args)
				       "\n\n")))
	  (save-excursion
	    (set-buffer run-buffer)
	    (erase-buffer)
	    (cd doc-directory)
	    (insert (concat "cd " doc-directory "\n"))
	    (insert command-string)
	    (jde-run-mode))
	  (jde-run-applet-exec run-buffer doc-name prog nil prog-args)
	  (pop-to-buffer run-buffer))
      (message "An instance of the applet in %s is running." doc-name)
      (pop-to-buffer run-buf-name))))


(defun jde-run-find-html-files ()
  "If (buffer-file-name) is /a/b/c.xxx (where xxx can be anything), 
return (\"/a/b/c.html\") if it exists, else return (\"/a/b/c.htm\") 
if it exists, else return a list of all *.html files in /a/b/
directory."
  (let ((basename (file-name-sans-extension (buffer-file-name)))
	f)
    (cond 
     ((file-exists-p (setq f (concat basename ".html")))
      (list f))
     ((file-exists-p (setq f (concat basename ".htm"))) ;; for poor winXX souls
      (list f))
     (t
      (mapcan (lambda (file)
		(if (or
		     (string-match "[.]html$" file)
		     (string-match "[.]htm$" file))
		    (list file)))       
	      (directory-files 
	       (file-name-directory (buffer-file-name)) t))))))



(setq jde-run-applet-last-doc nil) 

;;;###autoload
(defun jde-run-applet (&optional doc)
  "Runs an applet. This function prompts you to enter the path of an
html document that displays the applet. If you enter return without
specifying a document, this function next checks whether
`jde-run-applet-doc' specifies a document. If so, it displays that
specified document. Next, it checks whether the current directory
contains any html files. If the current directory contains an html
file with the same root name as the Java file in the current buffer,
it displays the file. If not, it displays the first html file that it
finds in the current directory. If if cannot find an html file, it
signals an error.  This function uses the viewer specified by
`jde-run-applet-viewer' to display the specified document. Note that
if you run two html applet files successively with the same name, you
must kill the buffer created to run the first file before running the
second file. Otherwise, this command will simply redisplay the first
file."
  (interactive
   (let ((insert-default-directory nil))
     (list (read-file-name "Applet doc: " nil nil nil jde-run-applet-last-doc))))
  (setq jde-run-applet-last-doc doc)
  (let ((applet-doc (if (and jde-run-applet-last-doc
			     (not (string= jde-run-applet-last-doc "")))
			jde-run-applet-last-doc
		      (if (and jde-run-applet-doc
			       (not (string= jde-run-applet-doc "")))
			    jde-run-applet-doc
			  (car (jde-run-find-html-files))))))
    (if applet-doc
	(if (string-match "appletviewer" jde-run-applet-viewer)
	    (jde-run-applet-internal applet-doc)
	  (if (or
	       (string= jde-run-applet-viewer "")
	       (string-match "browse-url" jde-run-applet-viewer))
	      (browse-url applet-doc 
			  (if (boundp 'browse-url-new-window-flag)
			      'browse-url-new-window-flag
			    browse-url-new-window-p))
	    (jde-run-applet-internal (concat default-directory applet-doc))))
      (signal 'error "Could not find html document to display applet."))))


(defun jde-run-menu-run-applet ()
  (interactive)
  (jde-run-applet))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  Exception Trace Navigation                                          ;;
;;                                                                      ;;           
;;  Copyright (C) 1999 Phillip Lord <p.lord@hgmp.mrc.ac.uk>             ;; 
;;  Copyright (C) 2001 Sam Steingold <sds@gnu.org>                      ;;              
;;  Copyright (C) 2001 Kevin A. Burton <burton@apache.org>              ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
(defvar jde-run-etrace-current-marker (cons (make-marker) (make-marker))
  "The location of the last stack shown.
A cons of two markers, location of the error and the location in the code.")

(defun jde-run-etrace-update-current-marker () 
  "Updates the `car' of `jde-run-etrace-current-marker' to be at the current point"
  (set-marker (car jde-run-etrace-current-marker) (point) (current-buffer)))

(defun jde-run-etrace-current-marker (&optional next)
  "Update the `cdr' and the `car' of `jde-run-etrace-current-marker' from its `car'.
Here goes all the error message parsing."
  (let ((here (car jde-run-etrace-current-marker))
        (there (cdr jde-run-etrace-current-marker))
        (n (or next 0))
        (re (concat " \\([a-zA-Z0-9_.]+\\.\\)?" ; package
                     "\\([a-zA-Z0-9_$]+\\)" ; class
                     "\\.<?[a-zA-Z0-9_]+>?" ; method
                     "(\\([a-zA-Z0-9_]+\\)\\.java:" ; java file = public class
                     "\\([0-9]+\\))"))); line number
    (save-excursion
      (set-buffer (marker-buffer here))
      (goto-char here)
      ;; In order to display the current match at the top of 
      ;; the error screen the previous match was set to be 
      ;; at the beggining of the line. If we do not place the point
      ;; at the end of the line again, you will match the same error.
      ;; If n is 0 ensure that point is at the beggining
      (if (> n 0)
          (end-of-line)
        (beginning-of-line))
      
      (if (>= n 0)
          (if (re-search-forward re nil t)
              (progn
                ;;setting the line at the beggining
                ;;so that it is the first line of the errors
                (beginning-of-line) 
                (jde-run-etrace-update-current-marker))
            (error "End of stack trace"))
        (if (re-search-backward re nil t)
            (progn
              (jde-run-etrace-update-current-marker))
          (error "Start of stack trace")))
;;       (message "1: [%s]; 2: [%s]; 3: [%s]; 4: [%s]" (match-string 1)
;;                (match-string 2) (match-string 3) (match-string 4))
      (let* ((package (or (match-string 1) ""))
            (class (match-string 2))
            (file-name (match-string 3))
            (line (car (read-from-string (match-string 4))))
            (file (jde-find-class-source-file (concat package file-name)))
	    buf) 
        (condition-case err
            (progn 
              (setq buf (if file (find-file-noselect file)))
              (set-buffer buf)
              (goto-line line)
              (set-marker there (point) buf))
          (error err))))
    jde-run-etrace-current-marker))

(defun jde-run-etrace-goto (&optional next)
  "Display the current stack using `compilation-goto-locus'."
  (compilation-goto-locus (jde-run-etrace-current-marker next)))


(defun jde-run-etrace-show-at-mouse (event)
  "Jump to the stack position at the mouse click.
Click anywhere on the line with the stack reference."
  (interactive "e")
  (let ((pos (event-start event)))
    (set-marker (car jde-run-etrace-current-marker)
                (posn-point pos)
                (window-buffer (posn-window pos))))
  (jde-run-etrace-goto))


(defun jde-run-etrace-show-at-point ()
  "Jump to the stack position on this current line.
The point should be anywhere on the line with the stack reference."
  (interactive)
  (set-marker (car jde-run-etrace-current-marker) (point) (current-buffer))
  (jde-run-etrace-goto))


(defun jde-run-etrace-next ()
  "Jump to the next stack position (next line)."
  (interactive)
  (jde-run-etrace-goto 1))


(defun jde-run-etrace-prev ()
  "Jump to the previous stack position (previous line)."
  (interactive)
  (jde-run-etrace-goto -1))

(defun jde-run-etrace-setup-font-lock ()
    ;;setup the correct font-lock stuff


  ;;font lock setup notes
  ;;
  ;; the actual exception class -> font-lock-keyword-face
  ;; exception message -> font-lock-string-face
  ;; stack entry class and method -> font-lock-constant-face
  ;; stack entry file -> font-lock-variable-name-face
  ;; stack entry line number -> font-lock-type-face

  (if (featurep 'xemacs)
      nil
    (progn
      (font-lock-add-keywords 
       nil 
       '(("\\(^[_a-z.]+[_a-zA-Z0-9]+Exception\\)\\(: \\)?\\(.*\\)?"
	  (1 'font-lock-keyword-face append)
	  (3 'font-lock-string-face append))))
  
      (font-lock-add-keywords 
       nil 
       '(("\\(at [_a-z.]+[_a-zA-Z0-9]+\\.[_a-zA-Z<>]*\\)(\\([_a-zA-Z0-9]+.java\\):\\([0-9]+\\))$"
	  (1 'font-lock-constant-face append)
	  (2 'font-lock-variable-name-face append)
	  (3 'font-lock-type-face append))))
      (font-lock-fontify-buffer))))

(provide 'jde-run)


;; Change History
;; $Log: jde-run.el,v $
;; Revision 1.73  2002/09/10 13:51:47  jslopez
;; Fixes regression bug in jde-run-etrace-set-font-lock.
;;
;; Revision 1.72  2002/09/08 14:33:58  jslopez
;; Fixes etrace highlighting of classes that contains underscores.
;; Thanks to Laurent Martelli <laurent@bearteam.org>
;;
;; Revision 1.71  2002/08/20 02:34:50  jslopez
;; Fixes bug in etrace command that did not recognize inner classes as valid
;; stack positions.
;;
;; Revision 1.70  2002/08/07 06:36:17  paulk
;; Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
;; removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
;; for suggesting this restructuring. Also fixed a number of compiler warnings caused
;; by genuine bugs.
;;
;; Revision 1.69  2002/06/12 07:04:23  paulk
;; XEmacs compatibility fix: set win32-quote-process-args wherever
;; the JDEE sets w32-quote-process-args. This allows use of spaces in
;; paths passed as arguments to processes (e.g., javac)  started by
;; the JDEE.
;;
;; Revision 1.68  2002/06/11 06:26:14  paulk
;; Provides support for paths containing spaces as vm arguments via the following change:
;; locally set the w32-quote-process-args variable to a quotation mark when launching
;; the vm process.
;;
;; Revision 1.67  2002/03/24 05:30:10  paulk
;; Fixed regression error in jde-run command that prevented use of the prefix
;; argument to cause the command to prompt the user for the main class.
;;
;; Revision 1.66  2002/03/08 13:14:21  paulk
;; Fixed bug in jde-run-debugger-connect-args that generates an incorrect
;; -Xrunjdwp argument on Unix. Thanks to Jens Lautenbacher.
;;
;; Revision 1.65  2002/03/05 10:57:22  paulk
;; Fixed bug in jde-run-debugger-connect-args.
;;
;; Revision 1.64  2002/03/04 06:46:51  paulk
;; Adds jde-run-option-connect-to-debugger variable
;;
;; Revision 1.63  2002/02/08 11:52:17  paulk
;; The jde-run-executable command now normalizes the path to the executable.
;;
;; Revision 1.62  2001/12/08 04:11:26  paulk
;; Updated doc string for jde-run-executable.
;;
;; Revision 1.61  2001/12/05 19:16:53  jslopez
;; Now jde-run-etrace-prev and jde-run-etrace-next
;; allow you to move through the whole stack trace.
;; It behaves as compilation-next-error.
;; Better error handling.
;;
;; Revision 1.60  2001/12/05 02:45:34  jslopez
;; Substitute jde-xemacsp for (featurep 'xemacs).
;;
;; Revision 1.59  2001/12/05 02:41:26  jslopez
;; Fixes typo in the documentation
;; for jde-run-etrace-prev and
;; jde-run-etrace-next.
;;
;; Revision 1.58  2001/11/30 02:21:41  jslopez
;; Removes obsolete methods setting jde-run-java-vm
;; and jde-run-java-vm-w.
;; Substitutes remaining references to jde-run-java-vm
;; and jde-run-java-vm-w for jde-run-get-vm.
;;
;; Revision 1.57  2001/11/13 07:35:22  paulk
;; jde-get-java-vm returns the default vm when the JDE cannot determine the
;; version of the vm to be used.
;;
;; Revision 1.56  2001/11/05 05:10:56  paulk
;; Provided a better fix for classic mode argument.
;;
;; Revision 1.55  2001/11/04 14:31:53  paulk
;; Fixed regression in support for -classic argument.
;;
;; Revision 1.54  2001/10/31 08:58:44  paulk
;; Emacs 21 compatibility fix: added support for browse-url-new-window-flag variable (replaces browse-url-new-window-p in Emacs 21).
;;
;; Revision 1.53  2001/10/23 05:31:25  paulk
;; Replaces the customization variables jde-run-java-vm and
;; jde-run-java-vm-w with the variable jde-vm-path.  If this variable is
;; a valid path, the JDE uses this path to find the vm used to run Java
;; applications.  If this path is the empty string, the JDE runs the vm
;; included with the version of the JDK specified by jde-jdk, or if
;; jde-jdk is nil, the first vm on the system command path.
;;
;; This change is part of an ongoing enhancement of the JDE to permit
;; easy switching of JDK's. Now jde-jdk specifies both the compiler and
;; the vm used by the JDE.
;;
;; Revision 1.52  2001/04/28 06:22:19  paulk
;; Makes jde-run-mode a full-fledged major mode derived from comint-mode.
;;
;; Revision 1.51  2001/04/16 06:00:50  paulk
;; Normalize paths. Thanks to Nick Sieger.
;;
;; Revision 1.50  2001/04/12 04:37:45  paulk
;; Normalize jde-run-working-directory.
;;
;; Revision 1.49  2001/04/11 03:18:04  paulk
;; Updated to resolve relative paths relative to the project file that defines them. Thanks to Nick Seiger.
;;
;; Revision 1.48  2001/04/09 05:28:20  paulk
;; 
;; Revision 1.47  2001/03/16 04:52:46  paulk
;; Use major-mode instead of mode-name to check the buffer mode. Thanks to Kevin Burton.
;;
;; Revision 1.46  2001/03/02 04:10:37  paulk
;; Updated jde-run prompt to specify the class in the current buffer as the suggested response.
;;
;; Revision 1.45  2001/03/01 12:50:20  paulk
;; The jde-run command now prompts you to enter the name of the
;; application's main class if you type a prefix (C-u) first.
;;
;; Revision 1.44  2001/02/26 04:13:11  paulk
;; jde-run now handles case where jde-global-classpath and jde-run-option-classpath are nil.
;;
;; Revision 1.43  2001/02/03 08:23:53  paulk
;; Changed declaration of customized variables so you can use completion on path variables.
;;
;; Revision 1.42  2001/02/03 07:30:27  paulk
;; Now uses jde-build-classpath instead of jde-run-build-classpath to build classpath argument.
;;
;; Revision 1.41  2001/02/01 06:13:47  paulk
;; *** empty log message ***
;;
;; Revision 1.40  2000/10/20 04:11:07  paulk
;; Fix that allows the JDE to be used with NT/XEmacs.
;;
;; Revision 1.39  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.38  2000/09/21 04:45:32  paulk
;; Updates jde-run-applet to work with the appletviewer in JDK 1.3.
;;
;; Revision 1.37  2000/08/19 07:02:02  paulk
;; Changed variable name.
;;
;; Revision 1.36  2000/03/16 05:18:11  paulk
;; Miscellaneous small bug fixes and enhancements.
;;
;; Revision 1.35  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.34  2000/02/01 04:11:56  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.33  1999/12/27 08:02:13  paulk
;; Enhanced JDE->Run App command to run executables.
;;
;; Revision 1.32  1999/12/14 05:15:09  paulk
;; JDE->Run Applet now looks in the current Java source directory for an
;; html file having the same root name as the current Java source
;; buffer. If it finds such a file, it runs it. Otherwise, it runs the first html file that it encounters in the directory. Thanks to  Richard Y. Kim <ryk@coho.net> for providing a patch implementing this change.
;;
;; Revision 1.31  1999/12/03 08:22:00  paulk
;; Updated JDEbug to run under JDK 1.3beta.
;;
;; Revision 1.30  1999/09/28 04:06:59  paulk
;; Supplied missing left parentheses.
;;
;; Revision 1.29  1999/09/05 04:33:28  paulk
;; Added support for running vm in classic mode.
;;
;; Revision 1.28  1999/08/29 04:29:18  paulk
;; Patches provided by Michael Ernst <mernst@alum.mit.edu>
;;
;; Revision 1.27  1999/07/04 03:31:11  paulk
;; Added jde-run-application-running-p predicate function.
;;
;; Revision 1.26  1999/05/07 23:22:09  paulk
;; Changed jde-run-parse-args to accept any substring enclosed in single or double
;; quotes or that does not contain white space as an arg.
;;
;; Revision 1.25  1999/02/10 18:29:03  paulk
;; Added support for appletviewer options.
;;
;; Revision 1.24  1999/02/05 21:59:30  paulk
;; Added file-completion and default-to-last-entry to jde-run-applet
;; command.
;;
;; Revision 1.23  1999/02/03 01:08:34  paulk
;; Enhanced jde-run-applet to look in current directory for html file
;; to display, if you do not specify a file in the minibuffer or via
;; jde-run-applet-doc. Also fixed a bug in the minibuffer version of
;; jde-run-applet that forced you always to specify the name of an
;; html document.
;;
;; Revision 1.22  1999/01/15 21:59:34  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.21  1998/12/06 02:19:36  paulk
;; Fixed bug with jde-run-options-properties. (The bug was putting a space before the
;; -D switch.)
;;
;; Revision 1.20  1998/09/11 23:53:32  paulk
;; Added a jde-run-working-directory customization variable. If set to a valid
;; path, the JDE starts the application from the directory specified by the
;; path. If the value of this variable is the empty string (the default),
;; the JDE starts the application from the default directory of the current
;; source buffer. The default directory is usually the directory containing
;; the source file.
;;
;; Revision 1.19  1998/08/28 12:49:23  paulk
;; Updated to support NT/Emacs 20.3
;;
;; Revision 1.18  1998/07/02 05:36:00  paulk
;; Added $ to the set of characters recognized by the JDE as valid
;; in vm and Java app command-line arguments.
;;
;; Revision 1.17  1998/06/30 21:10:28  paulk
;; Fixed jde-run-parse-args to recognize % as an argument
;; character.
;;
;; Revision 1.16  1998/06/30 03:32:37  paulk
;; Added the variables `jde-run-read-vm-args' and `jde-run-read-app-args'.
;; The first cause the jde-run command to read vm arguments from the
;; minibuffer and append them to the vm arguments specified by
;; the `jde-run-option' group of customization variables. The second
;; causes jde-run to read arguments to be passed to the application
;; from the minibuffer and append them to the arguments specified
;; by `jde-run-applications-args'. The JDE maintains separate histories
;; for both types of arguments.
;;
;; Revision 1.15  1998/05/27 06:01:04  paulk
;; Added autoload comments.
;;
;; Revision 1.14  1998/03/04 04:08:21  kinnucan
;; Fixed bug in jde-run.
;;
;; Revision 1.13  1998/02/27 21:55:04  kinnucan
;; * Added support for Emacs customization feature.
;;
;; Revision 1.12  1997/10/26 05:57:22  kinnucan
;; Fixed bug where jde-run was incorrectly parsing command line arguments
;; containing an equal (=) sign.
;;
;; Revision 1.11  1997/10/05 21:21:59  kinnucan
;; Unquoted classpath as quotes are only necessary for compilation (because
;; the JDE uses a shell to run the compiler).
;;
;; Revision 1.10  1997/10/05 17:15:44  kinnucan
;; Added the function jde-run-set-app-args, which allows you to
;; specify command line arguments for the application you are running.
;;
;; Also, changed the value of jde-run-args from a string to a list.
;;
;; Revision 1.9  1997/09/16 02:37:16  kinnucan
;; Changed w32-start-process-show-window to win32-start-process-show-window
;;
;; Revision 1.8  1997/09/04 03:54:34  kinnucan
;; Added jde-run-applet command, which runs a Java applet.
;;
;; Revision 1.7  1997/08/29 03:19:04  kinnucan
;; Fixed bug in save-w32-show-window.
;;
;; Revision 1.6  1997/08/26 08:46:41  kinnucan
;; Tweaked version number.
;;
;; Revision 1.5  1997/08/26 08:33:16  kinnucan
;; Deleted superfluous comments.
;;
;; Revision 1.4  1997/08/26 08:31:36  kinnucan
;; 1. Ported jde-run onto comint mode.
;;
;;    This allows you to interact with a Java application in the
;;    run buffer, if the application accepts command line input.
;;    You can use the comint history features to facilitate interaction
;;    with such an application.
;;
;; 2. Added the jde-run-set-java-vm and jde-run-set-java-vm-w
;;    commands, which let you specify the Java interpreter to use to
;;    run on non-Windows and Windows platforms, respectively.
;;
;;    Note that you must use javaw on Windows platforms to avoid
;;    opening a superfluous command shell window.
;;
;; 3. Added the jde-run-set-args command and associated jde-run-args
;;    variable, which let you specify Java interpreter options via
;;    command-line arguments.
;;
;;    jde-run passes the value of jde-classpath (defined in jde.el
;;    and set via the jde-set-classpath command) and jde-run-args
;;    to the Java interpreter.
;;
;;   This means that you can use a common classpath definition for
;;   compiling and running applications, while passing other
;;   runtime arguments via jde-run-set-args.
;;
;; Revision 1.3  1997/07/05 04:20:44  kinnucan
;; Modified jde-run command to derive the class name from the name of the file in
;; the current buffer rather than the buffer name. This avoids an incorrect derivation
;; when more than one buffer is open on the same source file.
;;
;; Revision 1.2  1997/06/29 08:23:21  kinnucan
;; 1. Added jde-run-set-app function, which lets you specify the application
;;    class to run.
;;
;; 2. Updated jde-run to run either the app specified by jde-run-set-app or
;;    the class whose source is in the current buffer. In the latter case,
;;    jde-run extracts the package of the app class from the source buffer.
;;
;; Revision 1.1  1997/06/18 17:23:28  paulk
;; Initial revision
;;

;;; jde-run.el ends here
