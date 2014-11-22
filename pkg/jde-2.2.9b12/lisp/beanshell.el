;;; beanshell.el
;; $Revision: 1.47 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002 Paul Kinnucan.

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

(require 'eieio)

(defgroup bsh nil
  "Customizations for the Emacs inteface to Pat Neimeyer's Java
interpreter, the Beanshell."
  :group 'tools
  :prefix "bsh-")

;; (makunbound 'bsh-startup-timeout)
(defcustom bsh-startup-timeout 10
  "*Length of time Emacs waits for the Beanshell to startup.
Increase the value of this variable if you get Lisp errors on
BeanShell startup on Unix. Setting this value on some versions of
XEmacs, e.g., Windows, seems to cause premature timeouts.  If you keep
getting timeout errors no matter how large a value you set, try
setting this variable to no timeout (nil)."
  :group 'bsh
  :type '(choice (const :tag "No timeout" :value nil)
                 (number :tag "Length")))

;; (makunbound 'bsh-eval-timeout)
(defcustom bsh-eval-timeout 30
  "*Length of time in seconds Emacs waits for the Beanshell to
evaluate a Java expression before giving up and signaling an
error. Setting this value on some versions of XEmacs, e.g., Windows,
seems to cause premature timeouts. If you keep getting timeout errors
no matter how large a value you set, try setting this variable to no
timeout (nil)."
  :group 'bsh
  :type '(choice (const :tag "No timeout" :value nil)
                 (number :tag "Length")))

(defcustom bsh-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java vm that runs the BeanShell. Note that the value of this
variable should be a list of strings, each of which represents an
argument. When customizing this variable, use a separate text field
for each argument."
  :group 'bsh
  :type '(repeat (string :tag "Argument")))

(defcustom bsh-startup-directory ""
  "Path of directory in which to start the beanshell.
The path may start with a tilde (~) indication your
home directory and may include environment variables.
If this variable is the null string (the default), 
the beanshell starts in the directory of the current
buffer."
  :group 'bsh
  :type 'directory)


(defun bsh()
  "*Starts BeanShell, a Java interpreter developed by Pat Niemeyer."
  (interactive)
  (bsh-internal t))

(defun bsh-running-p ()
  "Returns t if the beanshell is running."
  (if (comint-check-proc "*bsh*") t))

(defun bsh-internal (&optional display-buffer) 
  (let ((bsh-buffer-name "*bsh*"))
    (if (not (comint-check-proc bsh-buffer-name))
	(let* ((bsh-buffer (get-buffer-create bsh-buffer-name))
               (ant-home (if (string= jde-ant-home "") (getenv "ANT_HOME")
                           jde-ant-home)) 
	       (jde-java-directory
		(concat
		 (jde-find-jde-data-directory)
		 "java/"))
	       (vm (oref (jde-run-get-vm) :path))
	       (vm-args
		(list
		 "-classpath"
		 (jde-build-classpath
		  (delq 
		   nil
		   (append
		    (list
		     (expand-file-name "bsh-commands" jde-java-directory)
		     (let ((tools 
			    (expand-file-name "lib/tools.jar" (jde-get-jdk-dir))))
		       (if (file-exists-p tools)
			   tools
			 (error "Cannot find JDK's tools jar file. See jde-get-jdk-dir.")))
		     (if ant-home (expand-file-name "lib/ant.jar" ant-home))
		     (expand-file-name "lib/checkstyle-all.jar" jde-java-directory)
		     (expand-file-name "lib/jakarta-regexp.jar" jde-java-directory)
		     (if jde-devel-debug
			 (expand-file-name "classes" jde-java-directory)
		       (expand-file-name "lib/jde.jar" jde-java-directory))
		     (expand-file-name "lib/bsh.jar" jde-java-directory))
		    (jde-get-global-classpath)))
		  'jde-global-classpath)))
	       (dir (cond
		     ((not (string= bsh-startup-directory ""))
		      (jde-normalize-path 'bsh-startup-directory))
		     ((buffer-file-name)
		      (file-name-directory (buffer-file-name)))
		     (t
		      default-directory))))

	  (setq vm-args (append vm-args bsh-vm-args))
	  (setq vm-args (append vm-args (list "bsh.Interpreter")))

	  (save-excursion
	    (set-buffer bsh-buffer)
	    (erase-buffer)

	    (cd dir)
	    (insert (concat "cd " dir "\n"))
	    (insert 
	     (concat vm " "
		     (mapconcat (lambda (x) x) vm-args " ") "\n\n"))

	    (comint-mode)
	    (setq comint-prompt-regexp "bsh % "))

	  (message "%s" "Starting the BeanShell. Please wait...")

	  (let ((win32-start-process-show-window t)
		(w32-start-process-show-window t)
		(w32-quote-process-args ?\") ;; Emacs
		(win32-quote-process-args ?\") ;; XEmacs
		(windowed-process-io t)
		;; XEmacs addition
		(coding-system-for-read
		 (if (or (eq system-type 'cygwin32)
			 (eq system-type 'windows-nt))
		     'raw-text-dos)))
 	    (comint-exec bsh-buffer "bsh" vm nil vm-args))

	  (let ((bsh-process (get-buffer-process bsh-buffer)))
	    (process-kill-without-query bsh-process))

	  (if display-buffer
	      (pop-to-buffer bsh-buffer-name)))
      (when display-buffer
	(message "The Java interpreter is already running.")
	(pop-to-buffer bsh-buffer-name)))))


(setq bsh-tq-reply nil)

(defun bsh-eval-filter (process result)
  (let ((end-of-result (string-match ".*bsh % " result)))
    ;; Check for case
    ;;   %bsh\n...eval output...%bsh\n
    ;; This can happen because the beanshell outputs two or more
    ;; prompts after evaluating some expressions.
    ;; Thanks to Stephane Nicolas.
    ;; (if (eq end-of-result 0)
    ;; (accept-process-output process 0 5))
    (if end-of-result
        (setq bsh-tq-reply (concat bsh-tq-reply
                                   (substring result 0 end-of-result)))
      (setq bsh-tq-reply (concat bsh-tq-reply result))
      (accept-process-output process bsh-eval-timeout 5))))

(defun bsh-get-process () 
  "Gets the beanshell process if it exists. If it does not, it creates one."
  (if (get-process "bsh")
      (get-process "bsh")
    (let (proc)
      (bsh-internal)
      (setq proc (get-process "bsh"))
      (if (eq system-type 'windows-nt)
          (accept-process-output proc bsh-startup-timeout 0)
        (while (accept-process-output proc bsh-startup-timeout 0)))
      proc)))

(defun bsh-eval (expr &optional eval-return)
  "Uses the BeanShell Java interpreter to evaluate a Java statement.  If
the interpreter is not already running, this function starts the
interpreter. This function returns any text output by the Java
interpreter's standard out or standard error pipes.  If the optional
argument eval-return is non-nil, this function returns the result of
evaluating the Java output as a Lisp expression."
  (let* ((bsh-process (bsh-get-process))
	 (comint-filter (if bsh-process (process-filter bsh-process))))
    (when bsh-process
      (setq bsh-tq-reply nil)
      (set-process-filter bsh-process 'bsh-eval-filter)
      ;; (message "Evaluating: %s" expr)
      (process-send-string bsh-process (concat expr "\n"))

      (if (not (accept-process-output bsh-process bsh-eval-timeout))
	  (progn 
	    (set-process-filter bsh-process comint-filter)
	    (error "No reply from BeanShell")))
      
      (set-process-filter bsh-process comint-filter)
      (if (string-match "// Error:" bsh-tq-reply)
	  (progn
	    (message 
	     "Beanshell expression evaluation error.\n  Expression: %s\n  Error: %s"
	     expr bsh-tq-reply)
	    (error "Beanshell eval error. See messages buffer for details.")))
      ;; (if eval-return (message "Evaluating reply: %s" bsh-tq-reply))
      (if eval-return
	  (if bsh-tq-reply
	      (condition-case eval-error
		  (eval (read bsh-tq-reply))
		(error
		 (message "Error evaluating Lisp result of Java expression evaluation.")
		 (message "  Java expression: %s." expr)
		 (message "  Java evaluation result: %s." bsh-tq-reply)
		 (error "Error evaluating Java expresson. See *Messages* buffer.")))
	    (progn
	      (message "bsh-eval-r error: Beanshell result is null. Cannot evaluate.")
	      (message "  Expression: %s" expr)))
	bsh-tq-reply))))

(defun bsh-eval-r(java-statement) 
  "Convenience function for evaluating Java statements
that return Lisp expressions as output. This function 
invokes bsh-eval with the evaluate-return option set to
t."
  (bsh-eval java-statement t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beanshell commands    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun bsh-exit ()
  "Closes the existing beanshell process"
  (interactive)
  (if (get-process "bsh")
      (if (string= (car jde-ant-invocation-method) "Ant Server")
          (process-send-string (get-process "bsh") "jde.util.JdeUtilities.exit();\n")
        (process-send-string (get-process "bsh") "exit();\n"))
    (message "The beanshell is not running")))

(defun bsh-open-class-browser ()
  "Opens the beanshell class browser"
  (interactive)
  (bsh-eval "browseClass(\"\");"))

(defun bsh-open-desktop ()
  "Opens the beanshell desktop"
  (interactive)
  (bsh-eval "desktop();"))

(defclass bsh-compiler ()
  ((buffer    :initarg :buffer
	      :type buffer
	      :documentation
	      "Compilation buffer")
   (window    :initarg :window
	      :type window
	      :documentation
	      "Window that displays the compilation buffer."))
  "Class of beanshell expressions that emit Emacs compiler-compatible output.")

(defmethod bsh-compiler-create-buffer ((this bsh-compiler))
  (save-excursion
    (let ((buf (get-buffer-create "*compilation*"))
	  (error-regexp-alist compilation-error-regexp-alist)
	  (enter-regexp-alist 
	   (if (not (featurep 'xemacs)) compilation-enter-directory-regexp-alist))
	  (leave-regexp-alist 
	   (if (not (featurep 'xemacs)) compilation-leave-directory-regexp-alist))
	  (file-regexp-alist 
	   (if (not (featurep 'xemacs)) compilation-file-regexp-alist))
	  (nomessage-regexp-alist 
	   (if (not (featurep 'xemacs)) compilation-nomessage-regexp-alist))
	  (parser compilation-parse-errors-function)
	  (error-message "No further errors")
	  (thisdir default-directory))

      (oset this :buffer buf)

      (set-buffer buf)


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
      (if (not (featurep 'xemacs))
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
	    compilation-directory-stack (list default-directory)))))


(defmethod bsh-compiler-compile ((this bsh-compiler) compiler-expr)
  "Evaluates COMPILER-EXPR."
  (bsh-compiler-create-buffer this)

  ;; Pop to compilation buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if (not (featurep 'xemacs))
      (if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))
    

  (save-excursion
    (set-buffer (oref this :buffer))
    (insert (format "%s\n" compiler-expr))

    (if (not (bsh-running-p))
	(bsh-eval (jde-create-prj-values-str)))
	
    (let* ((output (bsh-eval 
		    compiler-expr
		    nil 
		    ;; (buffer-name (oref this :buffer))
		    ))
	   (len (length output))
	   (status "finished"
	    ;; (substring output (- len 2) (- len 1))
	    ))

      (delete-region (- (point-max) 2) (- (point-max) 1))
      (compilation-handle-exit 
       'exit status
       (if (string= "0" status)
	   "finished\n"
	 (format "exited abnormally with code %s\n"
		 status)))
      (set-buffer-modified-p nil)
      (setq compilation-last-buffer (oref this :buffer)))))
 

(defun bsh-test ()
  (interactive)
  (let* ((n1 (bsh-eval "jde.util.JdeUtilities.getQualifiedName(\"String\");"))
	 (n2 (bsh-eval "jde.util.JdeUtilities.getQualifiedName(\"Class\");")))
    (message "n1 = %s, n2 = %s" n1 n2)))
 


(provide 'beanshell)

;; $Log: beanshell.el,v $
;; Revision 1.47  2002/09/11 03:32:56  paulk
;; If jde-devel-debug is true, the BeanShell now uses the JDEE classes
;; in the jde/java/classes directory instead of those in the
;; jde/java/lib/jde.jar file. This simplifies testing changes
;; to the JDEE classes.
;;
;; Revision 1.46  2002/09/10 04:44:31  paulk
;; The JDEE now waits for the BeanShell startup message
;; on Windows. Previously, it returned possibly creating
;; subtle timing bugs.
;;
;; - Paul
;;
;; Revision 1.45  2002/08/27 05:03:30  paulk
;; Put the JDEE libraries ahead of jde-global-classpath in the beanshell classpath to ensure that the
;; JDEE versions of these libraries are loaded and not other versions that the user may have installed
;; in their classpath. Thanks to Andy Piper.
;;
;; Revision 1.44  2002/06/22 05:52:18  paulk
;; Fixed minor compilation error.
;;
;; Revision 1.43  2002/06/18 06:02:51  paulk
;; XEmacs compatibility fix: allow user to set bsh-startup-timeout
;; and bsh-eval-timeout to floating point values and to nil
;; (no timeout) to accommodate an apparent bug in the way
;; accept-process-output works on the Windows version of XEmacs.
;;
;; Revision 1.42  2002/06/12 07:04:31  paulk
;; XEmacs compatibility fix: set win32-quote-process-args wherever
;; the JDEE sets w32-quote-process-args. This allows use of spaces in
;; paths passed as arguments to processes (e.g., javac)  started by
;; the JDEE.
;;
;; Revision 1.41  2002/06/11 06:30:37  paulk
;; Provides support for paths containing spaces as beanshell vm arguments via the following change:
;; locally set the w32-quote-process-args variable to a quotation mark when launching
;; the beanshell vm process.
;;
;; Revision 1.40  2002/02/26 04:35:27  paulk
;; * Fixes regression bug that calls bsh-eval to fail when ant-home is nil.
;; * Adds a new eieo class, bsh-compiler, intended to serve as a base class
;;   for compiler-like tools (e.g., javac, ant, checkstyle) based on
;;   the beanshell.
;;
;; Revision 1.39  2002/02/25 20:07:38  jslopez
;; Remove no buffering filter.
;; Add get-process method.
;;
;; Revision 1.38  2002/02/21 12:42:33  jslopez
;; Fixes bug adding incorrect path for ant.jar in
;; the beanshell classpath.
;;
;; Revision 1.37  2002/02/15 17:50:46  jslopez
;; Removes reference to bsh-eval-comint-filter.
;; Switch the order of the sit-for and the accept-process-output.
;;
;; Revision 1.36  2002/02/15 02:48:20  jslopez
;; Adds a new non buffering filter, this filter is meant to be used
;; by the compile server and the ant server.
;; Modifies bsh-eval to support a new filter.
;;
;; Revision 1.35  2001/12/09 17:17:48  jslopez
;; Replaces repetitive code with jde-get-global-classpath.
;;
;; Revision 1.34  2001/11/05 14:18:32  jslopez
;; Modifies bsh-exit to use two different exit methods.
;; If Ant Server is enable it uses JdeUtilities.exit()
;; otherwise it uses the beanshell exit() method.
;;
;; Revision 1.33  2001/11/05 02:13:59  jslopez
;; Modified bsh-exit to class jde.util.JdeUtilities.exit() instead
;; of using the beanshell exit method.
;;
;; Revision 1.32  2001/10/24 05:27:56  paulk
;; Updated bsh-internal to use jde-run-get-vm (instead of the obsolete
;; jde-run-java-vm) to get the path to the vm to run the Beanshell.
;;
;; Revision 1.31  2001/10/19 09:47:55  paulk
;; XEmacs compatibility fix: Now correctly converts new lines (^M) on Windows.
;; Thanks to Andy Piper.
;;
;; Revision 1.30  2001/09/05 06:18:05  paulk
;; bsh-internal now uses jde-get-jdk-directory to determine the JDK directory.
;;
;; Revision 1.29  2001/08/30 04:15:06  paulk
;; Beanshell now uses jde-jdk-directory instead of jde-bug-jdk-directory (soon to be deprecated) to find the JDK tools.jar file.
;;
;; Revision 1.28  2001/08/30 01:34:47  paulk
;; Adds JDK tools.jar file to beanshell classpath. Needed to support compile server. Thanks to Javier Lopez.
;;
;; Revision 1.27  2001/08/14 06:11:35  paulk
;; Add bsh-exit, bsh-open-class-browser, and bsh-open-desktop. Thanks to Javier Lopez.
;;
;; Revision 1.26  2001/07/31 05:11:48  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.25  2001/06/13 03:51:44  paulk
;; Now defines bsh customization group.
;;
;; Revision 1.24  2001/05/31 05:14:38  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.23  2001/05/19 02:39:18  paulk
;; Put jde-global-classpath first on the classpath to facilitate debugging of Java code run in hthe Beanshell.
;;
;; Revision 1.22  2001/04/16 05:33:20  paulk
;; Normalized paths. Thanks to Nick Sieger.
;;
;; Revision 1.21  2001/03/21 20:46:34  paulk
;; Updated bsh-internal to handle case where both jde-global-classpath and CLASSPATH environment variable are nil. Thanks to Toru TAKAHASHI <torutk@alles.or.jp> for reporting this bug and supply an initial version of a fix.
;;
;; Revision 1.20  2001/03/01 05:01:28  paulk
;; Adds the customization variable bsh-startup-directory.
;;
;; Revision 1.19  2001/02/25 04:23:12  paulk
;; Fixed bug in processing CLASSPATH environment variable.
;;
;; Revision 1.18  2001/02/03 07:44:26  paulk
;; Now uses jde-build-classpath to build BeanShell classpath. This allows enviromnent variables in the classpath.
;;
;; Revision 1.17  2000/10/08 12:55:38  paulk
;; *** empty log message ***
;;
;; Revision 1.16  2000/08/10 09:09:47  paulk
;; Now handles Lisp eval errors gracefully.
;;
;; Revision 1.15  2000/08/07 05:11:38  paulk
;; Adds bsh-vm-args variable.
;;
;; Revision 1.14  2000/08/04 02:51:19  paulk
;; Added bsh-eval-timeout variable.
;;
;; Revision 1.13  2000/02/16 04:39:28  paulk
;; Implemented Cygwin/XEmacs compatiblity fix provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.12  2000/02/02 05:51:00  paulk
;; Expanded doc string.
;;
;; Revision 1.11  2000/01/28 04:28:00  paulk
;; Fixed startup timing bug that cause commands that use the beanshell to
;; failt the first time on Unix systems.
;;
;; Revision 1.10  2000/01/15 08:00:03  paulk
;; Corrected typo.
;;
;; Revision 1.9  1999/11/01 03:13:07  paulk
;; No change.
;;
;; Revision 1.8  1999/09/17 06:55:26  paulk
;; Set comint-prompt-regexp to the beanshell prompt.
;; Fixed bug where Emacs was querying user whether to kill the beanshell
;; buffer on exit from Emacs.
;;
;; Revision 1.7  1999/01/15 22:18:41  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.6  1998/12/13 22:10:04  paulk
;; Add check for chunked traffic between Emacs and the BeanShell.
;;
;; Revision 1.5  1998/12/09 00:59:43  paulk
;; Added a startup message for beanshell.
;;
;; Revision 1.4  1998/11/27 10:07:57  paulk
;; Use CLASSPATH environment variable if jde-global-classpath is nil.
;;
;; Revision 1.3  1998/11/22 23:14:28  paulk
;; Fixed path separator bug.
;;
;; Revision 1.2  1998/11/22 18:11:56  paulk
;; Changed path to use jde.jar.
;;
;; Revision 1.1  1998/10/22 00:07:56  paulk
;; Initial revision
;;


;; End of beanshell.el
