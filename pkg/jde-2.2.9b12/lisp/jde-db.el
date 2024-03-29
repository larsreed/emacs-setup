;;; jde-db.el -- Debugger mode for jdb.
;; $Revision: 1.111 $ $Date: 2002/08/27 04:19:01 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 2000, 2001, 2002 Paul Kinnucan.

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
;; Boston, MA 02111-1307, US
;;; Commentary:

;; This package interfaces emacs to jdb, the debugger
;; distributed as part of JavaSoft's Java
;; Development Kit (JDK).

;; Please send bug reports and enhancement suggestions
;; to Paul Kinnucan at <paulk@mathworks.com>

;; See end of this file for change history.

;;; Code:

(require 'jde-parse)
(require 'eieio)
(require 'jde-util)


;; ======================================================================
;; jde-db variables

	   
(defcustom jde-db-query-missing-source-files t
  "If nonnil, this variable causes the debugger to query you
for the path of a class source file that it cannot find in
`jde-sourcepath'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-db-mode-hook nil
  "*Customization hook for jde-db inferior mode."
  :group 'jde-project
  :type 'hook
)

(defcustom jde-db-initial-step-p t
  "*If non-nil, this option causes the debugger
to issue a step-into command after launching
a program. This causes the vm to step to the
first line of the debuggee program."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-db-startup-commands nil
  "*Commands to run at debugger startup."
  :group 'jde-project
  :type '(repeat (string :tag "Command")))

(defcustom jde-db-read-vm-args nil
"*Read vm arguments from the minibuffer.
If this variable is non-nil, the jde-db command reads vm arguments
from the minibuffer and appends them to those specified by
the `jde-db-option' variable group."
  :group 'jde-project
  :type 'boolean)

(defvar jde-db-interactive-vm-arg-history nil
"History of vm arguments read from the minibuffer")

(defcustom jde-db-read-app-args nil
"*Read arguments to be passed to application from the minibuffer."
  :group 'jde-project
  :type 'boolean)

(defvar jde-db-interactive-app-arg-history nil
"History of application arguments read from the minibuffer")

(defcustom jde-db-classic-mode-vm nil
"Runs applications in the classic (i.e., not HotSpot) mode when
debugging."
  :group 'jde-project
  :type 'boolean)

(defgroup jde-db-options nil
  "JDE Debugger Options"
  :group 'jde
  :prefix "jde-db-option-")

(defcustom jde-db-option-classpath nil
"*Specify paths of classes required to run this application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-db-options
  :type '(repeat (file :tag "Path")))
 
(defcustom jde-db-option-verbose (list nil nil nil)
  "*Print messages about the running process.
The messages are printed in the run buffer."
  :group 'jde-db-options
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

(defcustom jde-db-option-properties nil
  "*Specify property values.
Enter the name of the property, for example, awt.button.color, in the
Property Name field; enter its value, for example, green, in the
Property Value field. You can specify as many properties as you like."
  :group 'jde-db-options
  :type '(repeat (cons 
		  (string :tag "Property Name") 
		  (string :tag "Property Value"))))

(defcustom jde-db-option-heap-size (list
				    (cons 1 "megabytes")
				    (cons 16 "megabytes"))
"*Specify the initial and maximum size of the interpreter heap."
:group 'jde-db-options
:type '(list
	(cons (integer :tag "Start")
	     (radio-button-choice (const "bytes")
				  (const "kilobytes")
				  (const "megabytes")))
	(cons (integer :tag "Max")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))


(defcustom jde-db-option-stack-size (list
				     (cons 128 "kilobytes")
				     (cons 400 "kilobytes"))
  "*Specify size of the C and Java stacks."
  :group 'jde-db-options
  :type '(list
	  (cons (integer :tag "C Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))
	  (cons (integer :tag "Java Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))

(defcustom jde-db-option-garbage-collection (list t t)
  "*Specify garbage collection options."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect garbage asynchronously.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect unused classes.")))

(defcustom jde-db-option-java-profile (cons nil "./java.prof")
  "*Enable Java profiling."
  :group 'jde-db-options
  :type '(cons boolean
	       (file :tag "File"
		     :help-echo 
"Specify where to put profile results here.")))

(defcustom jde-db-option-heap-profile (cons nil
					    (list "./java.hprof"
						  5
						  20
						  "Allocation objects"))
"*Output heap profiling data."
  :group 'jde-db-options
  :type '(cons boolean
	       (list
		(string :tag "Output File Path")
		(integer :tag "Stack Trace Depth")
		(integer :tag "Allocation Sites")
		(radio-button-choice :format "%t \n%v"
				     :tag "Sort output based on:"
		 (const "Allocation objects")
		 (const "Live objects")))))
		 
(defcustom jde-db-option-verify (list nil t)
  "*Verify classes."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Executed code in all classes.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Classes loaded by a classloader.")))

(defcustom jde-db-option-host ""
  "Host of a remote process to which you wish to attach. This
option is invalid for JDK verions greater than JDK 1.1.x."
  :group 'jde-db-options
  :type 'string)

;; (makunbound 'jde-db-option-connect-address)
(defcustom jde-db-option-connect-address nil
  "Specify address used to connect the debugger to a running process.
If nil, this option defaults to \"javadebug\" on Windows systems
and \"4444\" on UNIX systems."
  :group 'jde-db-options
  :type '(choice
	  (const :menu-tag "Use Default" nil)
	  (string :menu-tag "Specify Value" :tag "value")))


(defcustom jde-db-option-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java interpreter. It is an alternative to using JDE Run Option
variables, such as `jde-run-option-stack-size', to specify Java
interpreter options. Also, it makes it possible to use the JDE with
interpreters that accept command line arguments not supported by 
the JDE Run Option variable set."
  :group 'jde-db-options
  :type '(repeat (string :tag "Argument")))


(defcustom jde-db-option-application-args nil
  "*Specify command-line arguments to pass to the application.
The JDE passes the specified arguments to the application on
the command line."
  :group 'jde-db-options
  :type '(repeat (string :tag "Argument")))

(defmacro jde-assert-source-or-debug-buffer ()
  "Asserts that the current buffer is a
Java source or a debug buffer."
  '(assert 
    (or
     (eq major-mode 'jde-mode)
     (and (slot-boundp 'jde-db-debugger 'the-debugger)
	  (eq (current-buffer) 
	      (oref (oref 'jde-db-debugger the-debugger) buffer))))
    nil 
    "This command works only in a Java source or debug buffer."))

(defcustom jde-db-log-debugger-output-flag nil
  "Log raw debugger output to a buffer. This variable is intended
to be used for debugging the JDEE's debuggers."
  :group 'jde-db-options
  :type 'boolean)

(defun jde-db-log-debugger-output (output)
  (if jde-db-log-debugger-output-flag
      (let ((buf (get-buffer "debugger output")))
	(when (not buf)
	  (setq buf (get-buffer-create  "debugger output"))
	  (pop-to-buffer buf))
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-max))
	  (insert-string output)))))		  

(defun jde-db-get-debuggee-status ()
  "Get the`jde-db-debuggee-status' of the 
current debuggee process."
  (if (slot-boundp 'jde-db-debugger 'the-debugger)
      (let* ((debugger (oref 'jde-db-debugger the-debugger))
	     (debuggee (oref debugger debuggee)))
	(oref debuggee status))))


(defun jde-db-debuggee-stopped-p ()
  "Return t if current debuggee process is stopped."
  (let ((status (jde-db-get-debuggee-status)))
     (if status
	 (oref status stopped-p))))

(defun jde-db-debuggee-suspended-p ()
  "Return t if current debuggee process is suspended."
  (let ((status (jde-db-get-debuggee-status)))
     (if status
	 (oref status suspended-p))))

(defun jde-db-debuggee-running-p ()
  "Return t if current debuggee process is running."
  (let ((status (jde-db-get-debuggee-status)))
     (if status
	 (oref status running-p))))


;;;###autoload
(defun jde-db-set-debugger (name is-executable)
  "Specify the pathname of the debugger, if an executable, or the
debugger's fully qualified class name, if a class."
  (interactive
   "sEnter name of Java interpreter: \nsIs %s executable? (yes): ")
  (let ((db name)
	(type
	 (if (stringp is-executable)
	     (if (or
		  (string= is-executable "")
		  (eq (aref is-executable 0) ?y))
		 "Executable"
	       "Class")
	   "Executable")))
    (setq jde-db-debugger (cons "Other" (cons db type)))))

;;;###autoload
(defun jde-db-set-args (args)
  "Specify the arguments (except -classpath) to be passed to the debugger."
  (interactive 
   "sEnter arguments: ")
  (setq jde-db-option-vm-args (jde-run-parse-args args)))

;;;###autoload
(defun jde-db-set-app-args (args)
  "Specify the arguments to be passed to the Java application class."
  (interactive 
   "sEnter arguments: ")
  (setq jde-db-option-application-args (jde-run-parse-args args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoints                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom jde-db-spec-breakpoint-face-colors (cons "black" "green")
"*Specifies the foreground and background colors used to highlight
the line at which you have specified that a breakpoint to be set."
  :group 'jde-project
  :type '(cons :tag "Colors"
	  (string :tag "Foreground") 
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'jde-db-spec-breakpoint-face)
	  (set-face-foreground 'jde-db-spec-breakpoint-face (car val))
	  (set-face-background 'jde-db-spec-breakpoint-face (cdr val))
	  (set-default sym val)))

(defcustom jde-db-requested-breakpoint-face-colors (cons "black" "yellow")
"*Specifies the foreground and background colors used to highlight
the line at which you have requested a breakpoint to be set."
  :group 'jde-project
  :type '(cons :tag "Colors"
	  (string :tag "Foreground") 
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'jde-db-requested-breakpoint-face)
	  (set-face-foreground 'jde-db-requested-breakpoint-face (car val))
	  (set-face-background 'jde-db-requested-breakpoint-face (cdr val))
	  (set-default sym val)))

(defcustom jde-db-active-breakpoint-face-colors (cons "black" "red")
"*Specifies the foreground and background colors used to highlight
a line where an active breakpoint exists."
  :group 'jde-project
  :type '(cons :tag "Colors"
	  (string :tag "Foreground") 
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'jde-db-active-breakpoint-face)
	  (set-face-foreground 'jde-db-active-breakpoint-face (car val))
	  (set-face-background 'jde-db-active-breakpoint-face (cdr val))
	  (set-default sym val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Marker Class                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-db-breakpoint-marker ()
  ((marker :initarg :marker
	   :documentation
	   "Overlay in Emacs, extent in XEmacs"))
  "Indicates the location of breakpoints in a source buffer. This class
uses overlays as markers in Emacs and extents in XEmacs.")

(defmethod initialize-instance ((this jde-db-breakpoint-marker) &rest fields)
  "Create a breakpoint overlay at LINE in FILE."
  
  ;; Call parent initializer.
  (call-next-method)

  (oset this marker
	(if (featurep 'xemacs)
	    (if (or (not (extent-at (line-beginning-position)))
		    (not (jde-db-breakpoint-marker-p
			   (extent-at (line-beginning-position)))))
		(make-extent
		 (line-beginning-position)
		 (1+ (line-end-position))))
	  (make-overlay
	   (line-beginning-position)
	   (1+ (line-end-position))
	   (current-buffer) nil t))))


(defmethod jde-db-breakpoint-marker-set-face ((this jde-db-breakpoint-marker) face)
  "Apply FACE to OVERLAY."
  (let ((marker (oref this marker)))
    (if (featurep 'xemacs)
	(progn
	  (set-extent-face marker face)
	  (set-extent-priority marker 98))
      (progn
	(overlay-put marker  'face face)
	(overlay-put marker 'priority 98)))))

(defun jde-db-breakpoint-marker-p (marker)
  "Return t if overlay is a breakpoint marker overlay."
    (let ((marker-face 
	   (if (featurep 'xemacs)
	       (extent-property marker 'face nil)
	     (overlay-get marker 'face))))
      (or
       (eq marker-face 'jde-db-spec-breakpoint-face)
       (eq marker-face 'jde-db-requested-breakpoint-face)
       (eq marker-face 'jde-db-active-breakpoint-face))))

(defmethod jde-db-breakpoint-marker-delete ((this jde-db-breakpoint-marker))
  "Remove breakpoint overlay at LINE in FILE."
  (if (featurep 'xemacs)
      (delete-extent (oref this marker))
    (delete-overlay (oref this marker))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Class                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-breakpoint ()
  ((id       :initarg :id
	     :type integer
	     :documentation
	     "Identifies this breakpoint.")
   (file     :initarg :file
	     :initform ""
	     :type string
	     :documentation
	     "Pathname of file containing this breakpoint.")
   (line     :initarg :line
	     :type integer
	     :documentation
	     "Number of line at which breakpoint is set.")
   (marker   :initarg :marker
	     :type (or null jde-db-breakpoint-marker)
	     :initform nil
	     :documentation
	     "Marker used to highlight breakpoint line.")
   (class    :initarg :class
	     :type string
	     :documentation
	     "Qualified name of class containing breakpoint.")
   (status   :initarg status
	     :type symbol
	     :initform specified
	     :documentation
	     "Status of this breakpoint. Legal values are `specified', `requested', `active'."))
  (:allow-nil-initform t)
  "Class of breakpoints.")


(defmethod initialize-instance ((this jde-db-breakpoint) &rest fields)
  "Constructor for a breakpoint specification."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this file))

  (oset this 
	marker 
	(jde-db-breakpoint-marker "breakpoint marker"))
  
  (jde-db-breakpoint-marker-set-face 
   (oref this marker) 'jde-db-spec-breakpoint-face))

(defmethod jde-db-breakpoint-get-line ((this jde-db-breakpoint))
  "Get the number of the line at which this breakpoint is set."
  (save-excursion
    (set-buffer (find-file-noselect (oref this file)))
    (if (oref this marker)
	(let ((marker-start
	       (if (featurep 'xemacs)
		   (extent-start-position (oref (oref this marker) marker))
		 (overlay-start (oref (oref this marker) marker)))))
	  (jde-get-line-at-point marker-start))
      (oref this line))))

(defvar jde-db-breakpoints nil
"Current breakpoints.")


(defun jde-db-get-breakpoint-marker (file line)
  "Get breakpoint marker at FILE and LINE."
  (let ((bp (jde-db-find-breakpoint file line)))
    (if bp
	(oref bp marker))))

(defun jde-db-mark-breakpoint-specified (file line)
  "Changes the face of the breakpoint marker at LINE in FILE 
to the specified face."
  (let ((marker (jde-db-get-breakpoint-marker file line)))
    (if marker
	(jde-db-breakpoint-marker-set-face marker 'jde-db-spec-breakpoint-face))))

(defun jde-db-mark-breakpoint-active (file line)
  "Changes the face of the breakpoint marker at LINE in FILE 
to the active face."
  (let ((marker (jde-db-get-breakpoint-marker file line)))
    (if marker
	(jde-db-breakpoint-marker-set-face marker 'jde-db-active-breakpoint-face))))

(defun jde-db-mark-breakpoint-requested (file line)
  "Changes the face of the breakpoint marker at LINE in FILE 
to the active face."
  (let ((marker (jde-db-get-breakpoint-marker file line)))
    (if marker
	(jde-db-breakpoint-marker-set-face marker 'jde-db-requested-breakpoint-face))))

(defun jde-db-set-all-breakpoints-specified ()
  "Changes the face of all breakpoints to `jde-db-spec-breakpoint-face'
and sets the status of all breakpoints to `specified'."
  (loop for bp-assoc in jde-db-breakpoints do
	(let* ((bp (cdr bp-assoc))
	       (marker (oref bp marker)))
	  (oset bp status 'specified)
	  (if marker
	      (jde-db-breakpoint-marker-set-face marker 'jde-db-spec-breakpoint-face)))))

(defun jde-db-delete-breakpoint (bp)
  "Delete the breakpoint at LINE in FILE."
  (setq jde-db-breakpoints
	(remove-if
	 (lambda (assoc)
	   (let* ((xbp (cdr assoc))
		  (xfile (oref xbp file))
		  (xline (jde-db-breakpoint-get-line  xbp))
		  (deletep
		   (and
		    (string= (oref bp file) xfile)
		    (equal (jde-db-breakpoint-get-line bp) xline))))
	     (if deletep
		 (jde-db-breakpoint-marker-delete
		  (oref bp marker)))
	     deletep))
	 jde-db-breakpoints)))

(defun jde-db-clear-breakpoints ()
  "Clear all breakpoints from all buffers."
  (mapc
   (lambda (assoc)
     (let* ((xbp (cdr assoc))
	    (file (oref xbp file))
	    (buf (find-buffer-visiting file)))
       (if buf
	   (save-excursion
	     (set-buffer buf)
	     (let ((xmarker (oref xbp marker)))
	       (jde-db-breakpoint-marker-delete xmarker))))))
      jde-db-breakpoints)
  (setq jde-db-breakpoints nil))


(defun jde-db-breakpoints-add (bp)
  "Adds this breakpoint to the list of breakpoints."
  (setq jde-db-breakpoints 
	(cons (cons (oref bp id) bp) 
	      jde-db-breakpoints)))


(defun jde-db-find-breakpoint (file line)
  "Finds the breakpoint object for the breakpoint at FILE and LINE."
  (cdr (find-if 
	(lambda (assoc)
	  (let ((bp (cdr assoc)))
	       (and (string= (oref bp file) file)
		    (equal (jde-db-breakpoint-get-line bp) line))))
	jde-db-breakpoints)))


(defvar jde-db-breakpoint-id-counter 0
"Counter for generating breakpoint ids")

(defun jde-db-nullify-breakpoint-markers ()
  "Set the marker field for each breakpoint
in the current buffer to nil."
 (when (eq major-mode 'jde-mode)
   (let ((file (buffer-file-name)))
     (loop for bp-assoc in jde-db-breakpoints do
	   (let ((bp (cdr bp-assoc)))
	     (when (string= (oref bp file) file)
	       (oset bp line (jde-db-breakpoint-get-line bp))
	       (oset bp marker nil)))))))

(add-hook 'kill-buffer-hook 'jde-db-nullify-breakpoint-markers)

(defun jde-db-remark-breakpoints ()
  "Highlights all breakpoints in the current buffer if not 
already highlighted."
  (save-excursion
    (loop for bp-assoc in jde-db-breakpoints do
	  (let* ((bp (cdr bp-assoc))
		 (file (buffer-file-name))
		 (line (oref bp line))
		 (status (oref bp status)))
	    (goto-line line)
	    (oset bp
		  marker 
		  (jde-db-breakpoint-marker "breakpoint marker"))
	    (cond
	     ((eq status 'specified)
	      (jde-db-mark-breakpoint-specified file line))
	     ((eq status 'requested)
	      (jde-db-mark-breakpoint-requested file line))
	     ((eq status 'active)
	      (jde-db-mark-breakpoint-active file line))
	     (t
	      (error "Unknown breakpoint status: %s" (symbol-name status))))))))
	  

(add-hook 'jde-mode-hook 'jde-db-remark-breakpoints)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debug Cursor Handling                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-db-query-source-file (class)
  (let ((source-file
	 (read-file-name 
	  (format "Cannot find %s source. Enter path: " class))))
  (if (and
       source-file
       (file-exists-p source-file)
       (not (file-directory-p source-file)))
      (find-file-noselect source-file))))

(defun jde-db-find-class-source (class)
  "Find and open the source file for a class. CLASS is the fully
qualified name of the class. If this function is unable to find the
source for CLASS in `jde-sourcepath' and
`jde-db-query-missing-source-files' is nonnil, this function queries
the user for the path to the source file. If successful, this function
returns an unselected buffer containing the source file for the
class. Otherwise, it returns nil."
  (let* ((source-file (jde-find-class-source-file class))
	 (source-buffer
	  (if source-file	  
	      (find-file-noselect source-file)
	    (if jde-db-query-missing-source-files
		(jde-db-query-source-file class)))))
    source-buffer))

(defun jde-db-set-debug-cursor (class file line)
  "Shows the source at LINE in CLASS."
  (let* ((buffer (jde-db-find-class-source class))
	 (window 
	  (and buffer
	       (or (get-buffer-window buffer)
		   (selected-window))))
	  pos) 
    (if buffer
	(progn
	  (if (not (get-buffer-window buffer))
	      (set-window-buffer window buffer))
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (setq pos (point))
	      (setq overlay-arrow-string "=>")
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))	 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debuggee Process Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-debuggee-status ()
  ((running-p   :initarg :running-p
		:type boolean
		:initform nil
		:documentation
		"Non-nil if debuggee process is running.")
   (stopped-p   :initarg :stopped-p
		:type boolean
		:initform nil
		:documentation
		"Non-nil if debugee process is stopped.")
   (suspended-p :initarg :suspended-p
		:type boolean
		:initform nil
		:documentation
		"Non-nil if debugee process is suspended."))
  "Status of debuggee process.")

(defmethod initialize-instance ((this jde-db-debuggee-status) &rest fields)
  "Status of debuggee process."
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debuggee Process Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-debuggee ()
  ((main-class  :initarg :main-class
		:type string
		:documentation
		"Qualified name of debuggee main class.")
   (address     :initarg :address
		:type (or null string)
		:initform nil
		:documentation
		"String that identifies the vm to be debugged.")
   (status      :initarg :status
		:type jde-db-debuggee-status
		:documentation
		"Status of debuggee process.")
   (stack-depth :initarg :stack-depth
		:type string
		:initform ""
		:documentation
		"Stack depth."))
  "Application being debugged.")

(defmethod initialize-instance ((this jde-db-debuggee) &rest fields)
  "Constructs an instance of a debuggee."
  (call-next-method)
  (assert (or (slot-boundp this 'main-class)
	      (slot-boundp this 'address)))
  (oset  this  status  (jde-db-debuggee-status "debuggee status")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debugger Command Line Commands                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-cmd ()
  ((name           :initarg :name
                   :type string
	           :documentation
	           "Name of command.")
   (debugger       :initarg :debugger
		   :type jde-db-debugger
		   :documentation
		   "Debugger."))
  "Super class of debugger commands.")
 

(defmethod initialize-instance ((this jde-db-cmd) &rest fields)
  "Constructor for debugger commands."
  (call-next-method))

(defmethod jde-db-cmd-init ((this jde-db-cmd))
  "The debugger invokes this method before executing the 
command.")

(defmethod jde-db-cmd-make-command-line ((this jde-db-cmd))
  "Creates the command line for this command."
  (oref this name))
    
(defmethod jde-db-cmd-notify-response ((this jde-db-cmd) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response.")

(defmethod jde-db-cmd-response-p ((this jde-db-cmd) output)
  "Returns nonnil if external debugger's output is a 
response to this command."
  t)


(defclass jde-db-cmd-breakpoint (jde-db-cmd)
  ((breakpoints :initarg :breakpoints
	        :type list
	        :documentation
	        "List of breakpoint specification."))
  "Class of breakpoint commands.")
  
;; Generic Debugger Command Set.

(defclass jde-db-cmd-set ()
  ((debugger          :initarg :debugger
		      :type jde-db-debugger
		      :documentation
		      "Debugger that owns this command set.")
   (launch            :initarg :launch
		      :type jde-db-cmd
		      :documentation
		      "Launch debuggee application")
   (run               :initarg :run
		      :type jde-db-cmd
		      :documentation
		      "Starts the current debuggee application.")
   (cont              :initarg :cont
		      :type jde-db-cmd
		      :documentation
		      "Continues the current debuggee application.")
   (quit              :initarg :quit
		      :type jde-db-cmd
		      :documentation
		      "Quit debugging the current application.")
   (step-over         :initarg :step-over
		      :type jde-db-cmd
		      :documentation
		      "Step to the next line in the current frame.")
   (step-into         :initarg :step-into
		      :type jde-db-cmd
		      :documentation
		      "Step to the next line in the current program.")
   (step-out          :initarg :step-out
		      :type jde-db-cmd
		      :documentation
		      "Continue to the end of the current method.")
   (up                :initarg :up
		      :type jde-db-cmd
		      :documentation
		      "Move up the stack.")
   (down              :initarg :down
		      :type jde-db-cmd
		      :documentation
		      "Move down the stack.")
   (where             :initarg :where
		      :type jde-db-cmd
		      :documentation
		      "Point to the current stopping point.")
   (set-bp            :initarg :set-bp
		      :type jde-db-cmd
		      :documentation
		      "Cmd that asks debugger to set a breakpoint.")
   (clear-bp          :initarg :clear-bp
		      :type jde-db-cmd
		      :documentation
		      "Cmd that asks debugger to set a breakpoint."))
  "Set of debugger commands implemented by this debugger.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debug Process Listener                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-listener ()
  ((debugger   :initarg :debugger
	       :type jde-db-debugger
	       :documentation
	       "The debugger"))
  "Listens to the output from the debugger.")

(defmethod jde-db-listener-filter-output ((this jde-db-listener) output)
  "Filters the output of the debugger."
  output)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Class of JDE Debuggers                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-debugger ()
  ((name          :initarg :name
	          :type string
		  :initform "Java debugger"
	          :documentation 
	          "Name of this Java debugger.")
   (buffer-name   :initarg :buffer-name
                  :initform "Java Debugger"
		  :type string
		  :documentation
		  "Name of buffer used to interact with debugger.")
   (buffer        :initarg :buffer
		  :type buffer
		  :documentation
		  "Buffer used to interact with debugger.")

   (process       :initarg :process
		  :documentation 
		  "Debugger process.")

   (running-p     :initarg :process
		  :type boolean
		  :initform nil
		  :documentation
		  "Non-nil if debugger process is running.")
  
   (proc-filter   :initarg :proc-filter
		  :type function
		  :documentation
		  "Function used to parse debug output.")

   (listeners     :initarg :listeners
		  :type list
		  :initform nil
		  :documentation
		  "List of debugger output listeners.")

   (cmd-set       :initarg :cmd-set
		  :type jde-db-cmd-set
		  :documentation
		  "Commands implemented by this debugger.")

   (next-cmd      :initarg :next-cmd
		  :type list
		  :initform nil
		  :documentation
		  "Next command(s) to execute.")

   (last-cmd      :initarg :last-cmd
		  :type (or null jde-db-cmd)
		  :documentation
		  "Last command send to the debugger.")

   (debuggee      :initarg :debuggee
		  :type jde-db-debuggee
		  :documentation
		  "Application process being debugged.")

   (the-debugger  :type jde-db-debugger
		  :allocation :class
		  :documentation
		  "The currently active debugger."))
   "Class of Java debuggers.")

(defmethod initialize-instance ((this jde-db-debugger) &rest fields)
  "Constructor for generic debugger."
  (oset this cmd-set 
	(jde-db-cmd-set "Generic commands" :debugger this))
  (oset this last-cmd nil))


(defmethod jde-db-ready-p ((this jde-db-debugger) output)
  "Nonnil if OUTPUT indicates that the debugger is 
ready to accept the next command."
  (and output
       (or
	(string-match ">[ ]*$" output)
	(string-match "[a-zA-Z0-9]+\[[0-9]+\][ ]*$" output)
	(string-match "VM Started:[ ]*$" output))))


(defmethod jde-db-process-debugger-output ((this jde-db-debugger) output)
  "Process debugger output."
  (jde-db-log-debugger-output (concat "<<" output ">>"))
  (let ((proc (oref this process))
	(listeners (oref this listeners))
	(response output)
	(last-cmd (oref this last-cmd)))

    (loop for listener in listeners do
	  (setq output
		(jde-db-listener-filter-output listener output)))

    (comint-output-filter proc output)

    (if last-cmd
	(jde-db-cmd-notify-response last-cmd response))

    (if (jde-db-ready-p this (car (last (split-string output "\n"))))
	(jde-db-exec-next-cmd this))))

(defmethod jde-db-add-listener ((this jde-db-debugger) listener)
  "Adds LISTENER to the list of listeners listening for response
from the debugger. LISTENER must be an object of type
`jde-db-listener'."
  (assert (typep listener jde-db-listener))
  (oset this listeners (cons listener (oref this listeners))))

(defmethod jde-db-remove-listener ((this jde-db-debugger) listener)
  "Removes LISTENER from the list of listeners listening for a
response from the debugger.  LISTENER must be an object of type
`jde-db-listener'."
  (assert (typep listener jde-db-listener))
  (oset this listeners (remove listener (oref this listeners))))

(defmethod jde-db-set-process-filter ((this jde-db-debugger))
  "Set debugger process output filter. The default method sets a
function that invokes `jde-db-process-debugger-output'."
  (set-process-filter 
   (oref this process) 
   (lambda (process output)
     (jde-db-process-debugger-output 
      (oref 'jde-db-debugger the-debugger) output))))

(defmethod jde-db-notify-process-exit ((this jde-db-debugger) msg)
  "The default debugger process sentinel invokes this method 
when the debugger process terminates."
  (let ((proc (oref this process)))
    (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	   (setq overlay-arrow-position nil)
	   (set-process-buffer proc nil))
	  ((memq (process-status proc) '(signal exit))
	   ;; Stop displaying an arrow in a source file.
	   (setq overlay-arrow-position nil)
	   (let* ((obuf (current-buffer)))
	     ;; save-excursion isn't the right thing if
	     ;;  process-buffer is current-buffer
	     (unwind-protect
		 (progn
		   ;; Write something in debugger buffer and hack its mode line,
		   (set-buffer (process-buffer proc))
		   ;; Fix the mode line.
		   (setq mode-line-process
			 (concat ":"
				 (symbol-name (process-status proc))))
		   (force-mode-line-update)
		   (if (eobp)
		       (insert ?\n mode-name " " msg)
		     (save-excursion
		       (goto-char (point-max))
		       (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the command buffer.
	     (set-buffer obuf)))))))


(defmethod jde-db-notify-process-status-change ((this jde-db-debugger) msg)
  "The debugger process sentinel invokes this method when the status of
the debugger process changes. The default method invokes 
`jde-db-notify-process-exit'."
  (jde-db-notify-process-exit this msg))


(defmethod jde-db-set-process-sentinel ((this jde-db-debugger))
  (set-process-sentinel
   (oref this process)
   (lambda (process msg)
       (jde-db-notify-process-status-change
	(oref 'jde-db-debugger the-debugger) msg))))

(defmethod jde-db-exec-next-cmd ((this jde-db-debugger))
  "Executes the next command on the debugger's pending
command list."
  (let ((curr-cmd (car (oref this next-cmd))))
    (if curr-cmd
	(progn
	  (oset this next-cmd (cdr (oref this next-cmd)))
	  (oset this last-cmd curr-cmd)
	  (jde-db-cmd-init curr-cmd)
	  (save-excursion
	    (set-buffer (oref this buffer))
	    (let ((proc (oref this process)))
	      (goto-char (point-max))
	      (insert (jde-db-cmd-make-command-line curr-cmd))
	      (comint-send-input)))))))

(defmethod jde-db-exec-cmds ((this jde-db-debugger) cmds)
  "Executes list of debugger CMDS."
  (oset this next-cmd cmds)
  (jde-db-exec-next-cmd this))

(defmethod jde-db-exec-cmd ((this jde-db-debugger) cmd)
  "Executes CMD."
  (assert (and cmd (typep cmd 'jde-db-cmd)))
  (jde-db-exec-cmds this (list cmd)))

(defmethod jde-db-classpath-arg ((this jde-db-debugger))
  "Generate the -classpath command line argument for jdb."

  ;; Set the classpath option. Use the local
  ;; classpath, if set; otherwise, the global
  ;; classpath.
  (let ((classpath
	 (if jde-db-option-classpath
	     jde-db-option-classpath
	   jde-global-classpath))
	(symbol
	 (if jde-db-option-classpath
	     'jde-db-option-classpath
	   'jde-global-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jde-build-classpath
	  classpath symbol)))))

(defmethod jde-db-classic-mode-arg ((this jde-db-debugger))
  "Generate the classic mode command-line argument for jdb."
  (if jde-db-classic-mode-vm
      (list "-classic")))

(defmethod jde-db-property-args ((this jde-db-debugger))
  "Generate property arguments."
  (if jde-db-option-properties
      (mapcar
       (lambda (prop)
	 (concat "-D" (car prop) "=" (cdr prop)))
       jde-db-option-properties)))


(defmethod jde-db-verbose-args ((this jde-db-debugger))
  "Get the debugger verbosity arguments for jdb."
  (let ((print-classes-loaded
	 (nth 0 jde-db-option-verbose))
	(print-memory-freed
	 (nth 1 jde-db-option-verbose))
	(print-jni-info
	 (nth 2 jde-db-option-verbose))
	options)

    (if print-classes-loaded
	(add-to-list 'options "-verbose:class"))

    (if print-memory-freed
	(add-to-list 'options "-verbosegc"))

    (if print-jni-info
	(add-to-list options "-verbosejni"))

    options))

(defmethod jde-db-heap-size-args ((this jde-db-debugger))
  "Generate heap size options."
  (let* ((memory-unit-abbrevs
	 (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")))
	 (start-cons (nth 0 jde-db-option-heap-size))
	 (start-size (format "%d%s" (car start-cons) 
			     (cdr (assoc (cdr start-cons)
					 memory-unit-abbrevs))))
	 (max-cons (nth 1 jde-db-option-heap-size))
	 (max-size (format "%d%s" (car max-cons) 
			   (cdr (assoc (cdr max-cons)
				       memory-unit-abbrevs))))
	 options)
    (if (not (string= start-size "1m"))
	(setq options 
	      (append options (list (concat "-Xms" start-size)))))
    (if (not (string= max-size "16m"))
	(setq options 
	      (append options (list (concat "-Xmx" max-size)))))
    options))

(defmethod jde-db-stack-size-args ((this jde-db-debugger))
  "Generate stack size arguments."
  (let* ((memory-unit-abbrevs
	 (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")))
	 (c-cons (nth 0 jde-db-option-stack-size))
	 (c-size (format "%d%s" (car c-cons) 
			 (cdr (assoc (cdr c-cons)
				     memory-unit-abbrevs))))
	 (java-cons (nth 1 jde-db-option-stack-size))
	 (java-size (format "%d%s" (car java-cons) 
			    (cdr (assoc (cdr java-cons)
					memory-unit-abbrevs))))
	 option)

    (if (not (string= c-size "128k"))
	(setq option
	      (append option (list (concat "-Xss" c-size)))))

    (if (not (string= java-size "400k"))
	(setq option 
	      (append option (list (concat "-Xoss" java-size)))))
    option))

(defmethod jde-db-garbage-collection-args ((this jde-db-debugger))
  "Set garbage collection options."
  (let ((no-gc-asynch (not 
		       (nth 0 jde-db-option-garbage-collection)))
	(no-gc-classes (not 
			(nth 1 jde-db-option-garbage-collection)))
	options)

    (if no-gc-asynch
	(setq options (append options '("-Xnoasyncgc"))))

    (if no-gc-classes
	(setq options (append options '("-Xnoclassgc"))))

    options))

(defmethod jde-db-garbage-collection-arg ((this jde-db-debugger))
  "Generate Java profile arg."
  (let ((profilep (car jde-db-option-java-profile))
	(file (cdr jde-db-option-java-profile)))

    (if profilep
	(if (string= file "./java.prof")
	    (list "-Xprof")
	  (list (concat "-Xprof:" file))))))


(defmethod jde-db-heap-profile-arg ((this jde-db-debugger))
  "Generate heap profile option."
  (let* ((profilep (car jde-db-option-heap-profile))
	 (prof-options (cdr jde-db-option-heap-profile))
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
	    (list "-Xhprof")
	  (list
	   (format 
	    "-Xhprof:file=%s,depth=%d,top=%d,sort=%s"
	    file depth top sort))))))

(defmethod jde-db-verify-arg ((this jde-db-debugger))
  ;; Set verify options.
  (let ((verify-all (nth 0 jde-db-option-verify))
	(verify-remote (nth 1 jde-db-option-verify)))
    (if verify-all
	(list"-Xverify")
      ;      (if verify-remote
      ;	  (list "-Xverifyremote"))
      (if (and
	   (not verify-all)
	   (not verify-remote))
	  (list "-Xnoverify")))))


(defmethod jde-db-command-line-args ((this jde-db-debugger))
  "Generate command line args."
  (if jde-db-option-vm-args
      (mapcar
       (lambda (arg)
	 arg)
       jde-db-option-vm-args)))


(defmethod jde-db-host-arg ((this jde-db-debugger))
  (if (not (string= jde-db-option-host ""))
      (list "-host" jde-db-option-host)))

(defmethod jde-db-launch-arg ((this jde-db-debugger))
  "Argument that tells the debugger to launch the 
debuggee vm immediately instead of waiting for a 
run command. Only the new (JDK 1.3) version of jdb
provides this option."
  nil)

(defmethod jde-db-get-vm-args ((this jde-db-debugger))
  (append 
   (jde-db-classic-mode-arg this)
   (jde-db-launch-arg this)
   (jde-db-classpath-arg this)
   (jde-db-property-args this)
   (jde-db-verbose-args this)
   (jde-db-heap-size-args this)
   (jde-db-command-line-args this)))

(defmethod jde-db-debugger-get-working-dir ((this jde-db-debugger))
  (if (string= jde-run-working-directory "")
      default-directory
    (jde-normalize-path 'jde-run-working-directory)))

(defmethod jde-db-debugger-get-prog-args ((this jde-db-debugger))
  )


(defmethod jde-db-debugger-start ((this jde-db-debugger) prog-args cmdstr)
  "Start the debugger."
  (let ((w32-quote-process-args ?\")
	(win32-quote-process-args ?\") ;; XEmacs
	(source-directory default-directory)
	(working-directory
	 (jde-db-debugger-get-working-dir this)))

    (oset this :buffer (get-buffer-create (oref this :buffer-name)))

    (save-excursion
      (set-buffer (oref this :buffer))
      ;; Do not erase the last transcript; user may wish to view it.
      ;; (erase-buffer)
      (goto-char (point-max))
      (cd working-directory)
      (insert (concat "cd " working-directory "\n"))
      (insert cmdstr)
      (comint-mode)
      (make-local-variable 'comint-prompt-regexp)
      (setq comint-prompt-regexp "\\(^> *\\)\\|\\(^.*\\[[0-9]+\\] *\\)")
      (make-local-variable 'paragraph-start)
      (setq paragraph-start comint-prompt-regexp)

      (comint-exec (oref this :buffer)
		   (oref this :buffer-name)
		   (oref this :path)
		   nil
		   prog-args)	  

      (oset this process
	    (get-buffer-process (oref this buffer)))	  

      (cd source-directory)

      (jde-db-set-process-filter this)
      (jde-db-set-process-sentinel this)
      (run-hooks 'jde-jdb-mode-hook)
      (pop-to-buffer (oref this buffer))

      (oset-default 'jde-db-debugger the-debugger this)
      (oset this running-p t))))

(defmethod jde-db-debugger-launch ((this jde-db-debugger))
  "Launch the debugger."
  (if (or
       (not (slot-boundp this 'buffer))
       (not (oref this :buffer))
       (not (comint-check-proc (oref this :buffer))))
      (let* ((debuggee (oref this debuggee))
	     (main-class (oref debuggee main-class))
	     (source-directory default-directory)
	     (working-directory
	      (jde-db-debugger-get-working-dir this))
	     (prog-args
	      (jde-db-debugger-get-prog-args this))
	     (command-string 
	      (concat
	       (oref this :path) " "
	       (jde-run-make-arg-string prog-args) "\n\n")))

	(oset this :buffer-name (concat "*debug" main-class "*"))
	(oset this :buffer (get-buffer-create (oref this :buffer-name)))

	(if jde-db-initial-step-p
	    (let ((step-cmd (oref (oref this cmd-set) step-into)))
	      (oset this next-cmd 
		    (append (oref this next-cmd) (list step-cmd)))))

	;; Forward to the debugger any breakpoint requests made
	;; by the user before launching the application.
	(if jde-db-breakpoints
	    (let ((bp-cmd (oref (oref this cmd-set) set-bp)))
	      (oset 
	       bp-cmd 
	       breakpoints
	       (mapcar (lambda (assoc) (cdr assoc)) jde-db-breakpoints))

	      (oset this next-cmd 
		    (append (oref this next-cmd) (list bp-cmd)))))

	(jde-db-debugger-start this prog-args command-string)

	(let* ((debuggee (oref this debuggee))
		 (debuggee-status (oref debuggee status)))
	    (oset debuggee-status running-p t)
	    (oset debuggee-status stopped-p t)))
    (progn
      (message "An instance of %s is running." (oref this :buffer-name))	
      (pop-to-buffer (oref this :buffer-name)))))


(defmethod jde-db-debugger-connect ((this jde-db-debugger) &optional listenp)
  "Launch the debugger."
  (if (or
       (not (slot-boundp this 'buffer))
       (not (oref this :buffer))
       (not (comint-check-proc (oref this :buffer))))
      (let* ((debuggee (oref this debuggee))
	     (source-directory default-directory)
	     (address (oref debuggee :address))
	     (working-directory
	      (jde-db-debugger-get-working-dir this))
	     (prog-args 
	      (if listenp
		  (if address
		      (list "-listen" address)
		    (list "-listenany"))
		(list "-attach" address)))
	     (command-string 
	      (format "%s %s\n\n"
	       (oref this :path)  
	       (mapconcat (lambda (x) x) prog-args " "))))

	(oset this :buffer-name (concat "*debug " address "*"))
	(oset this :buffer (get-buffer-create (oref this :buffer-name)))

	;; Forward to the debugger any breakpoint requests made
	;; by the user before launching the application.
	(if jde-db-breakpoints
	    (let ((bp-cmd (oref (oref this cmd-set) set-bp)))
	      (oset 
	       bp-cmd 
	       breakpoints
	       (mapcar (lambda (assoc) (cdr assoc)) jde-db-breakpoints))

	      (oset this next-cmd 
		    (append (oref this next-cmd) (list bp-cmd)))))

	(jde-db-debugger-start this prog-args command-string)

	(let* ((debuggee (oref this debuggee))
		 (debuggee-status (oref debuggee status)))
	    (oset debuggee-status running-p t)
	    (oset debuggee-status stopped-p t)))
    (progn
      (message "An instance of %s is running." (oref this :buffer-name))	
      (pop-to-buffer (oref this :buffer-name)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Generic Debug Commands                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following commands serve as a generalized interface between the
;; JDE user and JDE-supported debuggers, e.g., jdb or JDEbug.

;; This section is a work in progress. It entails generalizing the
;; existing jdb and JDEbug commands and replacing those commands
;; with the generalized commands.

(defun jde-debug-run ()
  "Start the current debuggee application."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status running-p))
	(error "Application %s is already running."
	       (oref debuggee main-class))
      (let* ((cmd-set (oref debugger cmd-set))
	     (run (oref cmd-set run)))
	(oset debuggee-status running-p t)
	(oset debuggee-status stopped-p nil)
	(oset debuggee-status suspended-p nil)
	(jde-db-exec-cmd debugger run)))))

  
(defun jde-debug-cont ()
  "Continues the current debuggee application from its current
stopping point."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (or
	      (oref debuggee-status stopped-p)
	      (oref debuggee-status suspended-p)))
	(let* ((cmd-set (oref debugger cmd-set))
	       (cont (oref cmd-set cont)))
	  (oset debuggee-status stopped-p nil)
	  (oset debuggee-status suspended-p nil)
	  (jde-db-exec-cmd debugger cont))
      (let ((class (oref debuggee main-class)))
	(message "Application %s is not stopped" class)))))


(defun jde-debug-quit ()
  "Quit debugging the current application."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status running-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (quit (oref cmd-set quit)))
	  (jde-db-exec-cmd debugger quit))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not running" class)))))

(defun jde-debug-step-over ()
  "Step to the next line in the current stack frame."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (step-over (oref cmd-set step-over)))
	  (oset debuggee-status stopped-p nil)
	  (jde-db-exec-cmd debugger step-over))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))
 
(defun jde-debug-step-into ()
  "Step to the next line in the current program."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (step-into (oref cmd-set step-into)))
	  (oset debuggee-status stopped-p nil)
	  (jde-db-exec-cmd debugger step-into))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-debug-step-out ()
  "Continue execution to the end of the current method."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (step-out (oref cmd-set step-out)))
	  (oset debuggee-status stopped-p nil)
	  (jde-db-exec-cmd debugger step-out))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))


(defun jde-debug-up ()
  "Move up the stack."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (up (oref cmd-set up)))
	  (jde-db-exec-cmd debugger up))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-debug-down ()
  "Move down the stack."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (down (oref cmd-set down)))
	  (jde-db-exec-cmd debugger down))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))
 
(defun jde-debug-where ()
  "Show current stopping point."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (where (oref cmd-set where)))
	  (jde-db-exec-cmd debugger where))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))
 
(defun jde-db-spec-breakpoint ()
  "Creates a specification for the breakpoint at the 
current line in the current file. Returns an object of 
type `jde-db-breakpoint'."
  (let ((file (buffer-file-name)))
    (setq jde-db-breakpoint-id-counter
	  (1+ jde-db-breakpoint-id-counter))
    (jde-db-breakpoint 
     (format "breakpoint: %s %d" 
	     (file-name-nondirectory file)
	     jde-db-breakpoint-id-counter)
     :id   jde-db-breakpoint-id-counter
     :file file 
     :class (concat (jde-db-get-package) 
		    (jde-db-get-class)))))


(defun jde-debug-set-breakpoint ()
  "Ask debugger to set a breakpoint at the current line 
in the current buffer."
  (interactive)
  (let* ((file (buffer-file-name))
	 (line (jde-get-line-at-point))
	 (bp (jde-db-find-breakpoint file line)))
    (unless bp
      (setq bp (jde-db-spec-breakpoint))
      (oset bp line line)
      (jde-db-breakpoints-add bp))
    (if (and
	 (jde-db-debuggee-running-p)
	 (or 
	  (jde-db-debuggee-stopped-p)
	  (jde-db-debuggee-suspended-p)))
	(let* ((debugger (oref 'jde-db-debugger the-debugger))
	       (bp-cmd (oref (oref debugger cmd-set) set-bp)))
	       (oset bp-cmd breakpoints (list bp))
	       (jde-db-exec-cmd debugger bp-cmd)))))

(defun jde-debug-clear-breakpoint()
  "Clear the breakpoint at the current line in the current buffer."
  (interactive)
  (jde-assert-source-buffer)
  (let* ((file (buffer-file-name))
	 (line (jde-get-line-at-point))
	 (bp (jde-db-find-breakpoint file line)))
    (if bp
	(if (and
	     (jde-db-debuggee-running-p)
	     (or
	      (jde-db-debuggee-stopped-p)
	      (jde-db-debuggee-suspended-p)))
	    (let* ((debugger (oref 'jde-db-debugger the-debugger))
		   (bp-cmd (oref (oref debugger cmd-set) clear-bp)))    
	      (oset bp-cmd breakpoints (list bp))
	      (jde-db-exec-cmd debugger bp-cmd))
	  (jde-db-delete-breakpoint bp)))))

(defun jde-debug-toggle-breakpoint ()
  "Sets or clears a breakpoint at the current line."
  (interactive)
  (assert (eq major-mode 'jde-mode) nil 
	  "This command works only in a Java source buffer.")
  (let*  ((file (buffer-file-name))
	  (line (jde-get-line-at-point))
	  (bp (jde-db-find-breakpoint file line)))
    (assert (jde-db-src-dir-matches-file-p file) nil
	    "You cannot set a breakpoint in a file that is not in `jde-sourcepath'.")
    (if bp
	(jde-debug-clear-breakpoint)
      (jde-debug-set-breakpoint))))

(defun jde-debug-clear-breakpoints()
  "Clear all existing breakpoints."
  (interactive)
  (if jde-db-breakpoints
      (if (jde-db-debuggee-running-p)
	  (let* ((debugger (oref 'jde-db-debugger the-debugger))
		 (bp-cmd (oref (oref debugger cmd-set) clear-bp)))    
	    (oset 
	     bp-cmd 
	     breakpoints 
	     (mapcar 
	      (lambda (assoc) (cdr assoc))
	      jde-db-breakpoints))
	    (jde-db-exec-cmd debugger bp-cmd))
	(loop for bp-assoc in jde-db-breakpoints do
	      (let ((bp (cdr bp-assoc)))
		(jde-db-delete-breakpoint bp))))))


(defvar jde-db-minibuffer-local-map nil
  "Keymap for minibuffer prompting of jdb startup command.")
(if jde-db-minibuffer-local-map
    ()
  (setq jde-db-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    jde-db-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))


(defun class-from-file-name (file-name)
  (file-name-sans-extension (file-name-nondirectory file-name)))


(defun jde-db-get-vm-args-from-user ()
  (if jde-db-read-vm-args
      (jde-run-parse-args
       (read-from-minibuffer
	"Vm args: "
	(car jde-db-interactive-vm-arg-history)
	nil nil
	'jde-db-interactive-vm-arg-history))))

(defun jde-db-get-app-args-from-user ()
  (if jde-db-read-app-args
      (jde-run-parse-args
       (read-from-minibuffer
	"Application args: "
	(car jde-db-interactive-app-arg-history)
	nil nil
	'jde-db-interactive-app-arg-history))))
	  

(defun jde-db-get-package ()
  "Return the package of the class whose source file resides in the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*\\<\\(package\\) +\\([^ \t\n]*\\) *;" (point-max) t)
	(concat (buffer-substring-no-properties (match-beginning 2) (match-end 2))
		"."))))

(defun jde-db-get-class () "Lookups and return fully qualified class
name, e.g. A$B if point is in inner class B of A."
  (interactive)
  (let ((class-info (jde-parse-get-innermost-class-at-point)))
    (if class-info
      (save-excursion
	(goto-char (cdr class-info))
	(let ((parent (jde-db-get-class)))
	(if (not parent)
	    (car class-info)
	    (concat parent "$" (car class-info))))))))


(defun jde-db-src-dir-matches-file-p (file)
  "Return true if one of `jde-sourcepath'
matches FILE."
  (let* ((directory-sep-char ?/)
	 (filename (jde-normalize-path file)))
    (find-if
     (lambda (dir-x)
       (string-match 
	(concat 
	 "^" 
	 (jde-normalize-path 
	  dir-x
	  'jde-sourcepath)) 
	filename))
     jde-sourcepath)))


(provide 'jde-db)


;; Change History
;; $Log: jde-db.el,v $
;; Revision 1.111  2002/08/27 04:19:01  paulk
;; Fixes bug in jde-db-src-dir-matches-file-p. Thanks to Andy Piper.
;;
;; Revision 1.110  2002/08/10 03:18:30  paulk
;; Modified jde-db-breakpoint-marker class so that breakpoint highlighting extends
;; the entire width of the buffer window--not just to the end of the line.
;; Thanks to Kevin A. Burton.
;;
;; Revision 1.109  2002/08/07 06:36:18  paulk
;; Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
;; removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
;; for suggesting this restructuring. Also fixed a number of compiler warnings caused
;; by genuine bugs.
;;
;; Revision 1.108  2002/07/12 12:20:27  jslopez
;; Fixes jde-db-option-verbose when set to load classes.
;; It was appending -v instead -f -verbose:class.
;;
;; Revision 1.107  2002/06/17 07:24:08  paulk
;; Updated the JDEE's applet debugging command to
;; work with its new jdb interface.
;;
;; Revision 1.106  2002/06/12 07:04:28  paulk
;; XEmacs compatibility fix: set win32-quote-process-args wherever
;; the JDEE sets w32-quote-process-args. This allows use of spaces in
;; paths passed as arguments to processes (e.g., javac)  started by
;; the JDEE.
;;
;; Revision 1.105  2002/06/11 06:34:38  paulk
;; Provides support for paths containing spaces as jdb arguments via the following change:
;; locally set the w32-quote-process-args variable to a quotation mark when launching
;; the jdb process.
;;
;; Revision 1.104  2002/05/21 06:34:27  paulk
;; Updated to support J2SDK 1.4.0 version of jdb.
;;
;; Revision 1.103  2002/05/12 06:37:33  paulk
;; Moved jde-db-search-src-dirs to the jde-util package as jde-search-src-dirs.
;;
;; Revision 1.102  2002/03/31 07:49:51  paulk
;; Renamed jde-db-source-directories. The new name is jde-sourcepath.
;;
;; Revision 1.101  2002/03/12 04:43:38  paulk
;; Removed initarg for class slots to silence eieio warning.
;;
;; Revision 1.100  2002/03/06 13:00:18  paulk
;; * Removed references to obsolete jde-db-option-attach variable.
;; * The jdb launch, attach, and listen commands now update the
;;   the-debugger field in the jde-db-debugger class.
;;
;; Revision 1.99  2002/03/04 06:43:41  paulk
;; Adds support for connecting debugger to an independently started
;; process, using either attach or listen mode.
;;
;; Revision 1.98  2002/02/04 05:47:17  paulk
;; Added code to rehighlight breakpoints if the user kills a
;; buffer for a source file that contains breakpoints and
;; then reopens the file.
;;
;; Revision 1.97  2002/01/19 06:42:22  paulk
;; Minor updates.
;;
;; Revision 1.96  2002/01/15 13:33:27  paulk
;; Adds a Clear Breakpoints command for jdb.
;;
;; Revision 1.95  2002/01/14 13:30:42  paulk
;; - Now defines three breakpoint marker colors: green for a specified breakpoint,
;;   yellow for a requested breakpoint, and red for an enabled breakpoint.
;;
;; - The debug application command now requests all specified
;;   breakpoints at the beginning of a debug session.
;;
;; - The debug application command now changes the color of all breakpoints
;;   to specified at the end of a debug session.
;;
;; Revision 1.94  2002/01/11 05:43:37  paulk
;; - Use overlays/extents to record location of breakpoints in a buffer.
;; - Use different colors to indicate requested and enabled breakpoints.
;;
;; Revision 1.93  2002/01/04 07:12:17  paulk
;; Fixed XEmacs compatibility bug that caused the toggle
;; breakpoint command to signal an error that it could not
;; find the source file in jde-db-source-directories.
;;
;; Revision 1.92  2002/01/02 05:29:45  paulk
;; Added a stack-depth field to the jde-db-debuggee class.
;;
;; Revision 1.91  2001/12/31 07:51:09  paulk
;; Implemented generalized quit, step-over, step-into, stack up, stack down,
;; and stack where commands.
;;
;; Revision 1.90  2001/12/28 05:32:55  paulk
;; Implemented generalized debuggee process run and continue commands.
;;
;; Revision 1.89  2001/12/17 08:02:05  paulk
;; Initial version of generalized clear breakpoint command. Created generalized
;; classes to represent the debuggee process.
;;
;; Revision 1.88  2001/12/14 05:08:37  paulk
;; Setup generic methods for processing the debugger output and status
;; change notification.
;;
;; Revision 1.87  2001/12/10 04:29:55  paulk
;; Created generalized breakpoint framework. Provided initial
;; implementation for jdb. A lot of work remains.
;;
;; Revision 1.86  2001/12/04 05:37:35  paulk
;; Moved jdb related code to jde-jdb.el.
;;
;; Revision 1.85  2001/11/30 03:08:03  jslopez
;; Fixes reference to free variables.
;;
;; Revision 1.84  2001/11/27 08:03:46  paulk
;; Updated jdb-db to invoke the version of jdb appropriate to the JDK for the current project.
;;
;; Revision 1.83  2001/11/26 02:45:51  paulk
;; Added reference to jde-db-source-directories in invocation of jde-normalize-path
;; in jde-db-src-dir-matches-file-p.
;;
;; Revision 1.82  2001/11/25 06:34:12  paulk
;; Added function jde-db-src-dir-matches-file-p. Thanks to
;; Kevin Burton for initial implementation.
;;
;; Revision 1.81  2001/11/04 14:58:05  paulk
;; Restored jde-db-classic-mode-vm option.
;;
;; Revision 1.80  2001/11/04 14:51:23  paulk
;; Fixed typo in classic mode argument (i.e., -tclassic should be -classic).
;;
;; Revision 1.79  2001/09/30 05:29:32  paulk
;; Changed the name of the customization variable jde-db-debugger to be consistent with jde-compiler and to avoid conflict with jde-db-debugger class.
;;
;; Revision 1.78  2001/09/28 04:48:00  paulk
;; Defines a new eieio class of debuggers that serves as the parent
;; for jdb and JDEbug debuggers. Create a jde-db-jdb class that serves
;; as the root of jdb and oldjdb.
;;
;; Revision 1.77  2001/07/31 05:11:50  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.76  2001/04/16 05:49:34  paulk
;; Normalized paths. Thanks to Nick Sieger.
;;
;; Revision 1.75  2001/04/12 04:42:23  paulk
;; Normalize jde-run-working-directory.
;;
;; Revision 1.74  2001/04/11 03:21:33  paulk
;; Updated to resolve relative paths relative to the project file that defines them. Thanks to Nick Sieger.
;;
;; Revision 1.73  2001/04/08 04:14:29  paulk
;; jdb interface has been fixed to work around JDK 1.3 bug that causes the jdb command prompt to sometimes appear in the wrong place in jdb output. Thanks to Andy Bennett <andrew.bennett@ericsson.com> for this fix.
;;
;; Revision 1.72  2001/03/16 04:07:03  paulk
;; Fixed regular expression for finding package in source buffer so that there must be a space between package and the package name. This is to prevent false hits. Thanks to Rory Molinari <molinari@math.lsa.umich.edu>.
;;
;; Revision 1.71  2001/03/13 04:14:54  paulk
;; Split jde-find-class-source into to files, one of which returns the path of the source file while the other opens the file in a buffer.
;;
;; Revision 1.70  2001/02/26 04:17:50  paulk
;; jde-db now handles case where jde-global-classpath and jde-db-option-classpath are nil.
;;
;; Revision 1.69  2001/02/03 08:44:56  paulk
;; Changed declaration of customized variables to allow path completion.
;; Now allows environment variables in jde-db-source-directories.
;;
;; Revision 1.68  2001/02/03 07:28:06  paulk
;; Now uses generalized jde-build-classpath function to build classpath argument to debugger.
;;
;; Revision 1.67  2000/12/18 05:22:45  paulk
;; *** empty log message ***
;;
;; Revision 1.66  2000/10/10 06:36:59  paulk
;; Fixed bug where selecting Other and Executable as the debugger results in the executable name being inserted twice.
;;
;; Revision 1.65  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.64  2000/08/19 06:46:22  paulk
;; Updated to handle JDK 1.3 version of jdb. Source pointer now moves to
;; current location in current stack frame.
;;
;; Revision 1.63  2000/06/12 08:37:43  paulk
;; Now displays JDEbug menu when running XEmacs.
;;
;; Revision 1.62  2000/05/10 05:41:32  paulk
;; The JDEbug menu now appears or disappears when you select or deselect JDEbug as the current debugger.
;;
;; Revision 1.61  2000/03/16 05:08:25  paulk
;; Added JDEbug option to jde-db-debugger.
;;
;; Revision 1.60  2000/03/08 05:40:01  paulk
;; jde-db-format-command now signals an error it it cannot determine the containing class.
;;
;; Revision 1.59  2000/02/10 02:50:32  paulk
;; Replaced jde expand file name function with expand-file-name.
;;
;; Revision 1.58  2000/02/01 04:08:12  paulk
;; Modified the Jdb->Set Breakpoint command (gud-break) to set breakpoints correctly
;; in inner classes.
;;
;; Revision 1.57  2000/01/15 08:01:52  paulk
;; Reimplemented directory search functions.
;;
;; Revision 1.56  1999/11/16 05:58:17  paulk
;; Added trace method commands and skeletons for trace class and cancel
;; trace commands.
;;
;; Revision 1.55  1999/11/04 05:49:10  paulk
;; Amended jde-db-make-qualified-class-name-regexp to permit package names to begin
;; with non-word characters, e.g., underscores. Contributed by "Patrick J. McNerthney"
;; <pat@mcnerthney.com>.
;;
;; Revision 1.54  1999/09/28 04:06:59  paulk
;; Supplied missing left parentheses.
;;
;; Revision 1.53  1999/09/05 04:33:28  paulk
;; Added support for running vm in classic mode.
;;
;; Revision 1.52  1999/03/10 16:55:02  paulk
;; Fixed jde-db-find-file to return the current buffer if it cannot find a file and
;; XEmacs is the editor.
;;
;; Revision 1.51  1999/03/06 00:55:38  paulk
;; Changed default value of jde-db-source-directories to be nil.
;;
;; Revision 1.50  1999/03/06 00:44:08  paulk
;; Make sure that case-sensitive matching is used when extracting package names from
;; debugger breakpoint messages.
;;
;; Revision 1.49  1999/02/26 15:52:52  paulk
;; Catch non-existent directory errors when searching for source
;; files and packages. Thanks to Thanh Nguyen <Thanh.Nguyen@Eng.Sun.COM>
;; for finding and providing a fix for this bug.
;;
;; Revision 1.48  1999/02/25 15:24:43  paulk
;; Fixed jde-db-find-file so that it displays an error when it cannot find a file instead of
;; opening an empty source buffer.
;;
;; Provided a set-value function for jde-db-source-directories that appends a slash to
;; the end of each path if the path does not already end in a slash.
;;
;; Defined a new command, jde-find-class-source, that finds and opens the source file
;; for a specified class.
;;
;; Improved the regular expression used by jde-db-get-package to ignore tabs at the
;; beginning of a line.
;;
;; Revision 1.47  1999/02/15 02:02:35  paulk
;; Forgot to concatenate in last fix.
;;
;; Revision 1.46  1999/02/15 00:52:44  paulk
;; Fixed bug in qualified-class-name-regexp.
;;
;; Revision 1.45  1999/02/10 18:35:51  paulk
;; Added support for appletviewer -encoding and -J options.
;;
;; Revision 1.44  1999/02/08 17:18:17  paulk
;; jde-db-applet now supports file completion and remembers the last path entered.
;;
;; Revision 1.43  1999/02/06 03:55:11  paulk
;; Fixed bug and generalized regular expression in jde-db-make-qualified-class-name-regexp.
;;
;; Revision 1.42  1999/02/03 18:12:03  paulk
;; Fixed regular expression in jde-db-get-package to eliminate spurious hits, e.g.
;; commented out package statements. Thanks to Frederic Baumann <baumann@ilog.fr>
;; for reporting this bug.
;;
;; Revision 1.41  1999/02/03 17:48:34  paulk
;; Patched jde-db-get-app-args-from-user to remember arguments.
;; Thanks to Brian Burton <brian@burton-computer.com>
;;
;; Revision 1.40  1999/02/03 17:41:56  paulk
;; Fixed jde-db-make-qualified-class-name-regexp to handle packages with underscores.
;; Thanks to Brian Burton <brian@burton-computer.com>.
;;
;; Revision 1.39  1999/02/03 17:26:46  paulk
;; Changed jde-db-make-qualified-class-name-regexp to handle inner classes.
;; Thanks to Michael Lepore <lepore@process.com> for this fix.
;;
;; Revision 1.38  1999/02/03 01:53:49  paulk
;; Fixed jde-db-applet to check the current directory for the html file to run.
;;
;; Revision 1.37  1999/02/02 16:06:01  paulk
;; Added the jde-db-applet command. This command allows you to debug an applet, using
;; appletviewer.
;;
;; Revision 1.36  1999/02/02 15:25:28  paulk
;; Removed unwanted space in -D (properties) debug option.
;;
;; Revision 1.35  1999/01/17 00:36:43  paulk
;; Now uses gud-find-c-expr or find-c-expr, whichever is bound.
;;
;; Revision 1.34  1999/01/13 22:18:08  paulk
;; Added Andy Piper's NT/XEmacs 21 compatibility changes.
;; Changed find-c-expr to gud-findc-expr
;;
;; Revision 1.33  1998/11/22 18:18:36  paulk
;; Made comint-prompt-regexp and  paragraph-start local variables.
;;
;; Revision 1.32  1998/11/04 02:59:09  paulk
;; Corrected verbiage in Jde Debugger Options description.
;;
;; Revision 1.31  1998/09/12 00:05:57  paulk
;; Debugger now runs application from directory specified by jde-run-working-directory.
;;
;; Revision 1.30  1998/06/30 04:03:19  paulk
;; Added variables `jde-db-read-vm-args' and `jde-db-read-app-args'. The use of
;; these variables is the same as the corresponding jde-run variables.
;;
;; Revision 1.29  1998/06/29 02:50:44  paulk
;; Fixed bug in marker filter.
;;
;; Revision 1.28  1998/06/27 03:34:31  paulk
;; Provided a hack to handle reordering of threaded messages on Solaris.
;;
;; Provided code to handle case where current class has no line number
;; information.
;;
;; Revision 1.27  1998/06/25 04:27:23  paulk
;; Removed debug messages from jde-db-marker-filter.
;;
;; Revision 1.26  1998/06/25 04:21:10  paulk
;; Modified jde-db-marker-filter to accummulate debugger output
;; in chunks. Fixes bug reported by Eric Prud'hommeaux (eric@w3.org).
;;
;; Revision 1.25  1998/06/22 03:52:28  paulk
;; Added jde-db-startup-commands variable. This variable allows you to
;; specify debugger commands to run when the debugger is started.
;;
;; Revision 1.24  1998/06/21 00:09:43  paulk
;; Added a customizable feature, jde-db-set-initial-breakpoint, that causes
;; the JDE to set an initial breakpoint in an app's main routine and run
;; to the breakpoint on debugger startup. The feature is enabled by default.
;;
;; Revision 1.23  1998/06/20 23:42:07  paulk
;; Made jde-db-marker-regexp a custom variable to facilitate the use of the JDE
;; with debuggers other than jdb.
;;
;; Changed the marker regular expression to detect only jdb breakpoint messages,
;; i.e., messages of the form
;;
;;   Breakpoint hit: qualified.class.name (class:line)
;;
;; This should eliminate the problem of spurious hits when exceptions occur and
;; stack traces are printed.
;;
;; Revision 1.22  1998/05/27 06:09:46  paulk
;; Added autoload comments.
;;
;; Revision 1.21  1998/03/27 04:16:12  kinnucan
;; Fixed typo in the code that displays the jdb menu on XEmacs.
;;
;; Revision 1.20  1998/03/27 04:14:53  kinnucan
;; Modified jde-db-search-src-dirs to take current package as an
;; argument rather than use a global variable. This allows
;; it to be used by jde-java-build function.
;;
;; Revision 1.19  1998/03/18 03:54:06  kinnucan
;; Changed jde-db-marker-regexp to account for inner classes.
;; Thanks to Andreas Rasmusson <Andreas.Rasmusson@sics.se> for
;; providing this fix.
;;
;; Revision 1.18  1998/03/04 04:28:36  kinnucan
;; Added test for jde-run-application-class = "" to jde-db
;;
;; Revision 1.17  1998/02/27 22:16:34  kinnucan
;; Changed copyright to Paul Kinnucan.
;; Have not yet assigned rights to FSF.
;;
;; Revision 1.16  1998/02/27 22:15:24  kinnucan
;; Added support for Emacs customization feature.
;;
;; Revision 1.15  1998/02/17 04:16:38  kinnucan
;; Fixed bug in jde-deb-set-source-paths that caused the last
;; directory to not be normalized (i.e., slash appended).
;;
;; Revision 1.14  1998/02/12 05:15:38  kinnucan
;; Changed the jde-db-search-src-dirs to search the source directory list from
;; front to back instead of back to front. The former search order did not allow newer versions of the same class to shadow older versions. Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for supplying this fix.
;;
;; Revision 1.13  1998/02/12 04:57:13  kinnucan
;; Fixed bug in jde-db-marker-filter that sometimes prevented the JDE from
;; loading the correct source file. Thanks to David J. Biesack
;; <sasdjb@unx.sas.com> for supplying the fix.
;;
;; Revision 1.12  1997/10/30 05:42:37  kinnucan
;; Made configuration variables settable.
;;
;; Revision 1.11  1997/10/26 05:49:59  kinnucan
;; Applied Derek Young's patch to cause jde to qualify class names
;; when setting a breakpoint.
;;
;; Revision 1.10  1997/10/20 05:27:48  kinnucan
;; Removed reference to deleted function jde-db-massage-args
;;
;; Revision 1.9  1997/10/11 01:36:05  kinnucan
;; Fixed bug in jde-db-search-src-dirs discovered by Jonathan Payne.
;;
;; Revision 1.8  1997/10/06 14:40:53  kinnucan
;; Fixed bugs in jde-db-set-debugger command.
;;
;; Revision 1.7  1997/10/05 21:20:15  kinnucan
;; 1. Added the variables jde-db-debugger and jde-db-debugger-is-executable
;;    and the associated setter function jde-db-set-debugger. These allow
;;    you to specify a custom debugger for the JDE>
;;
;; 2. Added jde-db-args and jde-db-app-args and the associated setter
;;    functions. These allow you to specify debugger and application
;;    command line arguments.
;;
;; Revision 1.6  1997/10/05 04:53:04  kinnucan
;; Fixed bug in print object menu item.
;;
;; Revision 1.5  1997/08/26 14:53:39  paulk
;; Fixed bug in check-source-path.
;;
;; Revision 1.4  1997/08/26 08:52:14  kinnucan
;; Tweaked JDE Version number for JDE 1.8 release.
;;
;; Revision 1.3  1997/07/05 04:18:10  kinnucan
;; Updated make-jdb-command to run either the class previously specifed with
;; the jde-run-set-app command or the class corresponding to the code in the
;; current buffer.
;;
;; Revision 1.2  1997/06/18 18:45:11  paulk
;; Added error-checking to jde-db-set-source-paths function. Now checks for
;; existence of specified directories and appends a terminal slash to paths
;; that lack it.
;;
;; Revision 1.1  1997/06/18 17:21:59  paulk
;; Initial revision
;;

;;; end of jde-db.el
