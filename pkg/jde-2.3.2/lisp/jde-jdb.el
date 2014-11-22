;;; jde-jdb.el -- Debugger mode for jdb.
;; $Revision: 1.19 $ $Date: 2002/11/11 05:24:26 $ 

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

(require 'jde-db)


;; Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for this function
;; and its use in jde-db-marker-filter.
;; Amended by "Patrick J. McNerthney" <pat@mcnerthney.com> to allow
;; package names to begin with underscores.
(defun jde-jdb-make-qualified-class-name-regexp (class)
  "Constructs a regular expression to extract a qualified class name from a jdb
breakpoint message."
  (concat "\\(\\(\\(\\(\\w\\|[_]\\)*\\.\\)*\\)" class "\\)\\(\\b\\|\\$\\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Debugger Commands                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Launch command

(defclass jde-jdb-cmd-launch (jde-db-cmd) ()
  "Asks jdb to launch the debuggee application.")

(defmethod initialize-instance ((this jde-jdb-cmd-launch) &rest fields)
  (call-next-method)
  (oset this name "launch"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-launch))
  "Creates command line for jdb launch command."
  "")


;; Run command

(defclass jde-jdb-cmd-run (jde-db-cmd) ()
  "Asks jdb to start the debuggee application.")

(defmethod initialize-instance ((this jde-jdb-cmd-run) &rest fields)
  (call-next-method)
  (oset this name "run"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-run))
  "Creates command line for jdb run command."
  "run")

;; Cont command

(defclass jde-jdb-cmd-cont (jde-db-cmd) ()
  "Asks jdb to continue the debuggee application from its current
stopping point.")

(defmethod initialize-instance ((this jde-jdb-cmd-cont) &rest fields)
  (call-next-method)
  (oset this name "cont"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-cont))
  "Creates command line for jdb cont command."
  "cont")

;; Quit command

(defclass jde-jdb-cmd-quit (jde-db-cmd) ()
  "Quit debugging the current application.")

(defmethod initialize-instance ((this jde-jdb-cmd-quit) &rest fields)
  (call-next-method)
  (oset this name "quit"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-quit))
  "Creates command line for jdb quit command."
  "quit")

;; Step-over command

(defclass jde-jdb-cmd-step-over (jde-db-cmd) ()
  "Step to the next line in the current frame.")

(defmethod initialize-instance ((this jde-jdb-cmd-step-over) &rest fields)
  (call-next-method)
  (oset this name "next"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-step-over))
  "Creates command line for jdb step-over command."
  "next")

;; Step-into command

(defclass jde-jdb-cmd-step-into (jde-db-cmd) ()
  "Step to the next line in the current program.")

(defmethod initialize-instance ((this jde-jdb-cmd-step-into) &rest fields)
  (call-next-method)
  (oset this name "step"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-step-into))
  "Creates command line for jdb step-into command."
  "step")

;; Step-out command

(defclass jde-jdb-cmd-step-out (jde-db-cmd) ()
  "Continue to the end of the current method.")

(defmethod initialize-instance ((this jde-jdb-cmd-step-out) &rest fields)
  (call-next-method)
  (oset this name "step up"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-step-out))
  "Creates command line for jdb step-out command."
  "step up")


;; Up stack command

(defclass jde-jdb-cmd-up (jde-db-cmd) ()
  "Move up one stack frame.")

(defmethod initialize-instance ((this jde-jdb-cmd-up) &rest fields)
  (call-next-method)
  (oset this name "up"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-up))
  "Creates command line for jdb up command."
  "up")

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-up) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response. This method invokes the jdb where
command in order to position the debug pointer at the
current stack location."
  ;; (jde-debug-where)
  (let* ((jdb (oref this debugger))
	 (cmds (oref jdb cmd-set))
	 (cmd (oref cmds where)))
    (jde-db-exec-cmd jdb cmd)))

;; Down stack command

(defclass jde-jdb-cmd-down (jde-db-cmd) ()
  "Move down one stack frame.")

(defmethod initialize-instance ((this jde-jdb-cmd-down) &rest fields)
  (call-next-method)
  (oset this name "down"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-down))
  "Creates command line for jdb down command."
  "down")

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-down) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response. This method invokes the jdb where
command in order to position the debug pointer at the
current stack location."
  ;;(jde-debug-where)
  (let* ((jdb (oref this debugger))
	 (cmds (oref jdb cmd-set))
	 (cmd (oref cmds where)))
    (jde-db-exec-cmd jdb cmd)))


;; Where stack command

(defclass jde-jdb-cmd-where (jde-db-cmd) ()
  "Point to current location on the stack.")

(defmethod initialize-instance ((this jde-jdb-cmd-where) &rest fields)
  (call-next-method)
  (oset this name "where"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-where))
  "Creates command line for jdb where command."
  "where")

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-where) output)
"Processes the output of the jdb where
 command, which lists the current stack. An example of the output
 is
   
      [1] jmath.LinearSystem$InnerClass.print (LinearSystem.java:36)
      [2] jmath.LinearSystem.<init> (LinearSystem.java:52)
      [3] jmath.Test.main (Test.java:38)
   
 This method positions the source line cursor at the position that
 matches the current location of the debugger in the program's
 stack (set by the jdb up and down stack commands)."
  (let* ((jdb (oref this debugger))
	 (debuggee (oref jdb debuggee)))
    (if (string-match 
	 (concat "^  \\[" 
		 (oref debuggee :stack-depth)
		 "\\] .*(\\([^\$\n]*\\).*:\\([0-9]+\\))")
	 output)
	(let ((marker (match-string 0 output))
	      (class (match-string 1 output))
	      (line-no (string-to-int (match-string 2 output)))
	      (package ""))

	  (if (equal ".java" (substring class -5))
	      (setq class (substring class 0 -5)))

	  ;; Extract package path from input.
	  (let ((case-fold-search nil))	;; Make sure search is case-sensitive
	    (and (string-match (jde-jdb-make-qualified-class-name-regexp class) marker)
		 (setq package
		       (substring marker (match-beginning 2) (match-end 2)))))

	  (jde-db-set-debug-cursor 
	   (if package (concat package "." class) class)
	   (concat class ".java") line-no)))
    output))

;; Set Breakpoint command

(defclass jde-jdb-cmd-set-breakpoint (jde-db-cmd-breakpoint) ()
  "Asks jdb to set the breakpoint specified by the
breakpoint field.")

(defmethod initialize-instance ((this jde-jdb-cmd-set-breakpoint) &rest fields)
  (call-next-method)
  (oset this name "stop at"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-set-breakpoint))
  "Creates command line for jdb set breakpoint command."
  (let* ((bps (oref this breakpoints))
	 (bp (car bps)))
    (format "stop at %s:%d"  
	    (oref bp class)
	    (jde-db-breakpoint-get-line bp))))

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-set-breakpoint) output)
  "Called when the debugger responds to the last set-breakpoint
  command. Invokes `jde-db-mark-breakpoint-requested' on the breakpoint and
updates the breakpoint to `requested' status.  Removes the breakpoint
from the command's breakpoint list. If the list contains more
breakpoints, this method reissues the command on the next breakpoint
on the list."
  ;; (message "set-bp resp <<%s>>" output)
  (if (or
       (string-match "Deferring breakpoint" output)
       (string-match "Set breakpoint" output))
      (let* ((bps (oref this breakpoints))
	     (bp (car bps))
	     (file (oref bp file))
	     (line (jde-db-breakpoint-get-line bp)))
	(oset bp status 'requested)
	(jde-db-mark-breakpoint-requested file line)
	(setq bps (cdr bps))
	(oset this breakpoints bps)
	(if bps
	    (let ((jdb (oref this debugger)))
	      (jde-db-exec-cmd jdb this))))))

;; Clear Breakpoint command

(defclass jde-jdb-cmd-clear-breakpoint (jde-db-cmd-breakpoint) ()
  "Asks jdb to clear the breakpoint specified by the 
breakpoint field.")

(defmethod initialize-instance ((this jde-jdb-cmd-clear-breakpoint) &rest fields)
  (call-next-method)
  (oset this name "clear"))

(defmethod jde-db-cmd-make-command-line ((this jde-jdb-cmd-clear-breakpoint))
  "Creates command line for jdb clear breakpoint command."
  (let* ((bps (oref this breakpoints))
	 (bp (car bps)))
    (format "clear %s:%d"  
	    (oref bp class)
	    (jde-db-breakpoint-get-line bp))))

(defmethod jde-db-cmd-notify-response ((this jde-jdb-cmd-clear-breakpoint) output)
  "Called when the debugger responds to the last clear-breakpoint command.
Removes the breakpoint from the command's breakpoint list. If the list contains
more breakpoints, this method reissues the clear command on the next breakpoint
on the list."
  (let* ((bps (oref this breakpoints))
	 (bp (car bps)))
    (jde-db-delete-breakpoint bp)
    (setq bps (cdr bps))
    (oset this breakpoints bps)
    (if bps
	(let ((jdb (oref this debugger)))
	  (jde-db-exec-cmd jdb this)))))

;; jdb Command Set

(defclass jde-jdb-cmd-set (jde-db-cmd-set) ()
  "Set of debugger commands implemented by jdb.")

(defmethod initialize-instance ((this jde-jdb-cmd-set) &rest fields)
  "Construct jdb command set."
  (call-next-method)
  (let ((jdb (oref this debugger)))
    (oset this launch
	  (jde-jdb-cmd-launch "launch" :debugger jdb))
    (oset this run
	  (jde-jdb-cmd-run "run" :debugger jdb))
    (oset this cont
	  (jde-jdb-cmd-cont "cont" :debugger jdb))
    (oset this quit
	  (jde-jdb-cmd-quit "jdb quit" :debugger jdb))
    (oset this step-over
	  (jde-jdb-cmd-step-over "jdb step-over cmd" :debugger jdb))
    (oset this step-into
	  (jde-jdb-cmd-step-into "jdb step-into cmd" :debugger jdb))
    (oset this step-out
	  (jde-jdb-cmd-step-out "jdb step-out cmd" :debugger jdb))
    (oset this up
	  (jde-jdb-cmd-up "jdb up cmd" :debugger jdb))
    (oset this down
	  (jde-jdb-cmd-down "jdb down cmd" :debugger jdb))
    (oset this where
	  (jde-jdb-cmd-where "jdb where cmd" :debugger jdb))
    (oset this set-bp 
	  (jde-jdb-cmd-set-breakpoint "jdb set breakpoint" :debugger jdb))
    (oset this clear-bp
	  (jde-jdb-cmd-clear-breakpoint "jdb clear breakpoint" :debugger jdb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Breakpoint Listener                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-jdb-breakpoint-listener (jde-db-listener)
  ((marker-regexp   :initarg :marker-regexp
		    :type string
		    :documentation 
		    "Regular expression for parsing breakpoint messages.")
   (class-index     :initarg :class-index
		    :type integer
		    :initform 3
		    :documentation
		    "Index of class name parsed by marker-regex")
   (line-index      :initarg :line-index
		    :type integer
		    :initform 5
		    :documentation
		    "Index of line number parsed by marker-regex")
   (noline-regexp   :initarg :noline-regexp
		    :type string
		    :documentation 
		    "Regular expression for parsing breakpoint messages without line numbers.")
   ;; There's no guarantee that Emacs will hand the filter the entire
   ;; marker at once; it could be broken up across several strings.  We
   ;; might even receive a big chunk with several markers in it.  If we
   ;; receive a chunk of text which looks like it might contain the
   ;; beginning of a marker, we save it here between calls to the
   ;; filter.
   (marker-acc      :initarg :marker-acc
		    :type string
		    :initform ""
		    :documentation
		    "Debug output accumulator")

   )
  "Handles jdb breakpoint events.")


(defmethod initialize-instance ((this jde-jdb-breakpoint-listener) &rest fields)
  "Construct breakpoint listener."

  (call-next-method)

  ;; Regular expression used to find a jdb breakpoint position marker.
  ;; The regular expression must have two subexpressions. The first matches
  ;; the name of the class in which the breakpoint occurs; the second, the
  ;; line number at which the breakpoint occurs. The default expression
  ;; matches breakpoint messages emitted by jdb. You may need to change
  ;; the expression to accommodate other debuggers."
  (oset 
   this
   :marker-regexp
   "^.*: thread=.*, \\(\\(.*[.]\\)*\\)\\([^\$]*\\)\\(\$.*\\)*[.].+(), line=\\([0-9]*\\),")

  ;; Regular expression to match a breakpoint message that lacks a line
  ;; number because the breakpoint occurs in a class compiled without deug
  ;; information.
  (oset 
   this
   :noline-regexp
   "^Breakpoint hit: .*(pc \\([0-9]*\\))"))



(defmethod jde-jdb-fixup-output ((this jde-jdb-breakpoint-listener))
    ;; This is a hack to accommodate reorder of message chunks
    ;; on Solaris at debugger startup.
    (if (string-match "running ...\n" (oref this :marker-acc))
	(oset this :marker-acc
	      (concat "running ...\n"
		      (substring (oref this :marker-acc) 0 (match-beginning 0))
		      (substring (oref this :marker-acc) (match-end 0)))))


    ;; This is a hack to fix reordering of message chunks on Windows 2000
    ;; The problem is the debugger prompt - the thread name with the stack
    ;; depth (eg main[1]) - sometimes shows up in the middle of the output
    ;; from the command sent to the debugger.
    ;; This seems to show up most often with step commands.
    ;;(message "checking string %s" (oref jdb :marker-acc))
    (if (string-match "^.*: \\([-a-zA-Z0-9_$]+\\[[0-9]+\\] \\)thread=" 
		      (oref this :marker-acc))
	(oset this :marker-acc
	      (concat (match-string 1 (oref this :marker-acc))
		      (substring (oref this :marker-acc) 0 (match-beginning 1))
		      (substring (oref this :marker-acc) (match-end 1)))))
    ;; (message "fixed string is %s" jde-db-marker-acc)
    )


(defmethod jde-jdb-set-breakpoint-listener ((this jde-jdb-breakpoint-listener) output)
  "Listens for set breakpoint messages."
  (let ((msgs (split-string output "\n")))
    (loop for msg in msgs do
	  (if (string-match 
	       "^.*Set .*breakpoint \\(.*\\):\\([0-9]+\\)"
	       msg)
	      (let* ((class (substring 
			     msg
			     (match-beginning 1) 
			     (match-end 1)))
		     (line (string-to-int 
			    (substring 
			     msg
			     (match-beginning 2)
			     (match-end 2))))
		     (source-buffer (jde-db-find-class-source class))
		     (path (buffer-file-name source-buffer))
		     (bp (jde-db-find-breakpoint path line)))
		(oset bp status 'active)
		(jde-db-mark-breakpoint-active path  line))))))

(defmethod jde-db-listener-filter-output ((this jde-jdb-breakpoint-listener) input)
  "Filters the output of the debugger."
    (let ((jdb (oref this debugger))
	  (output ""))

    ;; Accumulate next chunk of debugger output.
    (oset this
	  :marker-acc (concat 
		       (oref this :marker-acc) 
		       input))

    ;; (message (format "<acc-start>%s<acc-end>" (oref this :marker-acc)))

    (jde-jdb-fixup-output this)

    (let* ((marker-regexp (oref this :marker-regexp))
	   (marker-regexp-class-index (oref this :class-index))
	   (marker-regexp-line-index (oref this :line-index)))

      ;; (message (concat "jdb output:" input))
      ;; (message (concat "acc = " jde-db-marker-acc))
    
      ;; Process all the complete markers in this chunk.
      (if (string-match marker-regexp (oref this :marker-acc))
	  ;; Extract the frame position from the marker.
	  (let ((premarker (substring 
			    (oref this :marker-acc) 0 (match-beginning 0)))
		(marker (substring (oref this :marker-acc) 
				   (match-beginning 0) (match-end 0)))
		(class (substring 
			(oref this :marker-acc)  
			(match-beginning marker-regexp-class-index) 
			(match-end marker-regexp-class-index)))
		(line-no (string-to-int 
			  (substring 
			   (oref this :marker-acc)
			   (match-beginning marker-regexp-line-index)
			   (match-end marker-regexp-line-index))))
		(rest (substring (oref this :marker-acc) (match-end 0)))
		(package ""))

	    ;; Extract package path from input.
	    (let ((case-fold-search nil)) ;; Make sure search is case-sensitive
	      (and (string-match (jde-jdb-make-qualified-class-name-regexp class) marker)
		   (setq package
			 (substring marker (match-beginning 2) (match-end 2))))

               ;; (message "jde-db package: %s. marker = %s" jde-db-last-package marker)
               ;;(message "case-fold-search = %s" (if case-fold-search "true" "false"))
	      )

	    ;; Insert debugger output into debugger buffer.
	    (setq output (concat premarker marker))

	    ;; Set the accumulator to the remaining text.
	    (oset this :marker-acc rest)

	    (jde-db-set-debug-cursor 
	     (concat package "." class) (concat class ".java") line-no)

	    (let* ((debuggee (oref jdb debuggee))
		   (status (oref debuggee status)))
	      (oset status stopped-p t)))))

   ;; Handle case where there is no line number info in current class.
    (if (string-match (oref this noline-regexp) (oref this marker-acc))
	(let ((premarker (substring 
			  (oref this :marker-acc) 0 (match-beginning 0)))
	      (marker (substring (oref this :marker-acc)
				 (match-beginning 0) (match-end 0)))
	      (pc (substring (oref this :marker-acc)
			     (match-beginning 1) (match-end 1)))
	      (rest (substring (oref this :marker-acc) (match-end 0))))

	  (setq output (concat premarker marker))
	  (oset this :marker-acc rest)))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\\(^Breakpoint hit:\\)\\|\\(^Step completed:\\)" 
		      (oref this :marker-acc))
	(progn
	;; Everything before the potential marker start can be output.
	  (setq output (concat output 
			       (substring (oref this :marker-acc)
					  0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (oset this 
		:marker-acc
		(substring (oref this :marker-acc) (match-beginning 0))))
      (setq output 
	    (concat output (oref this :marker-acc)))
      (oset this :marker-acc ""))

    (jde-jdb-set-breakpoint-listener this output)
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; jdb Stack Listener                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-jdb-stack-listener (jde-db-listener)
  ((stack-depth     :initarg :stack-depth
		    :type string
		    :initform ""
		    :documentation
		    "Stack depth."))
  "Listens for changes in the current stack frame.")


;; Thanks to Michael Ernst <mernst@cs.washington.edu> for the following
;; stack-related code.
;;
;; Extract the index of the current stack frame from the jdb prompt, where
;; the prompt is of the form 
;;
;;   thread[stack_index]
;; 
;; e.g.,
;;
;;   main[1]
;;
;; The user can move the debugger up and down the stack via the up and
;; down commands. The debugger indicates the current location by the
;; stack index portion of its prompt.
(defmethod jde-db-listener-filter-output ((this jde-jdb-stack-listener) output)
  (let* ((jdb (oref this debugger))
	 (debuggee (oref jdb debuggee)))

    (if (string-match "^[-a-zA-Z0-9_$]+\\[\\([0-9]+\\)\\] " output)
	(oset debuggee :stack-depth (match-string 1 output)))
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Class of JDE Debuggers based on jdb.                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-jdb (jde-db-debugger)
  ((exec-name       :initarg :exec-name
		    :type string
		    :initform "jdb"
	            :documentation 
	            "Name of the jdb executable.")
   (path            :initarg :path
		    :type string
		    :initform "jdb"
		    :documentation
		    "Path of the jdb executable.")
   (bp-listener     :initarg :bp-listener
		    :type jde-jdb-breakpoint-listener
		    :documentation "Breakpoint listener."))
  (:allow-nil-initform t)
"Class of generic jdb debuggers")

(defmethod initialize-instance ((this jde-db-jdb) &rest fields)
  "Constructor for generic jdb debugger."
  (call-next-method)
  (oset this :name "jdb")

  ;; Install jdb versions of debugger commands.
  (oset this cmd-set (jde-jdb-cmd-set "jdb commands" :debugger this))

  (oset this bp-listener
   (jde-jdb-breakpoint-listener 
    "jdb breakpoint listener"
    :debugger this))

  (jde-db-add-listener this (oref this bp-listener))

  (jde-db-add-listener
   this
   (jde-jdb-stack-listener 
    "jdb stack listener"
    :debugger this)))

(defmethod jde-db-notify-process-exit ((this jde-db-jdb) msg)
  "The default debugger process sentinel invokes this method 
when the jdb process terminates."
  (call-next-method)
  (let* ((debuggee (oref this debuggee))
	 (debuggee-status (oref debuggee status)))
    (oset this running-p nil)
    (oset debuggee-status running-p nil)
    (oset debuggee-status stopped-p nil)
    (jde-db-set-all-breakpoints-specified)))

(defmethod jde-db-launch-arg ((this jde-db-jdb))
  "Generate the -launch option for jdb."
  (list "-launch"))


(defmethod jde-db-debugger-get-prog-args ((this jde-db-jdb))
  (cond
   ((string= (oref this exec-name) "jdb")
    (append 
     (jde-db-get-vm-args this)
     (jde-db-get-vm-args-from-user)
     (list (oref (oref this debuggee) main-class))
     jde-db-option-application-args
     (jde-db-get-app-args-from-user)))
   ((string= (oref this exec-name) "appletviewer")
    (list "-debug" 
	  (oref (oref this debuggee) main-class)))
   (t
    (error "Unrecognized jdb debugger mode."))))
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.1.x Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-db-jdb-1.1 (jde-db-jdb)
  ()
  (:allow-nil-initform t)
"Class of jdb shipped with JDK 1.1.x.")


(defmethod initialize-instance ((this jde-db-jdb-1.1) &rest fields)
  "Constructor for jdb-1.1."
  (call-next-method)
  (oset (oref this bp-listener)
   :marker-regexp
   "^Breakpoint hit: .*(\\([^\$]*\\).*:\\([0-9]*\\))")
  (oset (oref this bp-listener) :class-index 1)
  (oset (oref this bp-listener) :line-index 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.3.1 Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-old-jdb (jde-db-jdb-1.1)
  ()
  (:allow-nil-initform t)
"Class of pre-JPDA jdb shipped with post-JPDA versions of the
JDK.")

(defmethod initialize-instance ((this jde-db-old-jdb) &rest fields)
  "Constructor for old jdb."

  (call-next-method)
  (oset this :exec-name "oldjdb"))


(defclass jde-db-jdb-1.3 (jde-db-jdb)
  ()
  (:allow-nil-initform t)
  "Class of JPDA-based jdb shipped with JDK 1.3.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDK 1.4.0 Support                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-jdb-1.4 (jde-db-jdb)
  ()
  (:allow-nil-initform t)
  "Class of JPDA-based jdb shipped with J2SDK 1.4")

(defmethod initialize-instance ((this jde-db-jdb-1.4) &rest fields)
  "Constructor for jdb-1.4."
  (call-next-method)
  ;; Regular expression used to find a jdb breakpoint position marker.
  ;; The regular expression has two subexpressions. The first matches
  ;; the name of the class in which the breakpoint occurs; the second, the
  ;; line number at which the breakpoint occurs."
  (oset (oref this bp-listener)
   :marker-regexp
   "^.*: \"thread=.*\", \\(\\(.*[.]\\)*\\)\\([^$]*\\)\\($.*\\)*[.].+(), line=\\([0-9]*\\)"))

(defun jde-jdb-get-jdb ()
  "Gets the version of jdb specified for the
current project."
  (let (jdb)
    (cond
     ((string= (car jde-debugger) "jdb")
      (cond 
       ((and (< (jde-java-major-version) 2)
	     (< (jde-java-minor-version) 2))
	(setq jdb (jde-db-jdb-1.1 "jdb 1.1")))
       ((and (< (jde-java-major-version) 2)
	     (= (jde-java-minor-version) 3))
	(setq jdb (jde-db-jdb-1.3 "jdb 1.3")))
       (t
	(setq jdb (jde-db-jdb-1.4 "jdb 1.4")))))       
     ((string= (car jde-debugger) "old jdb")
      (if (and (< (jde-java-major-version) 2)
	       (< (jde-java-minor-version) 2))
	  (setq jdb (jde-db-jdb-1.1 "jdb 1.1"))
	(setq jdb (jde-db-old-jdb "old jdb"))))
     (t
      (error "%s is not a valid jdb debugger choice." 
	     (car jde-debugger))))
    (oset 
     jdb 
     :path (jde-get-jdk-prog (oref jdb :exec-name)))
    jdb))


;;;###autoload		   
(defun jde-jdb ()
  "Run jdb on Java application whose source resides in the current buffer.
This command determines the main class of the application either from
the variable `jde-run-application-class' or from the source in the current 
buffer. If `jde-run-application-class' does not specify a class, the main class
is assumed to be the class defined by the current source buffer. This command
creates a command buffer for the debug session."   
  (interactive)
  (let ((debugger (jde-jdb-get-jdb))
	(debuggee 
	 (jde-db-debuggee 
	  "debuggee"
	  :main-class
	  (let ((main-class jde-run-application-class))
	    (if (or
		 (not main-class)
		 (string= main-class ""))
		(setq main-class
		      (if (buffer-file-name)
			  (concat (jde-db-get-package)
				  (file-name-sans-extension 
				   (file-name-nondirectory (buffer-file-name))))
			(read-string "Java class to debug: ")))
	      main-class)))))
    (oset debugger the-debugger debugger)
    (oset debugger :debuggee debuggee)
    (jde-db-debugger-launch debugger)))

(defun jde-jdb-attach ()
  (interactive)
  (let ((debugger (jde-jdb-get-jdb))
	(debuggee 
	 (jde-db-debuggee 
	  "debuggee"
	  :address 
	  (if jde-db-option-connect-address
	      jde-db-option-connect-address
	    (if (eq system-type 'windows-nt)
		"javadebug"
	      "4444")))))
    (oset debugger the-debugger debugger)
    (oset debugger :debuggee debuggee)
    (jde-db-debugger-connect debugger)))


(defun jde-jdb-listen ()
  (interactive)
  (let ((debugger (jde-jdb-get-jdb))
	(debuggee 
	 (jde-db-debuggee 
	  "debuggee"
	  :address 
	  (if jde-db-option-connect-address
	      jde-db-option-connect-address
	    (if (eq system-type 'windows-nt)
		"javadebug"
	      "4444")))))
    (oset debugger the-debugger debugger)
    (oset debugger :debuggee debuggee)
    (jde-db-debugger-connect debugger t)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Debug Commands                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar jde-jdb-menu-spec
  (list "Jdb"

	["Step Over"                  
	 jde-debug-step-over
	 :active (jde-db-debuggee-stopped-p)]

	["Step Into"                  
	 jde-debug-step-into
	 :active (jde-db-debuggee-stopped-p)]

	["Step Out"                  
	 jde-debug-step-out
	 :active (jde-db-debuggee-stopped-p)]

	["Run"
	 jde-debug-run
	 :active   (and
		    (slot-boundp 'jde-db-debugger 'the-debugger)
		    (let* ((debugger (oref 'jde-db-debugger the-debugger))
			   (debuggee (oref debugger debuggee))
			   (debuggee-status (oref debuggee status)))
		      (and (oref debugger running-p)
			   (not (oref debuggee-status running-p)))))
			
	 :included (or 
		    (not (slot-boundp 'jde-db-debugger 'the-debugger))
		    (let* ((debugger (oref 'jde-db-debugger the-debugger))
			   (debuggee (oref debugger debuggee))
			   (debuggee-status (oref debuggee status)))
		      (or (not (oref debugger running-p))
			  (not (oref debuggee-status running-p)))))]

        ["Continue"                   
	 jde-debug-cont 
	 :active   (or 
		    (jde-db-debuggee-stopped-p)
		    (jde-db-debuggee-suspended-p))

	 :included  (jde-db-debuggee-running-p)]

	["Quit"
	 jde-debug-quit
	 :active  (jde-db-debuggee-running-p)]

	"-"

        ["Toggle Breakpoint"          
	 jde-debug-toggle-breakpoint 
	 t]

        ["Clear Breakpoints"          
	 jde-debug-clear-breakpoints 
	 jde-db-breakpoints]

	"-"

	["Display Variable"                  
	 jde-jdb-cmd-print
	 nil]


	(list
	 "Stack"

	 ["Up"                        
	  jde-debug-up
	  :active  (jde-db-debuggee-stopped-p)]

	 ["Down"                      
	  jde-debug-down
	  :active (and
		   (jde-db-debuggee-stopped-p)
		   (let* ((debugger (oref 'jde-db-debugger the-debugger))
			  (debuggee (oref debugger debuggee)))
		     (> (string-to-int (oref debuggee :stack-depth)) 1)))]

	 ["Where"                      
	  jde-debug-where
	  :active (jde-db-debuggee-stopped-p)]

	 )
	"-"
	(list
	 "Exernal Process"
	 ["Attach to"
	  jde-jdb-attach
	  :active (not (jde-db-debuggee-running-p))]
	 ["Listen for"
	  jde-jdb-listen
	  :active (not (jde-db-debuggee-running-p))]
	 )	 
	"-"
	["Preferences"                
	 jde-bug-show-preferences nil]
	"-"
	["Help"                       
	 jde-bug-help nil]
	)
  "Defines the JDE's menu of jdb commands.")

(defvar jde-jdb-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define jde-jdb-menu km "JDEbug Minor Mode Menu"
                      jde-jdb-menu-spec)
    km)
  "Keymap for JDEbug minor mode.")

(defvar jde-jdb-minor-mode nil
  "Non-nil if jdb minor mode is enabled.")
(make-variable-buffer-local 'jde-jdb-minor-mode)

(defun jde-jdb-minor-mode (&optional arg)
  "Toggle jdb minor mode.
With prefix argument ARG, turn on if positive, otherwise off..

\\{jde-jdb-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if jde-jdb-minor-mode 0 1))))

  (setq jde-jdb-minor-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not jde-jdb-minor-mode)))

  (if jde-jdb-minor-mode
      (if (featurep 'xemacs)
            (easy-menu-add jde-jdb-menu-spec jde-jdb-mode-map))
    (if (featurep 'xemacs)
      (easy-menu-remove jde-jdb-menu-spec))))

(semantic-add-minor-mode 'jde-jdb-minor-mode " jdb" jde-jdb-mode-map)


;; (fmakunbound 'jde-jdb-key-bindings)
(defcustom jde-jdb-key-bindings
  (list (cons "[?\C-c ?\C-a ?\C-s]" 'jde-debug-step-over)
	(cons "[?\C-c ?\C-a ?\C-n]" 'jde-debug-step-into)
	(cons "[?\C-c ?\C-a ?\C-o]" 'jde-debug-step-out)
	(cons "[?\C-c ?\C-a ?\C-c]" 'jde-debug-cont)
	(cons "[?\C-c ?\C-a ?\C-r]" 'jde-debug-run)
	(cons "[?\C-c ?\C-a ?\C-b]" 'jde-debug-toggle-breakpoint)
	(cons "[?\C-c ?\C-a ?\C-u]" 'jde-debug-up)
	(cons "[?\C-c ?\C-a ?\C-d]" 'jde-debug-down))
  "*Specifies key bindings for jdb debug commands.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  ;; Unmap existing key bindings
	  (if (and
	       (boundp 'jde-jdb-key-bindings)
	       jde-jdb-key-bindings)
	      (mapc 
	       (lambda (binding)
		 (let ((key (car binding))
		       (fcn (cdr binding)))
		   (if (string-match "\\[.+]" key)
		       (setq key (car (read-from-string key))))
		   (define-key jde-jdb-mode-map key nil)))
	       jde-jdb-key-bindings))
	  ;; Map new key bindings.
	  (mapc 
	   (lambda (binding)
	     (let ((key (car binding))
		   (fcn (cdr binding)))
	       (if (string-match "\\[.+]" key)
		   (setq key (car (read-from-string key))))
	       (define-key jde-jdb-mode-map key fcn)))
	   val)
	  (set-default sym val)))


(defun jde-jdb-applet-init (applet-class applet-doc-path)
  (let* ((debug-buf-name (concat "*debug-" applet-class "*"))
	 (applet-doc (file-name-nondirectory applet-doc-path))
	 (applet-doc-directory (file-name-directory applet-doc-path))
	 (source-directory default-directory)
	 (working-directory 
	  (if applet-doc-directory
	      applet-doc-directory
	    source-directory))
	 (debugger (jde-jdb-get-jdb))
	 (debuggee
	  (jde-db-debuggee
	   "applet"
	   :main-class applet-doc)))
    (oset debugger :debuggee debuggee)
    (oset 
     debugger
     :path (jde-get-jdk-prog 'appletviewer))
    (oset debugger :exec-name "appletviewer")
    (jde-db-debugger-launch debugger)))
		   
	  
   
(defun jde-jdb-applet-internal (applet-doc)
  (let ((applet-class jde-run-application-class))
    (if (or
	 (not applet-class)
	 (string= applet-class ""))
	(setq applet-class
	      (concat (jde-db-get-package)
		      (file-name-sans-extension 
		       (file-name-nondirectory (buffer-file-name))))))
    (jde-jdb-applet-init applet-class  applet-doc)))

;;;###autoload 
(defun jde-jdb-applet (&optional doc) 
  "Runs an applet in the jdb debugger. This function prompts you to enter
the path of an html document that displays the applet. If you 
do not enter a path, this function next checks
whether `jde-run-applet-doc' specifies a document. If so, it displays
that specified document. Next, it checks whether the current directory
contains any html files. If so, it displays the first html file that
it finds. If if cannot find an html file, it signals an error.  This
function runs appletviewer in jdb to permit debugging. On startup, it
sets a breakpoint in the init method of the class specified by 
`jde-run-application-class' or in the class corresponding to the Java
file in the current buffer."
  (interactive
   (let ((insert-default-directory nil))
     (list (read-file-name "Applet doc: " nil nil nil jde-run-applet-last-doc))))
  (setq jde-run-applet-last-doc doc)
  (let ((applet-doc-path 
	 (if doc 
	     doc
	   (if (and jde-run-applet-doc
		    (not (string= jde-run-applet-doc "")))
	       jde-run-applet-doc
	     (car (jde-run-find-html-files))))))
    (if applet-doc-path 
	(jde-jdb-applet-internal applet-doc-path) 
      (signal 'error "Could not find html document to display applet."))))


(defun jde-jdb-menu-debug-applet ()
  (interactive)
  (jde-jdb-applet))

(provide 'jde-jdb)

;; Change History
;; $Log: jde-jdb.el,v $
;; Revision 1.19  2002/11/11 05:24:26  paulk
;; No need to add .exe to jdb path thanks to Mac compatibility fix.
;;
;; Revision 1.18  2002/11/05 07:56:20  paulk
;; Mac OS X (darwin) compatibility fix: find paths of jdb and appletviewer on the Mac. Thanks to Andrew Hyatt.
;;
;; Revision 1.17  2002/10/16 04:59:57  paulk
;; Debug cursor now works in files that do not belong to a package. Thanks to Andy Piper.
;;
;; Revision 1.16  2002/06/17 07:24:08  paulk
;; Updated the JDEE's applet debugging command to
;; work with its new jdb interface.
;;
;; Revision 1.15  2002/05/21 06:35:20  paulk
;; Updated to support J2SDK 1.4.0 version of jdb.
;;
;; Revision 1.14  2002/03/06 13:00:18  paulk
;; * Removed references to obsolete jde-db-option-attach variable.
;; * The jdb launch, attach, and listen commands now update the
;;   the-debugger field in the jde-db-debugger class.
;;
;; Revision 1.13  2002/03/04 06:43:41  paulk
;; Adds support for connecting debugger to an independently started
;; process, using either attach or listen mode.
;;
;; Revision 1.12  2002/02/08 12:04:00  paulk
;; Completed implementation of step-into and step-out commands.
;;
;; Revision 1.11  2002/02/04 05:47:17  paulk
;; Added code to rehighlight breakpoints if the user kills a
;; buffer for a source file that contains breakpoints and
;; then reopens the file.
;;
;; Revision 1.10  2002/01/15 13:34:24  paulk
;; Adds a Clear Breakpoints command for jdb.
;;
;; Revision 1.9  2002/01/14 13:33:57  paulk
;; - Now defines three breakpoint marker colors: green for a specified breakpoint,
;;   yellow for a requested breakpoint, and red for an enabled breakpoint.
;;
;; - The debug application command now requests all specified
;;   breakpoints at the beginning of a debug session.
;;
;; - The debug application command now changes the color of all breakpoints
;;   to green (specified) at the end of a debug session.
;;
;; Revision 1.8  2002/01/11 05:45:23  paulk
;; - Use overlays/extents to record location of breakpoints in a buffer.
;; - Use different colors to indicate requested and enabled breakpoints.
;;
;; Revision 1.7  2002/01/02 05:34:31  paulk
;; * Fixed some bugs in jdb stack navigation commands.
;; * Moved the where command out processing from the jdb stack listener
;;   to the whre cmd response method where it belongs.
;; * Added key bindings for jdb commands.* Fixed some bugs in jdb stack navigation commands.
;;
;; Revision 1.6  2001/12/31 07:54:39  paulk
;; Implemented jdb versions of generalized
;; quit, step-over, step-into, stack up, stack down,
;; and stack where commands.
;;
;; Revision 1.5  2001/12/28 05:35:45  paulk
;; * Implemented jdb versions of generalized stop and continue process commands.
;;
;; * Implemented breakpoint and stack message listeners.
;;
;; Revision 1.4  2001/12/17 08:07:47  paulk
;; jdb implementation of generalized clear breakpoint command.
;;
;; Revision 1.3  2001/12/10 04:29:54  paulk
;; Created generalized breakpoint framework. Provided initial
;; implementation for jdb. A lot of work remains.
;;
;; Revision 1.2  2001/12/04 06:05:36  paulk
;; Removed carriage returns.
;;
;; Revision 1.1  2001/12/04 05:25:40  paulk
;; Initial revision.
;;

;;; end of jde-jdb.el
