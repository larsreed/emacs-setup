;;; JDE-CHECK.EL --- Checkstyle interface for JDE


;; Copyright (C) 2001 Markus Mohnen


;; Author: Markus Mohnen <mohnen@informatik.rwth-aachen.de>
;; Maintainer: Markus Mohnen <mohnen@informatik.rwth-aachen.de>
;; Created: 06 Jun 2001
;; Version: 1.1
;; - minor bug fix to be comparible with recent versions of jde
;;
;; Version: 1.0
;;
;; Keywords:



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
;; <mohnen@informatik.rwth-aachen.de>) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;; LCD Archive Entry:
;; jde-check|Markus Mohnen|<mohnen@informatik.rwth-aachen.de>
;; |Checkstyle interface for JDE
;; |$Date$|$Revision$|~/packages/jde-check.el


;;; Commentary:


;;; This package provides an interface from JDE (see
;;; http://jde.sunsite.dk/) to checkstyle (see
;;; http://www.geocities.com/oburn/checkstyle/) an `development tool
;;; to help programmers write Java code that adheres to a coding
;;; standard'.


;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jde-check)


;;; Usage:
;;
;;  M-x `jde-check' to check the java file in the current buffer.
;;


;;; Customization:
;;
;;  M-x `jde-check-customize' to customize all the jpack options.


;;; Code:


(require 'jde-compile)


(if (fboundp 'jde-build-classpath)
    nil
  (require 'jde-run)
  (defalias 'jde-build-classpath 'jde-run-build-classpath-arg)
  )


(defconst jde-check-version "1.1")


(defgroup jde-check nil
  "JDE Checker Options"
  :group 'jde
  :prefix "jde-check-option-")


(defcustom jde-checker-class "com.puppycrawl.tools.checkstyle.Main"
  "*Java checker class.
Specifies the class of the the program to be used to check the source
in the current buffer. The default is the checkstyle program."
  :group 'jde-check
  :type 'string)


(defcustom jde-checker-classpath nil
  "*Specify paths of classes required to run the jde-checker application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-check
  :type '(repeat (file :tag "Path")))


(defcustom jde-read-check-args nil
"*Specify whether to prompt for additional checker arguments.
If this variable is non-nil, the jde-check command prompts
you to enter additional checker arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments
entered in the minibuffer."
  :group 'jde-check
  :type 'boolean)


(defvar jde-interactive-check-args ""
"String of checker arguments entered in the minibuffer.")


(defvar jde-interactive-compile-arg-history nil
"History of checker arguments entered in the minibuffer.")


(defcustom jde-check-option-command-line-args ""
  "*Specify options as a string of command-line arguments.
The value of this variable should be a string of switches understood
by the checker. This variable is intended to be used to set check
options not otherwise defined by the JDE, in particular, options
not defined by checkstyle but used by another checker that you might
want to use with the JDE."
  :group 'jde-check
  :type 'string)


(defcustom jde-check-option-allow-tabs nil
  "*Indicates whether to allow tabs."
  :group 'jde-check
  :type 'boolean)


(defcustom jde-check-option-allow-protected nil
  "*Indicates whether to allow protected data."
  :group 'jde-check
  :type 'boolean)


(defcustom jde-check-option-allow-noauthor nil
  "*Indicates whether to allow no @author tag to be defined for class and interface Javadoc comments."
  :group 'jde-check
  :type 'boolean)


(defcustom jde-check-option-allow-maxlinelen 80
  "*Specifies the maximum line length."
  :group 'jde-check
  :type 'integer)


(defcustom jde-check-option-pattern-member ""
  "*Specifies the regular expression to match against member variables.
If empty, use checkstyle defaults."
  :group 'jde-check
  :type 'string)


(defcustom jde-check-option-pattern-parameter ""
  "*Specifies the regular expression to match against parameters.
If empty, use checkstyle defaults."
  :group 'jde-check
  :type 'string)


(defcustom jde-check-option-pattern-const ""
  "*Specifies the regular expression to match against static/final variables.
If empty, use checkstyle defaults."
  :group 'jde-check
  :type 'string)


(defcustom jde-check-option-pattern-static ""
  "*Specifies the regular expression to match against static variables.
If empty, use checkstyle defaults."
  :group 'jde-check
  :type 'string)


(defcustom jde-check-option-pattern-type ""
  "*Specifies the regular expression to match against type names.
If empty, use checkstyle defaults."
  :group 'jde-check
  :type 'string)


(defcustom jde-check-option-header-file ""
  "*Specifies the file containing the header lines."
  :group 'jde-check
  :type 'file)


(defcustom jde-check-option-header-ignoreline nil
  "*Specifies the line in the header to ignore when comparing."
  :group 'jde-check
  :type 'boolean)


(defcustom jde-check-option-javadoc-relax nil
  "*Specifies whether to relax checking Javadoc comments."
  :group 'jde-check
  :type 'boolean)


(defcustom jde-check-option-ignore-imports nil
  "*Specifies whether to ignore checking import statements."
  :group 'jde-check
  :type 'boolean)


(defun jde-get-check-options ()
"Constructs a command-line argument string for checker.
The string consists of the contents of the jde-check-options
variable concatenated with the various jde-check-option
settings.
"
  (let (options)
    (if jde-check-option-allow-tabs
        (setq options (concat options " -Dcheckstyle.allow.tabs=yes")))
    (if jde-check-option-allow-protected
        (setq options (concat options " -Dcheckstyle.allow.protected=yes")))
    (if jde-check-option-allow-noauthor
        (setq options (concat options " -Dcheckstyle.allow.noauthor=yes")))
    (setq options (concat options " -Dcheckstyle.maxlinelen="
                          (int-to-string jde-check-option-allow-maxlinelen)))
    (if (not (string= "" jde-check-option-pattern-member))
        (setq options (concat options " -Dcheckstyle.pattern.member=\""
                              jde-check-option-pattern-member "\"")))
    (if (not (string= "" jde-check-option-pattern-member))
        (setq options (concat options " -Dcheckstyle.pattern.member=\""
                              jde-check-option-pattern-member "\"")))
    (if (not (string= "" jde-check-option-pattern-parameter))
        (setq options (concat options " -Dcheckstyle.pattern.parameter=\""
                              jde-check-option-pattern-parameter "\"")))
    (if (not (string= "" jde-check-option-pattern-const))
        (setq options (concat options " -Dcheckstyle.pattern.const=\""
                              jde-check-option-pattern-const "\"")))
    (if (not (string= "" jde-check-option-pattern-static))
        (setq options (concat options " -Dcheckstyle.pattern.static=\""
                              jde-check-option-pattern-static "\"")))
    (if (not (string= "" jde-check-option-pattern-type))
        (setq options (concat options " -Dcheckstyle.pattern.type=\""
                              jde-check-option-pattern-type "\"")))
    (if (not (string= "" jde-check-option-header-file))
        (setq options (concat options " -Dcheckstyle.header.file=\""
                              jde-check-option-pattern-type "\"")))
    (if jde-check-option-header-ignoreline
        (setq options (concat options " -Dcheckstyle.header.ignoreline=yes")))
    (if jde-check-option-javadoc-relax
        (setq options (concat options " -Dcheckstyle.javadoc.relax=yes")))
    (if jde-check-option-ignore-imports
        (setq options (concat options " -Dcheckstyle.ignore.imports=yes")))
    (if jde-check-option-ignore-imports
        (setq options (concat options " -Dcheckstyle.ignore.imports=yes")))
    ;;todo

    (if (not (string= jde-check-option-command-line-args ""))
        (setq options (concat options " " jde-check-option-command-line-args)))
    (if jde-checker-classpath
        (setq options
              (concat options " -classpath "
                      (jde-build-classpath jde-checker-classpath)))
      (if jde-global-classpath
          (setq options
                (concat options " -classpath "
                        (jde-build-classpath jde-global-classpath)))))
    options))



;;;###autoload
(defun jde-check-customize ()
  "Customization of the group jde-check."
  (interactive)
  (customize-group "jde-check"))



(defun jde-make-check-command (more-args)
  "Constructs the java checker command as: jde-checker + options + buffer file name."
  (concat (if (and (eq system-type 'windows-nt) (string= jde-run-java-vm "java"))
              jde-run-java-vm-w
            jde-run-java-vm)
          (jde-get-check-options)
          (if (not (string= more-args "")) (concat " " more-args))
          " "
          jde-checker-class
          " "
          (file-name-nondirectory buffer-file-name)))


;;;###autoload
(defun jde-check ()
  "Checks the Java program in the current buffer.
This command invokes the checker specified by `jde-checker-class'
with the options specified by the JDE customization variables
that begin with `jde-check'. If the variable
`jde-read-check-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)


  (if jde-read-check-args
      (setq jde-interactive-check-args
              (read-from-minibuffer
               "Check args: "
               jde-interactive-check-args
               nil nil
               '(jde-interactive-check-arg-history . 1))))


  (let ((check-command
         (jde-make-check-command jde-interactive-check-args)))

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-check from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
             (not jde-xemacsp))
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-check
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (compile-internal check-command "No more errors")))


(provide 'jde-check)


;;; JDE-CHECK.EL ends here
