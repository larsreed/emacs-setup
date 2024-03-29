;; jde-open-source.el -- Open class source files
;;
;; $Revision: 1.8 $
;;
;; Author: Klaus Berndl

;; Keywords: java, open files

;; Copyright (C) 2002 Klaus Berndl

;; This package follows the GNU General Public Licence (GPL), see the
;; COPYING file that comes along with GNU Emacs. This is free software,
;; you can redistribute it and/or modify it under the GNU GPL terms.
;;
;; Java is a registered trademark of Sun Microsystem, Inc.
;;
;;; Commentary:

;; This is one of a set of packages that make up the
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; This package allows to open the class at point.

;; Known bugs/problems :
;;
;; TODO
;;
;; The latest version of the JDE is available at
;; <URL:http://jde.sunsite.dk>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@mediaone.net

(require 'jde-parse)
(require 'jde-util)
(require 'senator)

(defcustom jde-open-class-at-point-find-file-function 'find-file-other-window
  "Define the function for opening the class at point. See
`jde-open-class-at-point'`. Default is `find-file-other-window'. A function
defined here must have the same signature as `find-file' means the first
argument is the filename and the second optional argument is a
wildcard-pattern."
  :group 'jde-project
  :type '(function :tag "Function to open class at point"))

(defvar jde-open-cap-ff-function-temp-override nil
  "Maybe some tools needs to temporally override the value of
`jde-open-class-at-point-find-file-function'. Cause of the auto. resetting
mechanism of JDE for defcustom-variables this is not possible with the
defcustom version. So, if you need to override the value of
`jde-open-class-at-point-find-file-function' from within your elisp code you
can use the variable `jde-open-cap-ff-function-temp-override'.
`jde-open-class-at-point' checks first if this variable is not nil and uses
then this value. Only if this variable is nil it uses the value of
`jde-open-class-at-point'!
This variable is NOT for user customizing, but only for use within elisp!")

(defmacro jde-with-file-contents (file &rest body)
  "If FILE exists and is readable creates a temporary buffer with the contents
of FILE, points to beginning of buffer, evaluates BODY and return the value of
the last form of BODY. If FILE does not exist or is not readable nil is
returned.
Note: No major/minor-mode is activated and no local variables are evaluated
for FILE, but proper EOL-conversion and charcater interpretation is done!"
  (let ((exp-filename (make-symbol "exp-filename")))
    `(let ((,exp-filename (expand-file-name ,file)))
       (if (and (file-exists-p ,exp-filename)
                (file-readable-p ,exp-filename))
           (with-temp-buffer
             (insert-file-contents ,exp-filename)
             (beginning-of-buffer)
             ,@body)
         nil))))

(defun jde-open-get-class-to-open (pair parsed-symbol)
  "Evaluates PARSE-SYMBOL to check if it is a variable name or a class name.
If this fails point is on a method or an attribute of a class in the current
buffer or in a superclass. In this cases we check first if the parsed-symbol
is a possible member of the current class(\"this\") and if this fails it
checks if it is a member of the base class(\"super\")."
 (if (and (stringp (car pair)) (> (length (car pair)) 0))
     ;; if we got a pair all should work fine.
     (jde-parse-eval-type-of (car pair))
   (or (condition-case () (jde-parse-eval-type-of parsed-symbol)
         (error nil))
       (if (jde-parse-find-completion-for-pair `("this" ,parsed-symbol) nil
                                               jde-complete-private)
           (jde-parse-eval-type-of "this")
         nil)
       (if (jde-parse-find-completion-for-pair `("super" ,parsed-symbol) nil
                                               jde-complete-private)
           (jde-parse-eval-type-of "super")
          nil))))

(defun jde-open-get-path-prefix-list () 
  "Builds all the path prefixes used to search for the full qualified
source file. For this method to work `jde-sourcepath' needs to be set."
  (if jde-sourcepath
      (append (jde-normalize-paths jde-sourcepath 'jde-sourcepath))
    (error (concat "For this method to work the variable "
                   "jde-sourcepath needs to be set"))))

(defun jde-open-functions-exist ()
  "Checks if the functions `jde-parse-java-variable-at-point',
`jde-parse-eval-type-of', and `jde-parse-find-completion-for-pair' are defined"
  (and (fboundp 'jde-parse-java-variable-at-point)
       (fboundp 'jde-parse-eval-type-of)
       (fboundp 'jde-parse-find-completion-for-pair)))

(defun jde-open-get-java-file-name (classname) 
  "Converts a class name into a relative file name."
  (concat "/" (substitute ?/ ?. classname) ".java"))

(defun jde-open-find-java-file-name (class-name prefix-list)
  "Looks for a the absolute file name of CLASS-FILE-NAME by looping 
through the PREFIX-LIST"
  (let ((class-file-name (jde-open-get-java-file-name class-name))
        source-path-prefix java-file-name temp-file-name)
    (while (and (not java-file-name) prefix-list)
      (setq source-path-prefix (car prefix-list)
            prefix-list (cdr prefix-list))
      (setq temp-file-name (concat source-path-prefix class-file-name))
      (when (and (file-exists-p temp-file-name)
                 (file-readable-p temp-file-name))
        (setq java-file-name temp-file-name)))
    
    ;; if we have not found a file we try to match
    ;; sourcecode-file in the current directory with the package
    ;; definition. This situation can occur if a user does not
    ;; define a root path but instead he has defined "."
    ;; (current dir) as a root-path.
    (if (not java-file-name)
        (let ((filename (expand-file-name
                         (file-name-nondirectory class-file-name))))
          ;; check if the package statement in the java-file matches
          ;; the package-part of class-name. If yes we have found
          ;; our source, otherwise not.
          (if (jde-with-file-contents
               filename
               (re-search-forward (concat "package[ \t\n]+"
                                          (progn
                                            (string-match
                                             "^\\(.+\\)\\."
                                             class-name)
                                            (match-string
                                             1 class-name))
                                          "[ \t\n]*;") nil t))
              (setq java-file-name filename))))
      java-file-name))

(defun jde-open-jump-to-class (parsed-symbol class-name java-file-name) 
  "Opens JAVA-FILE-NAME and place the cursor in the parsed variable"
  (let* ((tokens (semantic-token-type-parent-superclass
		 (semantic-current-nonterminal-of-type 'type)))
        (super-class (car tokens))
        (first-time t))
    
    ;; if the current buffer contains java-file-name do not try to
    ;; open the file
    (if (not (string-equal (buffer-file-name) java-file-name))
        (funcall (or jde-open-cap-ff-function-temp-override
                     jde-open-class-at-point-find-file-function)
                 java-file-name))
    ;; Now let�s jump to the thing-of-interest. If this is a
    ;; variable-name then we will not find this with senator in
    ;; the opened java-file so we search for the definiton of
    ;; the class itself. This feature is only available if we
    ;; have senator!
    (when (fboundp 'senator-search-forward)
      (beginning-of-buffer)
      (senator-parse)
      (setq parsed-symbol (concat "\\b" parsed-symbol "\\b"))
      (while (not (senator-re-search-forward parsed-symbol nil t))
        ;; searching for the thing-of-interest has failed 
        ;; let's try in the base class
          (progn
            (jde-show-superclass-source-2 tokens)
            (beginning-of-buffer)
            (senator-parse)
            (setq tokens (semantic-token-type-parent-superclass
                          (semantic-current-nonterminal-of-type 'type)))
            ;;if it is the first time try in the class definition
            ;;itself.
            (if first-time
                (progn 
                  (setq first-time nil)
                  (senator-re-search-forward
                   (progn
                     (string-match ".*\\.\\([^.]+\\)$"
                                   (concat "." class-name))
                     (match-string 1 (concat "." class-name)))
                   nil t)))
            (if (not super-class)
                (error "Method not found"))
            (setq super-class (car tokens)))))))

(defun jde-open-class-at-point ()
  "Opens the java-file which defines the class where current point is and
jumps to the definition of current thing at point \(this can be a
variablename, classname, methodname, attributename). This function needs the
same requirements to work as the field/method-completion-feature in JDE \(see
`jde-complete-at-point')!. The source-file is searched first in
`jde-sourcepath', then in `jde-global-classpath', then in
$CLASSPATH, then in current-directory."
  (interactive)
  (if (jde-open-functions-exist)
      (let* ((thing-of-interest (thing-at-point 'symbol))
             (pair (save-excursion (end-of-thing 'symbol)
                                   (jde-parse-java-variable-at-point)))
             (class-to-open (jde-open-get-class-to-open
                             pair thing-of-interest))
             (source-path-prefix-list (jde-open-get-path-prefix-list)) 
             found java-file-name)
        (if (and class-to-open (stringp class-to-open))
            (progn
              (setq java-file-name (jde-open-find-java-file-name
                                    class-to-open source-path-prefix-list))
              (if (not java-file-name)
                  (error "Can not find the sourcecode-file for \"%s\""
                         thing-of-interest)
                ;; we have found the java-sourcefile. So let�s open it and
                ;; then jump to the thing-of-interest
                (jde-open-jump-to-class thing-of-interest
                                        class-to-open
                                        java-file-name)))
          (error "Can not parse the thing at point!")))
    (message "You need JDE >= 2.2.6 and Senator for using this feature!")))

(defun jde-open-class-source ( &optional unqual-class )
  "Displays source of the class whose name appears at point in the current
Java buffer. This command finds only classes that reside in the source paths
specified by `jde-sourcepath'. You should provide a global setting
for this variable in your .emacs file to accommodate source files that are
not associated with any project."
  (interactive)
  (condition-case err
      (let* ((unqualified-name 
 	      (or unqual-class
		  (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
 	     (class-names 
 	      ;;expand the names into full names, or a list of names
 	      (jde-jeval-r 
 	       (concat 
 		"jde.util.JdeUtilities.getQualifiedName(\"" 
 		unqualified-name "\");"))))
 	;;Check return value of QualifiedName
 	(if (or (eq class-names nil)
		(not (listp class-names)))
 	    (error "Cannot find %s" unqualified-name))
	;; Turn off switching project settings to avoid 
	;; resetting jde-sourcepath.
	(let ((old-value jde-project-context-switching-enabled-p))
	  (setq jde-project-context-switching-enabled-p nil)
	  ;;If the list is only one long
	  (if (eq 1 (length class-names))
	      ;;then show it
	      (progn(other-window 1)
		    (jde-find-class-source (car class-names)))
	     	  ;;else let the user choose
	    (let ((class (efc-query-options class-names "Which class?")))
		  (if class
		      (jde-find-class-source class))))
	  (setq jde-project-context-switching-enabled-p old-value)))
    (error
     (message "%s" (error-message-string err)))))

(defalias 'jde-show-class-source 'jde-open-class-source)

;; Thanks to Sandip Chitale <sandip.chitale@blazesoft.com>
(defun jde-show-superclass-source-2 (tokens)
  (if tokens
      (if (= (length tokens) 1)
          (jde-show-class-source (car tokens))
        (let ((parent (efc-query-options tokens "Which super class?")))
          (if parent
              (jde-show-class-source parent))))
    (jde-show-class-source "Object")))

(defun jde-show-superclass-source () 
  "Show the source for the parent of the class at point."
  (interactive)
  (let ((tokens (semantic-token-type-parent-superclass
		 (semantic-current-nonterminal-of-type 'type))))
    (jde-show-superclass-source-2 tokens)))
;; Thanks to Sandip Chitale <sandip.chitale@blazesoft.com>
    
(defun jde-show-interface-source () 
  "Show the source for the interface implemented by the class at point.
If the class implements more than one interface, this command prompts
you to select one of the interfaces to show."
  (interactive)
  (let ((tokens (semantic-token-type-parent-implement
		 (semantic-current-nonterminal-of-type 'type))))
    (if tokens
	(if (= (length tokens) 1)
	    (jde-show-class-source (car tokens))
	  (let ((interface (efc-query-options tokens "Which interface?")))
	    (if interface 
		(jde-show-class-source interface)))))))


(provide 'jde-open-source)

;; $Log: jde-open-source.el,v $
;; Revision 1.8  2002/08/22 04:15:17  jslopez
;; Fixes jde-open-class-at-point to loop through
;; all the super classes looking for the given token.
;;
;; Revision 1.7  2002/07/27 13:21:30  jslopez
;; Fixes regression in jde-open-class-at-point.
;;
;; Revision 1.6  2002/07/27 13:03:19  jslopez
;; Substitute obsolete call to jde-open-base-class-source.
;;
;; Revision 1.5  2002/07/01 04:52:11  paulk
;; - Moved jde-open-class-source, jde-show-superclass-source, jde-show-interface-source from jde-help.el
;;   to jde-open-source.el.
;;
;; - Removed jde-open-source-for-symbol because it has been superceded by jde-open-class-at-point.
;;

;; end jde-open-source.el
