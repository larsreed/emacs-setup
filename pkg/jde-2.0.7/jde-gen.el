;;; jde-gen.el -- Integrated Development Environment for Java.
;; $Revision: 1.12 $ 

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
;;

(require 'tempo)

(defgroup jde-gen nil
  "JDE Autocoder"
  :group 'jde
  :prefix "jde-gen-")


(defun jde-gen-lookup-named (name)
  "Lookup some saved data under the name NAME.
Returns the data if NAME was found, and nil otherwise."
  (cdr (assq name tempo-named-insertions)))

(defun jde-gen-read-template (strings)
  "Converts an autocode template represented as a list
of strings to a list of Lisp objects as required by
tempo."
  (let ((template-string "")
	(n (length strings))
	(i 0))
    (while (< i n)
      (setq template-string
		  (concat template-string " " (nth i strings)))
      (setq i (1+ i)))
      (setq template-string
	    (concat "'(" template-string ")"))
      (eval (car (read-from-string template-string)))))

(defcustom jde-gen-buffer-boilerplate nil
"*Lines of boilerplate text to put at the head of a buffer template."
  :group 'jde-gen
  :type '(repeat (string :tag "Line")))

(defcustom jde-gen-boilerplate-function 'jde-gen-create-buffer-boilerplate
"*Specifes buffer boilerplate text function.
This variable specifies a function to create boilerplate text for
insertion at the head of Java source buffers generated by JDE
templates. The specified function should take no arguments and should
return a text string.  The default value is
`jde-gen-create-buffer-boilerplate', which returns the lines of text
specified by `jde-gen-buffer-boilerplate'."
  :group 'jde-gen
  :type 'function)

(defun jde-gen-create-buffer-boilerplate ()
"This function creates buffer boilerplate from the
variable `jde-gen-buffer-boilerplate."
  (if jde-gen-buffer-boilerplate
      (let ((bp "")
	    (n (length jde-gen-buffer-boilerplate))
	    (i 0))
	(while (< i n)
	  (setq bp
		(concat bp (elt jde-gen-buffer-boilerplate i) "\n"))
	  (setq i (1+ i)))
	bp)))

(defcustom jde-gen-class-buffer-template
  '(
    "(funcall jde-gen-boilerplate-function) 'n"
    "\"/**\" 'n"
    "\" * \"" 
    "(file-name-nondirectory buffer-file-name) 'n"
    "\" *\" 'n"
    "\" *\" 'n"
    "\" * Created: \" (current-time-string) 'n"
    "\" *\" 'n"
    "\" * @author \" (user-full-name) 'n"
    "\" * @version\" 'n"
    "\" */\" 'n>"
    "'n>"
    "\"public class \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\" {\" 'n> 'n>"
    "\"public \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\"() {\" 'n>"
    "\"        \" 'n>"
    "\"}\" 'n>"
    "'n>"
    "\"} // \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "'n>")
  "*Template for new Java class.
Setting this variable defines a template instantiation
command `jde-gen-class', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-class
	    (tempo-define-template "java-class-buffer-template"
		       (jde-gen-read-template val)
                       nil
                       "Insert a generic Java class buffer skeleton."))
	  (set-default sym val)))

;;;###autoload
(defun jde-gen-class-buffer (file)
  "Create a new Java buffer containing a class of the same name.
This command inserts the class template generated by `jde-gen-class'.
It then moves the point to the location to the constructor."
  (interactive "F")
  (find-file file)
  (jde-gen-class)
  (beginning-of-buffer)
  (search-forward "{")
  (backward-char 1)
  (c-indent-exp)
  (search-forward "        "))



(defcustom jde-gen-console-buffer-template
  '(
    "\"/**\" 'n"
    "\" * \"" 
    "(file-name-nondirectory buffer-file-name) 'n"
    "\" *\" 'n"
    "\" *\" 'n"
    "\" * Created: \" (current-time-string) 'n"
    "\" *\" 'n"
    "\" * @author \" (user-full-name) 'n"
    "\" * @version\" 'n"
    "\" */\" 'n>"
    "'n>"
    "\"public class \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\" {\" 'n> 'n>"
    "\"public \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\"() {\" 'n>"
    "'n>"
    "\"}\" 'n>"
    "'n>"
    "\"public static void main(String[] args) {\" 'n>"
    "\"        \" 'n>"
    "\"}\" 'n> 'n>"
    "\"} // \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "'n>")
  "*Template for new Java console app main class buffer.
Setting this variable defines a template instantiation
command `jde-gen-console', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-console
	    (tempo-define-template "java-console-buffer-template"
		       (jde-gen-read-template val)
                       nil
                       "Insert skeleton for a new Java console buffer"))
	  (set-default sym val)))

;;;###autoload
(defun jde-gen-console-buffer (file)
  "Create a new Java buffer containing a console class of the same name.
This command inserts the class template generated by `jde-gen-class'.
It then moves the point to the location to the constructor."
  (interactive "F")
  (find-file file)
  (jde-gen-console)
  (beginning-of-buffer)
  (search-forward "{")
  (backward-char 1)
  (c-indent-exp)
  (search-forward "        "))


(defcustom jde-gen-jfc-app-buffer-template
  '(
    "\"import java.awt.*;\" 'n"
    "\"import java.awt.event.*;\" 'n"
    "\"import com.sun.java.swing.*;\" 'n 'n"
    "\"/**\" 'n"
    "\" * \"" 
    "(file-name-nondirectory buffer-file-name) 'n"
    "\" *\" 'n"
    "\" *\" 'n"
    "\" * Created: \" (current-time-string) 'n"
    "\" *\" 'n"
    "\" * @author \" (user-full-name) 'n"
    "\" * @version\" 'n"
    "\" */\" 'n>"
    "'n>"
    "\"public class \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\" extends JFrame {\" 'n> 'n>"

    ;; Constructor
    "\"public \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\"() {\" 'n>"
    "\"super(\\\"\" (P \"Enter app title: \") \"\\\");\" 'n>"
    "\"setSize(600, 400);\" 'n>"
    "\"addWindowListener(new WindowAdapter() {\" 'n>"
    "\"public void windowClosing(WindowEvent e) {System.exit(0);}\" 'n>"
    "\"public void windowOpened(WindowEvent e) {}});\" 'n>"
    "\"}\" 'n>"
    "'n>"

    ;; Main method
    "\"public static void main(String[] args) {\" 'n>"
    "'n>"
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\" f = new \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\"();\" 'n>"
    "\"f.show();\" 'n>"
    "\"}\" 'n> 'n>"
    "\"} // \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "'n>")
  "*Template for JFC (Swing) application buffer.
Setting this variable defines a template instantiation
command `jde-gen-jfc-app', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-jfc-app
	    (tempo-define-template "java-jfc-app-buffer-template"
		       (jde-gen-read-template val)
                       nil
                       "Insert skeleton for a JFC app buffer"))
	  (set-default sym val)))

;;;###autoload
(defun jde-gen-jfc-app-buffer (file)
  "Create a new Java buffer containing a JFC application class.
This command inserts the class template generated by `jde-gen-jfc-app'.
It then moves the point to the location to the constructor."
  (interactive "F")
  (find-file file)
  (jde-gen-jfc-app)
  (beginning-of-buffer)
  (search-forward "{")
  (backward-char 1)
  (c-indent-exp))


(defcustom jde-gen-buffer-templates 
  (list (cons "Class" 'jde-gen-class)
	(cons "Console" 'jde-gen-console)
	(cons "Swing App" 'jde-gen-jfc-app))
  "*Specifies available autocode templates for creating buffers.
The value of this variable is an association list. The car of
each element specifies the template's title. The cdr specifies 
a command that inserts the template into a buffer. See the function
`tempo-define-template' for any easy way to create a template
insertion command."
  :group 'jde-gen
  :type '(repeat
	  (cons :tag "Template"
	   (string :tag "Title")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  (let ((n (length val))
		(i 0))
	    (setq jde-gen-buffer-template-names (list))
	    (while (< i n)
	      (setq jde-gen-buffer-template-names
		    (append
		     jde-gen-buffer-template-names
		     (list (cons (car (nth i val)) (1+ i)))))
	      (setq i (1+ i))))
	  (set-default sym val)))

;;;###autoload
(defun jde-gen-buffer (template file)
  "Create a new Java buffer containing a code template.
This command inserts the specified template at the beginning
of the buffer."
  (interactive 
   (list (completing-read "Template: " jde-gen-buffer-template-names)
	 (read-file-name "File: ")))
  (find-file file)
  (funcall (cdr (assoc template jde-gen-buffer-templates)))
  (beginning-of-buffer)
  (search-forward "{")
  (backward-char 1)
  (c-indent-exp))

(defun jde-gen-init-cap (name)
  (concat (upcase (substring name 0 1)) (substring name 1)))

(defcustom jde-gen-get-set-var-template
  '(
    "'n>"
    "(P \"Variable type: \" type) \" \""
    "(P \"Variable name: \" name) \";\" 'n> 'n>"
    "\"/**\" 'n>"
    "\"* Get the value of \" (s name) \".\" 'n>"
    "\"* @return Value of \" (s name) \".\" 'n>"
    "\"*/\" 'n>"
    "\"public \" (s type) \" get\" (jde-gen-init-cap (jde-gen-lookup-named 'name))"
    "\"() {return \" (s name) \";}\" 'n> 'n>"
    "\"/**\" 'n>"
    "\"* Set the value of \" (s name) \".\" 'n>"
    "\"* @param v  Value to assign to \" (s name) \".\" 'n>"
    "\"*/\" 'n>"
    "\"public void set\" (jde-gen-init-cap (jde-gen-lookup-named 'name))"
    "\"(\" (s type) \"  v) {this.\" (s name) \" = v;}\" 'n>" 
    )
  "*Template for creating a get/set method pair.
Setting this variable defines a template instantiation
command `jde-gen-get-set', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-get-set
	    (tempo-define-template
	     "java-get-set-pair"
	     (jde-gen-read-template val)
	     nil
	     "Insert variable get-set method pair."))
	  (set-default sym val)))

(defcustom jde-gen-inner-class-template
  '(
    "'& \"class \" (P \"Class name: \" class)"
    "(P \"Superclass: \" super t)"
    "(let ((parent (jde-gen-lookup-named 'super)))"
        "(if (not (string= parent \"\"))"
           "(concat \" extends \" parent))) \" {\" 'n>"
    "\"public \" (s class) \"() {\" 'n> \"}\" 'n> \"}\" 'n>"
    )
"*Template that creates an empty private class at point."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-inner-class
	    (tempo-define-template
	     "java-inner-class"
	     (jde-gen-read-template val)
	     nil
	     "Insert inner class."))
	  (set-default sym val)))

(defcustom jde-gen-action-listener-template
  '(
    "'& (P \"Component name: \")"
    "\".addActionListener(new ActionListener() {\" 'n>" 
    "\"public void actionPerformed(ActionEvent e) {\" 'n>"
    "\"}});\" 'n>"
    )
  "*Template for generating an action listener.
Setting this variable defines a template instantiation
command, `jde-gen-action-listener', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-action-listener
	    (tempo-define-template
	     "java-action-listener"
	     (jde-gen-read-template val)
	     nil
	     "Insert skeleton action listener."))
	  (set-default sym val)))

(defcustom jde-gen-window-listener-template
  '(
    "'& (P \"Window name: \")"
    "\".addWindowListener(new WindowAdapter() {\" 'n>"
    "\"public void windowActivated(WindowEvent e) {}\" 'n>"
    "\"public void windowClosed(WindowEvent e) {}\" 'n>"
    "\"public void windowClosing(WindowEvent e) {System.exit(0);}\" 'n>"
    "\"public void windowDeactivated(WindowEvent e) {}\" 'n>"
    "\"public void windowDeiconified(WindowEvent e) {}\" 'n>"
    "\"public void windowIconified(WindowEvent e) {}\" 'n>"
    "\"public void windowOpened(WindowEvent e) {}});\" 'n>"
    )
  "*Template for generating a window listener.
Setting this variable defines a template instantiation
command, `jde-gen-window-listener', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-window-listener
	    (tempo-define-template
	     "java-window-listener"
	     (jde-gen-read-template val)
	     nil
	     "Insert skeleton window listener."))
	  (set-default sym val)))



(defcustom jde-gen-mouse-listener-template
  '(
    "'& (P \"Component name: \")"
    "\".addMouseListener(new MouseAdapter() {\" 'n>"
    "\"public void mouseClicked(MouseEvent e) {}\" 'n>"
    "\"public void mouseEntered(MouseEvent e) {}\" 'n>"
    "\"public void mouseExited(MouseEvent e) {}\" 'n>"
    "\"public void mousePressed(MouseEvent e) {}\" 'n>"
    "\"public void mouseReleased(MouseEvent e) {}});\" 'n>"
    )
  "*Template for generating a mouse listener.
Setting this variable defines a template instantiation
command, `jde-gen-mouse-listener', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-mouse-listener
	    (tempo-define-template
	     "java-mouse-listener"
	     (jde-gen-read-template val)
	     nil
	     "Insert skeleton mouse listener."))
	  (set-default sym val)))

(defcustom jde-gen-mouse-motion-listener-template
  '(
    "'& (P \"Component name: \")"
    "\".addMouseMotionListener(new MouseMotionAdapter() {\" 'n>"
    "\"public void mouseDragged(MouseEvent e) {}\" 'n>"
    "\"public void mouseMoved(MouseEvent e) {}});\" 'n>"
    )
  "*Template for generating a mouse listener.
Setting this variable defines a template instantiation
command, `jde-gen-mouse-motion-listener', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-mouse-motion-listener
	    (tempo-define-template
	     "java-mouse-motion-listener"
	     (jde-gen-read-template val)
	     nil
	     "Insert skeleton mouse motion listener."))
	  (set-default sym val)))


(defcustom jde-gen-to-string-method-template
  '(
    "'&"
    "\"public String toString() {\" 'n>" 
    "\"return super.toString();\" 'n>"
    "\"}\" 'n>"
    )
  "*Template for generating a toString method.
Setting this variable defines a template instantiation
command, `jde-gen-to-string-method', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-gen-to-string-method
	    (tempo-define-template
	     "toString method"
	     (jde-gen-read-template val)
	     nil
	     "Insert skeleton toString method."))
	  (set-default sym val)))

(defcustom jde-gen-code-templates 
  (list (cons "Get Set Pair" 'jde-gen-get-set)
	(cons "toString method" 'jde-gen-to-string-method)
	(cons "Action Listener" 'jde-gen-action-listener)
	(cons "Window Listener" 'jde-gen-window-listener)
	(cons "Mouse Listener" 'jde-gen-mouse-listener)
	(cons "Mouse Motion Listener" 'jde-gen-mouse-motion-listener)
	(cons "Inner Class" 'jde-gen-inner-class))
  "*Specifies available autocode templates.
The value of this variable is an association list. The car of
each element specifies a template name. The cdr specifies 
a command that inserts the template into a buffer. See the function
`tempo-define-template' for any easy way to create a template
insertion command."
  :group 'jde-gen
  :type '(repeat
	  (cons :tag "Template"
	   (string :tag "Name")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  (let ((n (length val))
		(i 0))
	    (setq jde-gen-template-names (list))
	    (while (< i n)
	      (setq jde-gen-template-names
		    (append
		     jde-gen-template-names
		     (list (cons (car (nth i val)) (1+ i)))))
	      (setq i (1+ i))))
	  (set-default sym val)))

(defun jde-gen-code (name)
  "Insert the code template specified by NAME at point.
The template must be one of those specified by the
variable `jde-gen-code-templates'."
  (interactive
   (list
    (completing-read "Template name: " jde-gen-template-names)))
  (funcall (cdr (assoc name jde-gen-code-templates))))


(provide 'jde-gen)

;; $Log: jde-gen.el $
;; Revision 1.12  1998/07/01 03:54:40  paulk
;; Added source file boilerplate support.
;;
;; Revision 1.11  1998/06/27 03:04:46  paulk
;; Fixed capitalization on get-set method pair. Thanks to Jere_McDevitt@HomeDepot.COM
;;
;; Revision 1.10  1998/06/17 03:49:21  paulk
;; Fixed bug that caused jfc-app to be generated instead of console app.
;; Added a mouse motion listener template.
;; Added a toString method template.
;;
;; Revision 1.9  1998/05/27 05:55:20  paulk
;; Added autoload comments.
;;
;; Revision 1.8  1998/05/27 05:51:20  paulk
;; *** empty log message ***
;;
;; Revision 1.7  1998/05/17 06:20:37  paulk
;; Added templates for a Swing application and an inner class.
;;
;; Fixed a bug in jde-gen-buffer
;;
;; Revision 1.6  1998/04/18 14:08:55  kinnucan
;; Fixes some bugs in the generated code.
;;
;; Revision 1.5  1998/04/10 02:55:00  kinnucan
;; * Updated some of the doc strings.
;;
;; Revision 1.4  1998/04/09 04:51:09  kinnucan
;; * Added the capability to define your own custom autocode templates.
;;   The facility consists of the following items:
;;
;;   - jde-gen-code-templates
;;
;;     Defines a list of templates for code inserted at point. The
;;     list by default contains the templates defined by the JDE.
;;     You can define your own templates and add them to the list,
;;     using the Emacs customization feature. See tempo.el for
;;     information on creating templates.
;;
;;   - jde-gen-buffer-templates
;;
;;     Defines a list of templates for code to be inserted in a
;;     newly created Java buffer.
;;
;;   - jde-gen-code (JDE->Generate->Custom)
;;
;;     This command inserts a specified code template at point.
;;
;;   - jde-gen-buffer (Files->JDE New->Custom)
;;
;;     This command creates the specified buffer and inserts
;;     a specified template at the beginning of the buffer.
;;
;; Revision 1.3  1998/04/08 04:38:16  kinnucan
;; * Provided each template variable with a set function that regenerates
;;   the corresponding template command whenever the template is changed.
;;
;; Revision 1.2  1998/04/06 03:47:20  kinnucan
;; * Added jde-gen-class-buffer and jde-gen-console-buffer functions.
;;
;; Revision 1.1  1998/04/01 05:33:43  kinnucan
;; Initial revision
;;

;; End of jde-gen.el