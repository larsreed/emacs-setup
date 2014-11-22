;;; xae.el -- XML Authoring Environment for Emacs.
;; $Revision: 1.5 $ $Date: 2001/02/17 06:27:46 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 2000, 2001 Paul Kinnucan.

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
;; XML Authoring Environment (XAE) for Emacs. See the
;; XAE User's Guide for more information.

;; The latest version of the XAE is available at
;; <URL:http://xae.sunsite.dk>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@mediaone.net

;;; Code:

;;;###autoload
(defconst xae-version "1.0beta6"
  "XAE version number.")


(if (not (locate-library "psgml"))
    (add-to-list 
     'load-path 
     (expand-file-name  
      "psgml"
      (expand-file-name "../.." (locate-library "xae")))))

(if (not (locate-library "eieio"))
    (add-to-list 
     'load-path 
     (expand-file-name 
      "eieio"
      (expand-file-name "../.." (locate-library "xae")))))

;; Make xae-mode the default mode for XML document buffers.
;; Prepend the xae-mode entry so that it shadows the xml-mode
;; entry already in the list.
;;;###autoload
(setq auto-mode-alist
  (append
   '(("\\.xml\\'" . xae-mode))
	auto-mode-alist))


(require 'comint)
(require 'psgml)
(require 'tempo)
(require 'eieio)

(defconst xae-xemacsp (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running in the XEmacs environment.")

(defgroup xae nil
  "Technical Publishing Environment"
  :group 'tools
  :prefix "xae-")

;;(makunbound 'xae-standard-stylesheets)
(defcustom xae-standard-stylesheets
  (list (cons "docbook HTML"  
	      (list "doctypes/docbook/styles/docbook/html/docbook.xsl" "html"))
	(cons "docbook XHTML" 
	      (list "doctypes/docbook/styles/docbook/xhtml/docbook.xsl" "html"))
	(cons "docbook FO"    
	      (list "doctypes/docbook/styles/docbook/fo/docbook.xsl" "fo")))
  "*Specifies stylesheets that XAE knows about.
The value of this variable is an association list. The car of
each element specifies a name for the stylesheet. The cdr specifies
the path of the stylesheet relative to the XAE directory."
  :group 'xae
  :type '(repeat
	  (cons :tag "Stylesheet"
	   (string :tag "Name")
	   (list :tag "Properties"
	    (string :tag "Path")
	    (string :tag "Output Type"))))
  :set '(lambda (sym val)
	  (let ((n (length val))
		(i 0))
	    (setq xae-standard-stylesheet-names (list))
	    (while (< i n)
	      (setq xae-standard-stylesheet-names
		    (append
		     xae-standard-stylesheet-names
		     (list (cons (car (nth i val)) (1+ i)))))
	      (setq i (1+ i))))
	  (set-default sym val)))


(defvar xae-menu-def
  (list "XAE"
   (list "View Document"
    ["In XML Browser"       xae-view-doc-xml t]
    ["In HTML Browser"      xae-view-doc-html t]
   )

    (list "Apply Stylesheet"
	["Associated"  xae-apply-associated-stylesheet t]

	["Standard"    xae-apply-standard-stylesheet t]

	["User"        xae-apply-stylesheet t]
	
    )
    "-"
    ["Preferences"         xae-show-preferences nil]
    "-"
    (list "Help"
	  ["XAE"          xae-help-xae t]
	  ["Docbook"      xae-help-docbook t]
	  "-"
	  (concat "XAE " xae-version)
    )	  
   )
"Defines the XAE's menu.")


(defvar xae-keymap (make-sparse-keymap)
  "XAE keymap.")

(easy-menu-define  xae-menu xae-keymap "XAE menu" xae-menu-def)

(defvar xae-minor-mode-p nil
  "If non-nil, show jdebug menu.")
(make-variable-buffer-local 'xae-minor-mode-p)


(defun xae-install-menu ()
  "Installs the XAE menu in the current XML buffer."
  (if (and 
       (or 
	(not xae-xemacsp) 
	(featurep 'infodock)))
      (progn
	(setq xae-minor-mode-p t)
	(let ((a (assoc 'xae-minor-mode-p minor-mode-map-alist)))
	  (if a
	      (setcdr a xae-keymap)
	    (add-to-list 'minor-mode-map-alist
			 (cons 'xae-minor-mode-p
			       xae-keymap)))))
    (if (and 
	 (not (featurep 'infodock))
	 (not (memq 'infodock c-emacs-features))
	 (boundp 'current-menubar)
	 current-menubar
	 (not (car (find-menu-item current-menubar '("XAE")))))
	(if (fboundp 'add-submenu)
	    (add-submenu nil xae-menu)
	  (add-menu nil "XAE" (cdr xae-menu))))))

(defcustom xae-key-bindings
  (list (cons "[?\C-c ?\C-x ?\C-v]" 'xae-view-doc-html)
	(cons "[?\C-c ?\C-x ?\C-x]" 'xae-view-doc-xml))
  "*Specifies key bindings for XAE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'xae
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  ;; Unmap existing key bindings
	  (if (and
	       (boundp 'xae-key-bindings)
	       xae-key-bindings)
	      (mapc 
	       (lambda (binding)
		 (let ((key (car binding))
		       (fcn (cdr binding)))
		   (if (string-match "\\[.+]"key)
		       (setq key (car (read-from-string key))))
		   (define-key xae-keymap key nil)))
	       xae-key-bindings))
	  ;; Map new key bindings.
	  (mapc 
	   (lambda (binding)
	     (let ((key (car binding))
		   (fcn (cdr binding)))
	       (if (string-match "\\[.+]"key)
		   (setq key (car (read-from-string key))))
	       (define-key xae-keymap key fcn)))
	   val)
	  (set-default sym val)))

(defun xae-find-xae-data-directory ()
  "Return the path of the XAE data directory.
Returns the path of the directory containing the
XAE stylesheet, dtd, and documentation directories;  nil if the 
directory cannot be found. If XEmacs, returns the location of
the data directory in the XEmacs distribution hierarchy. On all other Emacs versions, 
the JDE exaects to find the documentation and Java class directories
in the same directory that contains the XAE lisp directory."
  (let (dir (xae-xemacsp nil))
    (if xae-xemacsp
	(progn
	  (setq dir (locate-data-directory "xae"))
	  (when (not dir)
	      (setq dir (file-name-directory (locate-library "xae")))
	      (setq dir (substring dir 0 (- (length dir) 5)))))
      (setq dir (file-name-directory (locate-library "xae"))))
    (if dir
	(nsubstitute ?/ ?\\ dir))
    (if (not xae-xemacsp)
	(setq dir (substring dir 0 (- (length dir) 5))))
    dir))

(defun xae-mode-internal () 
  (xae-install-menu))

;; This is actually a no-op to get jde auto-loaded.
;;;###autoload
(defun xae-mode ()
  "Major mode for developing Java applications and applets."
  nil)

(define-derived-mode 
  xae-mode xml-mode "XAE"
  "Major mode for creating XML applications.
  \\{xae-mode-map}"

  (xae-mode-internal)
)

(defun xae-path-to-uri (path)
  "Converts a file path to a uri"
  (if (eq system-type 'windows-nt)
      (concat "file:///" path)
    (concat "file://" path)))

;;;###autoload
(defun xae-apply-stylesheet (stylesheet-path output-doc-path)
  "Applies the stylesheet specified by STYLESHEET-PATH to the document 
in the current buffer to produce the output doc specified by OUTPUT-DOC-PATH."
  (interactive 
    (list (read-file-name "Stylesheet: ")
	  (read-from-minibuffer "Output File Name: " 
				(concat 
		   (file-name-sans-extension (file-name-nondirectory buffer-file-name))
		   ".html"))))
  (let* ((input-doc-path (buffer-file-name (current-buffer)))
	 (input-doc-uri (xae-path-to-uri input-doc-path))
	 (stylesheet-uri (xae-path-to-uri stylesheet-path)))
    (xae-cmd-apply-stylesheet 
     xae-the-xml-tool-server
     input-doc-uri stylesheet-uri output-doc-path)))


;;;###autoload
(defun xae-apply-associated-stylesheet (out-type)
  "Applies the stylesheet specified by the document in the current buffer
to the document. OUT-TYPE specifies the type (extension), e.g., html, of the output
file produced by the stylesheet."
  (interactive 
    (list (read-from-minibuffer "Output File Extension: " "html")))
  (let* ((input-doc-path (buffer-file-name (current-buffer)))
	 (input-doc-uri (xae-path-to-uri input-doc-path))
	 (output-doc-path (format "%s.%s"
			  (file-name-sans-extension input-doc-path)
			  out-type)))
    (xae-cmd-apply-associated-stylesheet
     xae-the-xml-tool-server input-doc-uri output-doc-path)))

(defvar xae-standard-stylesheet-name-history nil)

;;;###autoload
(defun xae-apply-standard-stylesheet ()
  "Apply a standard style sheet to the document in the
current buffer. A standard stylesheet is one of the stylesheets
defined by `xae-standard-stylesheets'. The resulting output file
resides in the same directory as the input document."
  (interactive) 
  (let ((name (completing-read "Stylesheet: " xae-standard-stylesheet-names 
			       nil nil nil 'xae-standard-stylesheet-name-history)))
    (if (not (string= name ""))
      (let* ((input-doc-path (buffer-file-name (current-buffer)))
	     (input-doc-uri (xae-path-to-uri input-doc-path))
	     (xae-dir (xae-find-xae-data-directory))
	     (props  (cdr (assoc name xae-standard-stylesheets)))
	     (stylesheet-path (expand-file-name (car props) xae-dir))
	     (stylesheet-uri (xae-path-to-uri stylesheet-path))
	     (out-type (nth 1 props))
	     (output-doc-path (format "%s.%s"
			  (file-name-sans-extension input-doc-path)
			  out-type)))
	(xae-cmd-apply-stylesheet 
	 xae-the-xml-tool-server input-doc-uri stylesheet-uri output-doc-path)))))

(defun xae-display-callback (server)
  (browse-url (oref server utility-slot) browse-url-new-window-p))

;;;###autoload
(defun xae-view-doc-html ()
  "Display XML document in the current buffer in the default browser. This
command assumes that the default browser accepts only HTML files. Use
`xae-view-doc-xml' if the default browser accepts XML (e.g., Internet
Explorer 5.5.) Use the browse-url package to specify the default browser.
This function assumes that the XML document specifies a stylesheet via
a stylesheet processing instruction and that the stylesheet converts the
documement to HTML. It applies the associated stylesheet to the document
and displays the result in the default browser."
  (interactive)
  (let* ((input-doc-path (buffer-file-name (current-buffer)))
	 (input-doc-uri (xae-path-to-uri input-doc-path))
	 (xae-dir (xae-find-xae-data-directory))
	 (output-doc-path 
	  (concat (file-name-sans-extension buffer-file-name) ".html"))
	 (output-doc-uri (xae-path-to-uri output-doc-path)))

    (oset xae-the-xml-tool-server command-completion-handler
	  (lambda (server termination-type)
      	    (if (eq termination-type :normal)
		(progn
		  (browse-url (oref server utility-slot) browse-url-new-window-p)
		  (oset xae-the-xml-tool-server utility-slot nil)
		  (slot-makeunbound xae-the-xml-tool-server 'command-completion-handler)))))
	  
    (oset xae-the-xml-tool-server utility-slot output-doc-uri)
    (xae-cmd-apply-associated-stylesheet
     xae-the-xml-tool-server input-doc-uri output-doc-path)))


;;;###autoload
(defun xae-view-doc-xml ()
  "Display XML document in the current buffer in the default browser. This
command assumes that the default browser can display XML documents. Use
`xae-vew-doc-xml' if the default browser accepts only HTML documents."
  (interactive)
  (let ((doc-path (buffer-file-name (current-buffer))))
    (browse-url doc-path browse-url-new-window-p)))

(defun xae-help-docbook ()
  (interactive)
  "Displays the Docbook user's guide."
  (let ((docbook-ug 
	(expand-file-name 
	 "doctypes/docbook/doc/html/docbook.html"
	 (xae-find-xae-data-directory))))
    (browse-url (concat "file://" docbook-ug browse-url-new-window-p))))

(defun xae-help-xae ()
  (interactive)
  "Displays the XAE user's guide."
  (let ((ug 
	(expand-file-name 
	 "doc/html/xae-ug/ug.html"
	 (xae-find-xae-data-directory))))
    (browse-url (concat "file://" ug browse-url-new-window-p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Document templates                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xae-read-template (strings)
  "Converts a template represented as a list
of strings to a list of Lisp objects as required by
tempo."
  (let ((template-string 
	 (concat 
	  "'("
	  (mapconcat
	   (lambda (string) string)
	   strings " ")
	  ")")))
    (eval (car (read-from-string template-string)))))

;;(makunbound 'xae-new-book-template)
(defcustom xae-new-book-template
  (list
   "'&\"<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\"?>\" 'n'n"
   "\"<!DOCTYPE book PUBLIC \\\"-//Arbortext//DTD DocBk XML V2.0//EN\\\" \"" 
   "\"\\\"file:///\" (xae-find-xae-data-directory) \"doctypes/docbook/docbookx.dtd\\\" []>\" 'n'n"
   "\"<?xml-stylesheet href=\\\"file:///\" (xae-find-xae-data-directory)"
   "\"doctypes/docbook/styles/docbook/html/docbook.xsl\\\" type=\\\"text/xsl\\\"?>\" 'n'n"
   "\"<book>\" 'n"
   "\"  <title>\" (P \"Book title: \") \"</title>\" 'n"
   "\"  <chapter>\" 'n"
   "\"    <title></title>\" 'n"
   "\"  </chapter>\" 'n"
   "\"</book>\" 'n'n"
   "\"<!-- \" 'n"
   "\"Local Variables:\" 'n" 
   "\"mode: xae\" 'n"
   "\"sgml-indent-step: 2\" 'n"
   "\"sgml-indent-data: t\" 'n"
   "\"sgml-set-face: t\" 'n"
   "\"sgml-insert-missing-element-comment: nil\" 'n"
   "\"End:\" 'n"
   "\"--> \"" 
   )
  "Template for new Docbook instance.
Setting this variable defines a template instantiation
command `xae-gen-book', as a side-effect."
  :group 'xae
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'xae-gen-book
	    (tempo-define-template "xae-book-template"
		       (xae-read-template val)
                       nil
                       "Insert skeleton for a new Docbook book buffer"))
	  (set-default sym val)))

;;;###autoload
(defun xae-new-book ()
  "Creates a buffer containing a new instance of a Docbook.
This command inserts `xae-new-book-template' in the buffer.
You can customize this template to suit your needs. See the 
tempo package for information on specifying tempo templates."
  (interactive)
  (switch-to-buffer (create-file-buffer "new.xml"))
  (xae-gen-book)
  (xae-mode))


(makunbound 'xae-new-article-template)
(defcustom xae-new-article-template
  (list
   "'&\"<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\"?>\" 'n'n"
   "\"<!DOCTYPE article PUBLIC \\\"-//Arbortext//DTD DocBk XML V2.0//EN\\\" \"" 
   "\"\\\"file:///\" (xae-find-xae-data-directory) \"doctypes/docbook/docbookx.dtd\\\" []>\" 'n'n"
   "\"<?xml-stylesheet href=\\\"file:///\" (xae-find-xae-data-directory)"
   "\"doctypes/docbook/styles/docbook/html/docbook.xsl\\\" type=\\\"text/xsl\\\"?>\" 'n'n"
   "\"<article>\" 'n"
   "\"  <title>\" (P \"Article title: \") \"</title>\" 'n"
   "\"</article>\" 'n'n"
   "\"<!-- \" 'n"
   "\"Local Variables:\" 'n" 
   "\"mode: xae\" 'n"
   "\"sgml-indent-step: 2\" 'n"
   "\"sgml-indent-data: t\" 'n"
   "\"sgml-set-face: t\" 'n"
   "\"sgml-insert-missing-element-comment: nil\" 'n"
   "\"End:\" 'n"
   "\"--> \"" 
   )
  "Template for new Docbook instance.
Setting this variable defines a template instantiation
command `xae-gen-article', as a side-effect."
  :group 'xae
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'xae-gen-article
	    (tempo-define-template "xae-article-template"
		       (xae-read-template val)
                       nil
                       "Insert skeleton for a new Docbook article"))
	  (set-default sym val)))


;;;###autoload
(defun xae-new-article ()
  "Creates a buffer containing a new instance of a Docbook article.
This command inserts `xae-new-article-template' in the buffer.
You can customize this template to suit your needs. See the 
tempo package for information on specifying tempo templates."
  (interactive)
  (switch-to-buffer (create-file-buffer "new.xml"))
  (xae-gen-article)
  (xae-mode))



(defvar xae-new-buffer-menu
  (list
   "XAE New"
   ["Book"         xae-new-book t]
   ["Article"      xae-new-article t]
   )
  "Menu for creating new Docbook instances.")

;; Add XAE New menu to Emacs Files menu.
(if (not xae-xemacsp)
    (let* ((mb (assq 'menu-bar global-map))
	   (files (assq 'files mb))
	   (menu (if (fboundp 'easy-menu-create-menu)
		     (easy-menu-create-menu 
		      (car xae-new-buffer-menu) (cdr xae-new-buffer-menu))
		   (easy-menu-create-keymaps 
		    (car xae-new-buffer-menu) (cdr xae-new-buffer-menu))))     
	   (menu-name (car xae-new-buffer-menu)))
      (define-key-after (cdr (cdr files)) [xae-new]
	(cons menu-name menu)
	'open-file))
  (unless (featurep 'infodock)
    (add-submenu '("File") xae-new-buffer-menu "Insert File...")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; XML Tool Server                                                            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xae-run-xml-tool-server()
"*Starts the XAE's Java XML tool server."
  (interactive)
  (xae-start xae-the-xml-tool-server t))

(defclass xae-xml-tool-server ()
  ((buffer-name   
    :initarg :buffer-name
    :type string
    :initform "*XML Tool Server*"
    :documentation
    "Name of XML tools server buffer.")
   (buffer        
    :initarg :buffer
    :documentation
    "Server buffer")
   (process       
    :initarg :process
    :accessor xae-get-process
    :documentation
    "Server process")
   (comint-filter
    :initarg :comint-filter
    :type function
    :documentation
    "Standard comint output filter.")
   (output-filter 
    :initarg :output-filter
    :type function
    :documentation
    "Filters server output.")
   (command-completion-handler
    :initarg :commmand-completion-handler
    :type function
    :documentation
    "Invoked when a server command finishes.")
   (utility-slot 
    :initarg :utility-slot
    :documentation
    "Slot to be used by server clients for temporary storage.")
   )
  "Defines the XML tool server.")


(defmethod initialize-instance ((this xae-xml-tool-server) &rest fields)
  "Constructor for objects of `xae-xml-tool-server' class."
  (call-next-method)
  (let ((filter-symbol (make-symbol "xae-xml-tool-server-output-filter")))
    (put filter-symbol :xml-tool-server this)
    (fset filter-symbol
	  (lambda (process output)
	    (let* ((this-filter (process-filter process))
		   (server (get this-filter :xml-tool-server))
		   (termination-type
		    (if (string-match "Done" output)
			:normal
		      (if (string-match "aborted" output)
			  :error))))
	   ;; (message "server output-filter")
	      (if (and 
		   termination-type
		   (slot-boundp server 'command-completion-handler)
		   (oref server command-completion-handler))
		(funcall (oref server command-completion-handler) server termination-type))
	    (funcall (oref server comint-filter) process output))))  
  (oset this output-filter filter-symbol)))

(defmethod xae-start ((this xae-xml-tool-server) &optional display-buffer-p)
  "Start the XML tool server."
  (if (not (comint-check-proc (oref this :buffer-name)))
    (let* ((xae-java-directory
	    (concat
	     (xae-find-xae-data-directory)
	     "java/"))
	   (vm (if (eq system-type 'windows-nt)
		   "javaw"
		 "java"))
	   (user-classpath (getenv "CLASSPATH"))
	   (vm-args
	    (list
	     "-classpath"
	     (concat
	      (if user-classpath
		  (concat user-classpath path-separator))
	      (expand-file-name "classes" xae-java-directory)
	      path-separator
	      (expand-file-name "xsl-engines/saxon/saxon.jar" 
				(xae-find-xae-data-directory)))))
	   (windowed-process-io t)
	   win32-start-process-show-window)

      (setq vm-args (append vm-args (list "xae.XMLToolServer")))

      (oset this buffer (get-buffer-create (oref this buffer-name)))

      (save-excursion
	(set-buffer (oref this buffer))
	(erase-buffer)
	(comint-mode)
	(make-local-hook 'comint-output-filter-functions)
	(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m nil t)
	(setq comint-scroll-to-bottom-on-output t)
	(setq comint-prompt-regexp "^> *"))
      (message "%s" "Starting the XML Tool Server. Please wait...")
      (comint-exec (oref this buffer) (oref this buffer-name)
		       vm nil vm-args)

      (oset this process (get-buffer-process (oref this buffer)))     
      (process-kill-without-query (oref this process))
      (oset this comint-filter (process-filter (oref this process)))
      (set-process-filter (oref this process)
 			  (oref this output-filter))
      (accept-process-output (oref this process) 10 0)
      (if display-buffer-p
	  (pop-to-buffer (oref this buffer-name))))
    (when display-buffer-p
      (message "The XML Tool Server is already running.")
      (pop-to-buffer (oref this buffer-name)))))

(defvar xae-the-xml-tool-server 
  (xae-xml-tool-server "tool server")
  "One-and-only instance of the XML tool server.")

(defmethod xae-exec ((this xae-xml-tool-server) command)
  (xae-start this)
  (save-excursion
    (set-buffer (oref this buffer))
    (insert command)
    (comint-send-input))    
  (pop-to-buffer (oref this buffer)))


(defmethod xae-cmd-apply-stylesheet ((this xae-xml-tool-server) 
				     input-doc-uri stylesheet-uri output-doc-path)
  "Applies a stylesheet to an input document to produce an output document.
INPUT-DOC-URI is the URI of the input document. STYLESHEET-URI is the URI of the
style sheet. OUTPUT-DOC-PATH is the path of the output document."
  (let ((cmd
	 (format "apply_stylesheet %s %s %s"    
		 input-doc-uri stylesheet-uri output-doc-path)))
    (xae-exec this cmd)))


(defmethod xae-cmd-apply-associated-stylesheet ((this xae-xml-tool-server)
						input-doc-uri output-doc-path)
  "Applies the stylesheet specified by an xsl-stylesheet processing instruction
in the input document to the input document to produce an output document.
INPUT-DOC-URI is the URI of the input document. OUTPUT-DOC-PATH is the path of the output document."
  (let ((cmd
	 (format "apply_assoc_stylesheet %s %s"    
		 input-doc-uri output-doc-path)))
    (xae-exec this cmd)))


(provide 'xae)

;; Revision history:
;;
;; $Log: xae.el,v $
;; Revision 1.5  2001/02/17 06:27:46  paulk
;; Fixed eieio loading bug.
;;


;; xae.el ends here.