;;; jde-javadoc-gen.el -- Javadoc builder
;; $Revision: 1.9 $ 

;; Author: Sergey A Klibanov <sakliban@cs.wustl.edu>
;; Maintainer: Paul Kinnucan, Sergey A Klibanov
;; Keywords: java, tools

;; Copyright (C) 2000, 2001, 2002 Paul Kinnucan.

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;;;;
;;;; Customization
;;;;

(defgroup jde-javadoc nil
  "Javadoc template generator"
  :group 'jde
  :prefix "jde-javadoc-")

(defcustom jde-javadoc-command-path "javadoc"
  "*Path to the javadoc executable."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-display-doc t
  "*Display the documentation generated by the `jde-javadoc-make' command. ."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-detail-switch (list "-protected")
  "Specifies what access level switch to use.
  -public will show only public classes and members
  -protected will show protected and public classes and members
  -package will show package, protected, and public classes and members
  -private will show all classes and members"
  :group 'jde-javadoc
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Select the detail level switch you want:"
	   (const "-public")
	   (const "-protected")
	   (const "-package")
	   (const "-private"))))

(defcustom jde-javadoc-gen-packages nil
  "Specifies which packages or files javadoc should be run on."
  :group 'jde-javadoc
  :type '(repeat (string :tag "Path")))

(defcustom jde-javadoc-gen-destination-directory "JavaDoc"
  "Specifies the directory where javadoc will put the generated files."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-link-URL nil
  "Specifies what URL's to link the generated files to.
   For more information, look at the -link option of the javadoc tool."
  :group 'jde-javadoc
  :type '(repeat (string :tag "URL")))

(defcustom jde-javadoc-gen-link-online nil
  "Specifies whether or not to use jde-javadoc-gen-link-URL."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-link-offline nil
  "Specifies URLs to link to and the local path to the directory holding the package-list for each URL.
   The second argument can be a URL (http: or file:). If it is a relative file name, it is relative to the directory
   from which javadoc is run."
  :group 'jde-javadoc
  :type '(repeat 
	  (cons :tag "Remote URL and directory holding package-list for that URL"
		(string :tag "URL")
		(string :tag "Path"))))

(defcustom jde-javadoc-gen-group nil
  "Specifies groups of packages with a group heading and package pattern.
   The heading is usually a string like Extension Packages. The pattern is
   any package name or wildcard matching that name. You can specify several
   packages by separating the package names by a semicolon."
  :group 'jde-javadoc
  :type '(repeat
	  (cons :tag "Package group name and contents"
		(string :tag "Heading")
		(string :tag "Package Pattern"))))

(defcustom jde-javadoc-gen-doc-title ""
  "Specifies the title to be placed near the top of the overview summary file."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-window-title ""
  "Specifies what should be placed in the HTML <title> tag.
   Quotations inside the title should be escaped."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-overview ""
  "Specifies where to get an alternate overview-summary.html.
   The path is relative to the sourcepath."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-doclet ""
  "Specifies the class file that starts an alternate doclet
   to generate the html files. This path is relative to docletpath"
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-docletpath nil
  "Specifies the path in which the doclet should be searched for."
  :group 'jde-javadoc
  :type '(repeat (string :tag "Path")))

(defcustom jde-javadoc-gen-header ""
  "Specifies what html code should be placed at the top of each output file."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-footer ""
  "Specifies what html code should be placed at the bottom of each output file."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-bottom ""
  "Specifies what text or html code should be placed at the bottom
   below the navigation bar."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-helpfile ""
  "Specifies the help file to be used for the Help link in the Navigation bar."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-stylesheetfile ""
  "Specifies the path to an alternate HTML stylesheet file."
  :group 'jde-javadoc
  :type 'string)

(defcustom jde-javadoc-gen-split-index nil
  "Specifies whether or not the index should be split alphabetically
   one file per letter."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-use nil
  "Specifies whether or not to create \"Use\" pages"
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-author t
  "Specifies whether or not to use @author tags"
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-version t
  "Specifies whether or not to use @version tags"
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-serialwarn nil
  "Specifies whether or not to generate compile-time errors for missed @serial tags"
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-nodeprecated nil
  "Specifies whether or not to remove all references to deprecated code"
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-nodeprecatedlist nil
  "Specifies whether or not to remove references to deprecated code
   from the navigation bar, but not the rest of the documents."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-notree nil
  "Specifies whether or not to omit generating the class/interface hierarchy."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-noindex nil
  "Specifies whether or not to omit generating the index."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-nohelp nil
  "Specifies whether or not to omit the HELP link in the navigation bar of each page."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-nonavbar nil
  "Specifies whether or not to omit generating the navigation bar at the top of each page."
  :group 'jde-javadoc
  :type 'boolean)

(defcustom jde-javadoc-gen-verbose nil
  "Specifies whether or not javadoc should be verbose about what it is doing."
  :group 'jde-javadoc
  :type 'boolean)


(defcustom jde-javadoc-gen-args nil
  "Specifies any other arguments that you want to pass to javadoc."
  :group 'jde-javadoc
  :type '(repeat (string :tag "Argument")))


;;;###autoload
(defun jde-javadoc-make ()
  "Generates javadoc for the current project. This command runs the
JDE javadoc program to generate the documentation. The variable
`jde-javadoc-command-path' specifies the path of the javadoc excutable.
The variable `jde-global-classpath' specifies the javadoc 
-classpath argument. The variable `jde-sourcepath'
specifies the javadoc  -sourcepath argument. You can specify all
other javadoc options via JDE customization variables. To specify the
options, select Project->Options->Javadoc from the JDE menu. Use 
`jde-javadoc-gen-packages' to specify the packages, classes, or source
files for which you want to generate javadoc. If this variable is nil,
this command generates javadoc for the Java source file in the current
buffer. If `jde-javadoc-display-doc' is nonnil, this command displays
the generated documentation in a browser."
  (interactive)
  (save-some-buffers)

  
  (if (not (directory-files jde-javadoc-gen-destination-directory nil nil nil)) 
      (make-directory jde-javadoc-gen-destination-directory))

  (let ((w32-quote-process-args ?\")
	(win32-quote-process-args ?\") ;; XEmacs
	(arguments 
	 (format "%s -d %s %s"
		 jde-javadoc-command-path
		 jde-javadoc-gen-destination-directory
		 (car jde-javadoc-gen-detail-switch))))

    ;;Insert online links
    (if jde-javadoc-gen-link-online 
	(setq arguments 
	      (concat 
	       arguments
	       (mapconcat
		(lambda (link) (format " -link \"%s\"" link))
		jde-javadoc-gen-link-URL " "))))
    
    ;;Insert offline links
    (if jde-javadoc-gen-link-offline
	(setq arguments
	      (concat 
	       arguments
	       (mapconcat
		(lambda (link) 
		  (format " -linkoffline \"%s\" \"%s\" " 
			  (car  link) (cdr link)))
		jde-javadoc-gen-link-offline " "))))

    ;;Insert -group
    (if jde-javadoc-gen-group
	(setq arguments
	      (concat
	       arguments
	       (mapconcat
		(lambda (group) 
		  (format " -group \"%s\" \"%s\" " 
			  ( car group) (cdr group)))
		jde-javadoc-gen-group " "))))

    ;; Insert classpath
    (if jde-global-classpath
	(setq arguments
	      (concat
	       arguments " "
	       (jde-build-classpath-arg
		'jde-global-classpath jde-quote-classpath))))

    ;; Insert sourcepath
    (if jde-sourcepath
	(setq arguments
	      (concat
	       arguments " "
	       (jde-build-path-arg
		"-sourcepath "
		'jde-sourcepath
		jde-quote-classpath))))


    ;; Insert bootclasspath
    (if jde-compile-option-bootclasspath
	(setq arguments
	      (concat
	       arguments " "
	       (jde-build-path-arg
		"-bootclasspath "
		'jde-compile-option-bootclasspath
		jde-quote-classpath))))

    ;; Insert extdirs
    (if jde-compile-option-extdirs
	(setq arguments
	      (concat
	       arguments " "
	       (jde-build-path-arg
		"-extdirs "
		'jde-compile-option-extdirs
		jde-quote-classpath))))

    ;; Insert windowtitle
    (if (not (equal "" jde-javadoc-gen-window-title))
	(setq arguments
	      (concat
	       arguments " -windowtitle \""
	       jde-javadoc-gen-window-title "\" ")))

    ;; Insert doctitle
    (if (not (equal "" jde-javadoc-gen-doc-title))
	(setq arguments
	      (concat
	       arguments " -doctitle \""
	       jde-javadoc-gen-doc-title "\" ")))

    ;; Insert header
    (if (not (equal "" jde-javadoc-gen-header))
	(setq arguments
	      (concat
	       arguments " -header \""
	       jde-javadoc-gen-header "\" ")))

    ;; Insert footer
    (if (not (equal "" jde-javadoc-gen-footer))
	(setq arguments
	      (concat
	       arguments " -footer \""
	       jde-javadoc-gen-footer "\" ")))

    ;; Insert bottom
    (if (not (equal "" jde-javadoc-gen-bottom))
	(setq arguments
	      (concat
	       arguments " -bottom \""
	       jde-javadoc-gen-bottom "\" ")))

    ;; Insert helpfile
    (if (not (equal "" jde-javadoc-gen-helpfile))
	(setq arguments
	      (concat
	       arguments " -helpfile \""
	       (jde-normalize-path 'jde-javadoc-gen-helpfile) "\" ")))

    ;; Insert stylesheet
    (if (not (equal "" jde-javadoc-gen-stylesheetfile))
	(setq arguments
	      (concat
	       arguments " -stylesheetfile \""
	       (jde-normalize-path 'jde-javadoc-gen-stylesheetfile) "\" ")))

    ;; Insert -overview
    (if (not (equal "" jde-javadoc-gen-overview))
	(setq arguments
	      (concat
	       arguments " -overview \""
	       jde-javadoc-gen-overview "\" ")))

    ;; Insert -doclet
    (if (not (equal "" jde-javadoc-gen-doclet))
	(setq arguments
	      (concat
	       arguments " -doclet \""
	       jde-javadoc-gen-doclet "\" ")))

    ;; Insert -docletpath
    (if jde-javadoc-gen-docletpath
	(setq arguments
	      (concat 
	       arguments " "
	       (jde-build-path-arg 
		"-docletpath "
		'jde-javadoc-gen-docletpath
		jde-quote-classpath))))

    ;; Inser -use
    (if jde-javadoc-gen-use
	(setq arguments
	      (concat
	       arguments " -use ")))

    ;;Insert -author
    (if jde-javadoc-gen-author
	(setq arguments
	      (concat
	       arguments " -author ")))
    
    ;;Insert -version
    (if jde-javadoc-gen-version
	(setq arguments
	      (concat
	       arguments " -version ")))

    ;;Insert -splitindex
    (if jde-javadoc-gen-split-index
	(setq arguments
	      (concat
	       arguments " -splitindex ")))

    ;;Insert -nodeprecated
    (if jde-javadoc-gen-nodeprecated
	(setq arguments
	      (concat 
	       arguments " -nodeprecated ")))

    ;;Insert -nodeprecatedlist
    (if jde-javadoc-gen-nodeprecatedlist
	(setq arguments
	      (concat 
	       arguments " -nodeprecatedlist ")))

    ;;Insert -notree
    (if jde-javadoc-gen-notree
	(setq arguments
	      (concat
	       arguments " -notree ")))

    ;;Insert -noindex
    (if jde-javadoc-gen-noindex
	(setq arguments
	      (concat
	       arguments " -noindex ")))

    ;;Insert -nohelp
    (if jde-javadoc-gen-nohelp
	(setq arguments
	      (concat
	       arguments " -nohelp ")))

    ;;Insert -nonavbar
    (if jde-javadoc-gen-nonavbar
	(setq arguments
	      (concat
	       arguments " -nonavbar ")))

    ;;Insert -serialwarn
    (if jde-javadoc-gen-serialwarn
	(setq arguments
	      (concat
	       arguments " -serialwarn ")))

    ;;Insert -verbose
    (if jde-javadoc-gen-verbose
	(setq arguments
	      (concat
	       arguments " -verbose ")))

    ;;Insert other tags
    (if jde-javadoc-gen-args
	(setq arguments
	      (concat
	       arguments " " 
	       (mapconcat
		(lambda (arg) arg)
		jde-javadoc-gen-args " "))))

    ;;Insert packages/files
    (if jde-javadoc-gen-packages
	(setq arguments 
	      (concat 
	       arguments
	       (mapconcat
		(lambda (packagename) (format "%s" packagename))
		jde-javadoc-gen-packages " ")))
      (setq arguments
	    (concat arguments " \"" (buffer-file-name) "\"")))
    
    (setq compilation-finish-function
	  (lambda (buf msg)
	    (message msg)
	    (if (and
		 jde-javadoc-display-doc
		 (string-match "finished" msg))
		(browse-url-of-file 
		 (expand-file-name "index.html" jde-javadoc-gen-destination-directory)))))

    (compile-internal arguments "No errors now!")))

(provide 'jde-javadoc-gen)

;;
;; $Log: jde-javadoc-gen.el,v $
;; Revision 1.9  2002/06/12 07:04:25  paulk
;; XEmacs compatibility fix: set win32-quote-process-args wherever
;; the JDEE sets w32-quote-process-args. This allows use of spaces in
;; paths passed as arguments to processes (e.g., javac)  started by
;; the JDEE.
;;
;; Revision 1.8  2002/06/11 06:44:05  paulk
;; Provides support for paths containing spaces as javadoc arguments via the following change:
;; locally set the w32-quote-process-args variable to a quotation mark when launching
;; the javadoc process.
;;
;; Revision 1.7  2002/03/31 07:49:50  paulk
;; Renamed jde-db-source-directories. The new name is jde-sourcepath.
;;
;; Revision 1.6  2001/04/16 05:56:42  paulk
;; Normalize paths. Thanks to Nick Sieger.
;;
;; Revision 1.5  2001/04/11 03:19:43  paulk
;; Updated to resolve relative paths relative to the project file that defines them. Thanks to Nick Sieger.
;;
;; Revision 1.4  2001/03/22 18:51:33  paulk
;; Quote buffer path argument on command line to accommodate paths with spaces in them as permitted on Windows. Thanks to "Jeffrey Phillips" <jeffrey.phillips@staffeon.com> for reporting this problem and providing a fix.
;;
;; Revision 1.3  2001/03/01 04:23:28  paulk
;; Fixed incorrect generation of javadoc index path in jde-javadoc-make function.   "William G. Griswold" <wgg@cs.ucsd.edu> for this fix.
;;
;; Revision 1.2  2001/01/27 05:53:38  paulk
;; No longer inserts -sourcepath argument twice in the javadoc command line.
;;
;; Revision 1.1  2000/10/20 04:06:34  paulk
;; Initial version.
;;
;;

;;; jde-javadoc-gen.el ends here
