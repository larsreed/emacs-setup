;; JDE-XREF.EL --- Class cross-reference commands for the JDEE.
;; $Revision: 1.10 $ $Date: 2002/12/18 04:09:15 $
;;
;; Copyright (C) 2002 Andrew Hyatt
;;
;; Author: Andrew Hyatt <andy_jde@thehyatts.net>
;; Maintainers: Andrew Hyatt and Paul Kinnucan
;; Keywords: java, tools
;; 
;;
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
;; ) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; jde-xref|Andrew Hyatt|
;; |Java class cross-referencing commands for the JDEE
;; |$Date: 2002/12/18 04:09:15 $|$Revision: 1.10 $|~/packages/jde-xref.el

;;; Commentary:

;; This file stores all caller-related functionality, from the
;; functions to find all callers of a Java method, to those that will
;; do more sophisticated things like creating a call-tree
;;
;; Everything works off the database, which is split into a series of
;; files, one file per package.  Each package consists of three tables.
;; The first is a mapping of class-function-args specifiers to a list
;; of all the classes, method & linenumbers that call it, such as:
;;
;; (("org.foo.MyClass" "getName" nil "String") .
;;   (("org.foo.MyOtherClass" "doProcessing" nil nil 42)
;;    ("org.foo.YetAnotherClass" "doMoreProcessing" nil nil 32)))
;;
;; The specifier for a method (the first part of the above is a: fully
;; qualified classname , function name, return type (nil for "void")
;; and non-fully qualified arglist.  We don't want to fully qualify
;; the arglist, so that we don't have to fully qualify it when doing a
;; lookup.  The changes of collisions are minor, I think.
;;
;; The next table is a table of which class has which interfaces.
;; This is used so that we can consider a call to IFoo.doSomething as
;; a call to Foo.doSomething, is Foo implements IFoo.  This
;; functionality is not always wanted, so the functions that show
;; caller information can either use it or not.
;;
;; The other table is a table of which class has which methods and
;; fields.  This is necessary to determine which references to
;; subclasses really are references to a superclass because the fields
;; or methods have not been overriden.
;;
;; There is also a global table across all packages, which is a
;; map of which classes are subclasses of which other classes. This
;; makes it possible to investigate a class's subclasses when looking
;; for references.
;; 

(require 'jde-parse)
(require 'jde-parse-class)
(require 'tree-widget)

(defconst jde-xref-version "1.5")

(defgroup jde-xref nil
  "JDEE Class Cross-Reference (Refactoring) Options"
  :group 'jde
  :prefix "jde-xref-")

(defcustom jde-xref-db-base-directory nil
  "The path to store the directory which contains the database of
  which function calls which.  The data directory will be called
  \"xrefdb\" and will reside in the directory pointed to at this
  location "
  :group 'jde-xref
  :type 'directory)
  
(defcustom jde-xref-class-path nil
  "Similar to `jde-global-classpath', except this path should only
have those items that the user wants to be in the database of
functions calls (for the finding of callers to functions
functionality).  This does not have to correspond in any way to a
package structure, it merely has to be a list of places to recursively
descend to find all class files."
  :group 'jde-xref
  :type '(repeat (file :tag "Path")))

(defcustom jde-xref-store-prefixes nil
  "A list of what prefixes to put in the database.  A value of NIL
  indicates that to store everything (this will include which
  functions call java functions like println, etc.)  Otherwise, it is
  a list of prefixes such as: '(\"org\" \"jde\"), to keep track of all
  references to classes that start with \"org\" or \"jde\"."
  :group 'jde-xref
  :type '(repeat (string :tag "Prefix")))

(defcustom jde-xref-cache-size 3
  "How much package info to cache in memory at once.  The higher the
  variable is the more memory will be used, but the faster things
  should be."
  :group 'jde-xref
  :type 'integer)

(defvar jde-xref-stack nil
  "A list of the callers of a function, to be popped one at a time
  and shown to the user")

(defvar jde-xref-modified-classes nil
  "A list of modified classes, to be used in updating the caller table
  when a recompile happens")

(defvar jde-xref-parsed-classes nil
  "A global variable that is used to hold which classes have been parsed")

(defvar jde-xref-cache nil
  "A cache holding package information that will grow to size
  `jde-xref-cache-size'")

(defvar jde-xref-subclasses nil
  "A hashtable containing a list of which classes subclass which other
subclasses.")

(defun jde-xref-remove-all-from-directory (dir)
  (if (file-directory-p dir)
    (progn 
      (mapcar 'jde-xref-remove-all-from-directory
              (directory-files dir t "[^\\.]$"))
      (delete-directory dir))
    (delete-file dir)))

(defun jde-xref-pickle-hash (hash filename)
  "Store HASH in the file FILENAME.  The hash can be retrieved by
calling `jde-xref-unpickle-hash'."
  (when (file-exists-p filename)
    (delete-file filename))
  (save-excursion
    (let ((buf (find-file-noselect (jde-normalize-path filename))))
      (set-buffer buf)
      (goto-char (point-min))
      (erase-buffer)
      (insert "(")
      (maphash (lambda (key val)
                 (when val
                   (insert (concat "(" (prin1-to-string key) " . "
                                   (prin1-to-string val) ")\n" ))))
               hash)
      (insert ")")
      (save-buffer)
      (kill-buffer buf))))

(defun jde-xref-unpickle-hash (hash filename)
  "Populate a hash created by loading the contents of FILENAME to HASH.
FILENAME must be created by `jde-xref-pickle-hash'"
  (unless (file-exists-p filename)
    (error (concat "Cannot unpickle - file " filename " does not exist.  "
                   "The xref database may need to be recreated.")))
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (goto-char (point-min))
      (dolist (item (read buf))
        (puthash (car item) (cdr item) hash))
      (kill-buffer buf))))

(defun jde-xref-get-db-directory ()
  (concat (jde-normalize-path jde-xref-db-base-directory) "/xrefdb"))

;;;###autoload
(defun jde-xref-make-xref-db ()
  "Create a database of caller to callee (and the reverse) from the
classes in `jde-xref-class-path' and store the data in the location
specified by `jde-xref-db-file'"
  (interactive)
  (when (null jde-xref-db-base-directory)
    (error "The variable `jde-xref-db-base-directory' must be defined to make a caller database"))
  (when (null jde-xref-class-path)
    (error "The variable `jde-xref-class-path' must be defined to make a caller database"))
  (unless (file-exists-p (jde-xref-get-db-directory))
    (make-directory (jde-xref-get-db-directory)))
  (jde-xref-update-xref-db )
  (message "Finished creating xref database")
  (add-hook 'jde-compile-finish-hook 'jde-xref-update-caller-table)
  (add-hook 'after-save-hook 'jde-xref-file-saved))

(defun jde-xref-partial-match-member (str list)
  "Like `member' but works with strings and will return true if any of
the strings in LIST exist at the end of STR"
  (member-if (lambda (item) (string-match (concat (regexp-quote item) "$")
                                          str)) list))

(defun jde-xref-substring-member (str prefixlist)
  "Like `member' but works with strings and will return true if any of
  the prefixes in PREFIXLIST match STR"
  (member-if (lambda (item) (string=
                             (substring str 0 (min (length item)
                                                   (length str)))
                             item)) prefixlist))

(defun jde-xref-get-package-data ()
  (let ((data (make-hash-table :test 'equal :size 10))
        (caller-files (directory-files (jde-xref-get-db-directory)
                                       t "caller$")))
    (dolist (caller-file caller-files)
      (let* ((package (mapconcat (lambda (x) x)
                                 (butlast 
                                  (split-string (car (last
                                                      (split-string caller-file
                                                                    "/")))
                                                "-")) "-"))
             (package-data (jde-xref-load-package-hashes package)))
        (puthash package package-data data)))
    data))

(defun jde-xref-update-xref-db (&optional only-classes)
  (let ((origdir default-directory)
        (origbuf (current-buffer))
        (package-data (if only-classes
                        (jde-xref-get-package-data)
                        (make-hash-table :test 'equal :size 10)))
        (subclasses (make-hash-table :test 'equal :size 500)))
    ;; Remove all occurances of classes to be updated from the package-data's caller-hashes
    (when only-classes
      (maphash (lambda (package single-package-data)
                 (maphash (lambda (callee callers)
                            (puthash callee
                                     (remove-if (lambda (item)
                                                  (member (car item)
                                                          only-classes))
                                                callers)
                                     (nth 0 single-package-data)))
                          (nth 0 single-package-data)))
               package-data))
    (unwind-protect
      (progn 
        (dolist (path jde-xref-class-path)
          (let ((normalized-path (jde-normalize-path path)))
            (unless (file-exists-p normalized-path)
              (error (concat "Could not find file or directory "
                     normalized-path)))
            (if (file-directory-p normalized-path)
              (jde-xref-make-db-from-path normalized-path
                                          package-data
                                          subclasses
                                          only-classes)
              ;; we're a directory, assume we are a jar file
              (let ((dir (concat temporary-file-directory "/"
                                 (make-temp-name "jde-xref"))))
                (make-directory dir)
                (cd dir)
                (let ((buf (get-buffer-create "*Jar output*")))
                  (unless (eq (call-process (jde-get-jdk-prog 'jar) nil buf nil "-xf"
                                            (expand-file-name normalized-path)) 0)
                    (error (concat "Could not unjar file " (expand-file-name
                                                            normalized-path)
                                   ".  See *Jar output* buffer for details")))
                  (kill-buffer buf))
                (jde-xref-make-db-from-path dir
                                            package-data
                                            subclasses
                                            only-classes)
                (jde-xref-remove-all-from-directory dir))))))
      (set-buffer origbuf)
      (cd origdir)
      (setq jde-xref-parsed-classes nil)
      (jde-xref-pickle-hash subclasses (jde-xref-get-subclass-file))
      (setq jde-xref-subclasses subclasses)
      (maphash (lambda (package data)
                 (jde-xref-pickle-hash (nth 0 data) 
                                       (jde-xref-get-caller-file package))
                 (jde-xref-pickle-hash (nth 1 data)
                                       (jde-xref-get-interface-file package))
                 (jde-xref-pickle-hash (nth 2 data)
                                       (jde-xref-get-member-file package)))
               package-data))))


(defun jde-xref-path-in-classes-p (path classes)
  "Returns true if the PATH looks like it represents a class in CLASSES"
  (jde-xref-partial-match-member
   (replace-regexp-in-string "\\.[Cc][Ll][Aa][Ss][Ss]$" "" 
                             (replace-regexp-in-string "/\\|\\$" "." path))
                             classes))

(defun jde-xref-create-package-hashes (&optional fake)
  "Returns a list of the three hashes that are in a package's data.
The hashes are for the caller-hash, the interface-hash and the
member-hash.  FAKE determines if we are just creating them so that
there is something to check against.  In those circumstance we just
create tiny hashes to conserve memory."
  (list (make-hash-table :test 'equal :size (if fake 1 100))
        (make-hash-table :test 'equal :size (if fake 1 20))
        (make-hash-table :test 'equal :size (if fake 1 100))))

(defun jde-xref-load-package-hashes (package)
  (let ((data (jde-xref-create-package-hashes)))
    (jde-xref-unpickle-hash (nth 0 data)
                            (jde-xref-get-caller-file package))
    (jde-xref-unpickle-hash (nth 1 data)
                            (jde-xref-get-interface-file package))
    (jde-xref-unpickle-hash (nth 2 data)
                            (jde-xref-get-member-file package))
    data))

(defun jde-xref-append-hash (key value hash)
  "Like `puthash' but appends VALUE to the HASH at KEY"
  (puthash key (append (gethash key hash) (if (listp value)
                                            value
                                            (list value))) hash))

(defun jde-xref-make-db-from-path (path package-data subclasses
                                        &optional only-classes)
  (if (file-directory-p path)
    (dolist (path (directory-files path t "[^.]$"))
      (jde-xref-make-db-from-path path package-data
                                  subclasses
                                  only-classes))
    (when (and (string-match "\.[Cc][Ll][Aa][Ss][Ss]$" path)
               (or (null only-classes) (jde-xref-path-in-classes-p
                                        path only-classes))
               (not (jde-xref-path-in-classes-p path jde-xref-parsed-classes)))
      (message (concat "Parsing class " path))
      (let ((info (jde-parse-class path)))
        (add-to-list 'jde-xref-parsed-classes
                     (jde-parse-class-extract-classname info))
        (let ((package (jde-parse-get-package-from-name
                        (jde-parse-class-extract-classname info))))
          ;; If there is no existing package data
          (unless (gethash package package-data)
            (puthash package
                     ;; package-data's values are (caller-hash
                     ;; interface-hash method-and-field-hash)
                     (jde-xref-create-package-hashes)
                     package-data))
          (destructuring-bind (caller-hash interface-hash
                               method-and-field-hash)
              (gethash package package-data)
            (puthash (jde-parse-class-extract-classname info)
                     (jde-parse-class-extract-interfaces info)
                     interface-hash)
            (puthash (jde-parse-class-extract-classname info)
                     (append (jde-parse-class-extract-method-signatures info)
                             (jde-parse-class-extract-field-signatures info))
                     method-and-field-hash)
            (jde-xref-append-hash
             (jde-parse-class-extract-superclass info)
             (jde-parse-class-extract-classname info) subclasses)
            (dolist (call (nreverse
                           (jde-parse-class-extract-method-calls info)))
              (let ((calls (car call))
                    (called (cadr call)))
                (if (or (not jde-xref-store-prefixes)
                        (and
                          (jde-xref-substring-member (car calls)
                                                     jde-xref-store-prefixes)
                          (jde-xref-substring-member (car called)
                                                     jde-xref-store-prefixes)))
                  (let* ((dqcalled (list (car called)
                                        (nth 1 called)
                                        (when (nth 2 called)
                                          (jde-parse-get-unqualified-name
                                           (nth 2 called)))
                                        ;; We don't want to need to
                                        ;; know the constructor args
                                        ;; for anonymous classes
                                        (unless (jde-xref-is-class-anonymous (car called))
                                          (mapcar 'jde-parse-get-unqualified-name
                                                  (nth 3 called)))))
                         (called-package (jde-parse-get-package-from-name
                                          (car dqcalled))))
                    ;; Create the package data if needed
                    (unless (gethash called-package package-data)
                      (puthash called-package (jde-xref-create-package-hashes)
                               package-data))
                    (let* ((called-package-hashes
                            (gethash called-package package-data))
                           (called-package-caller-hash
                             (car called-package-hashes)))
                      ;; add things to the table - making sure there are no duplicates
                      (puthash dqcalled
                               (if (member calls (gethash
                                                  dqcalled
                                                  called-package-caller-hash))
                                 (gethash dqcalled called-package-caller-hash)
                                 (cons calls
                                       (gethash dqcalled
                                                called-package-caller-hash)))
                               called-package-caller-hash))))))))))))

(defun jde-xref-class-and-token-to-signature (class token)
  (list (semantic-token-token token)
        class
        (if (equal (semantic-token-name token)
                   (jde-parse-get-unqualified-name
                    class))
          "<init>"
          (semantic-token-name token))
        (when (eq (semantic-token-token token) 'function)
          (if (or (not (nth 2 token)) (equal (nth 2 token) "void"))
            nil
            (jde-parse-get-unqualified-name (nth 2 token))))
        (if (eq (semantic-token-token token) 'function)
          (mapcar (lambda (arg) (jde-parse-get-unqualified-name (nth 2 arg)))
                  (semantic-token-function-args token))
          (list (jde-parse-get-unqualified-name (nth 2 token))))))

(defun jde-xref-get-current-class ()
  (concat (jde-parse-get-package-name) (when (jde-parse-get-package-name) ".")
          (or (car (jde-parse-get-innermost-class-at-point))
              (jde-parse-get-buffer-class))))

(defun jde-xref-get-current-signature ()
  (unless (member
           (semantic-token-token (semantic-current-nonterminal))
                '(function variable))
    (error "The cursor must be in a function or class variable to get the callers"))
  (jde-xref-class-and-token-to-signature
   (jde-xref-get-current-class)
   (semantic-current-nonterminal)))

;;;###autoload
(defun jde-xref-first-caller (strict)
  "Put the list of who calls the current function on the stack and
display the first caller.  Subsequent callers are displayed through
`jde-xref-show-next-caller'.  STRICT should be true if the callers of
interfaces to a function should not be considered"
  (interactive "P")
  (jde-xref-load-subclasses-table-if-necessary)
  (setq jde-xref-stack (jde-xref-get-callers
                            (jde-xref-get-current-signature) strict))
  (jde-xref-next-caller))

(defun jde-xref-goto-caller (caller)
  (jde-find-class-source (car caller))
  (goto-line (nth 4 caller)))

;;;###autoload
(defun jde-xref-next-caller ()
  "If there are items still on the caller stack, pop the first one off
and show it"
  (interactive)
  (if jde-xref-stack
    (jde-xref-goto-caller (pop jde-xref-stack))
    (message "No more calls")))

(defun jde-xref-load-subclasses-table-if-necessary ()
  (unless jde-xref-subclasses
    (setq jde-xref-subclasses (make-hash-table :test 'equal :size 500))
    (jde-xref-unpickle-hash jde-xref-subclasses
                            (jde-xref-get-subclass-file))
    ;; if subclasses were empty, then it's the first time this is run,
    ;; so do our one-time initializations
    (add-hook 'jde-compile-finish-hook 'jde-xref-compile-hook)
    (add-hook 'after-save-hook 'jde-xref-file-saved)))

(defun jde-xref-signature-to-string (sig)
  (concat (or (nth 3 sig) "void") " " (cadr sig) "."
          (if (equal (nth 2 sig) "<init>")
            (jde-parse-get-unqualified-name (cadr sig))
            (nth 2 sig))
          (when (eq (car sig) 'function)
            (concat "("
                    (mapconcat (lambda (x) x) (nth 4 sig) ",") ")"))))

(defun jde-xref-find-package-in-cache (package cache)
  (when cache
    (if (equal (caar cache) package)
      (cdar cache)
      (jde-xref-find-package-in-cache package (cdr cache)))))

(defun jde-xref-get-caller-file (package)
  (concat (jde-xref-get-db-directory) "/" package "-caller"))

(defun jde-xref-get-interface-file (package)
  (concat (jde-xref-get-db-directory) "/" package "-interfaces"))

(defun jde-xref-get-member-file (package)
  (concat (jde-xref-get-db-directory) "/" package "-members"))

(defun jde-xref-get-subclass-file ()
  (concat (jde-xref-get-db-directory) "/subclasses"))

(defun jde-xref-find-or-create-package-in-cache (package)
  (unless jde-xref-db-base-directory
    (error "The variable `jde-xref-db-base-directory' must be specified to load the xref db"))
  (if (jde-xref-substring-member package jde-xref-store-prefixes)
    (or (jde-xref-find-package-in-cache package jde-xref-cache)
        ;; Or we need to get the new package and put it in the cache
        (let ((data (jde-xref-load-package-hashes package)))
        (setq jde-xref-cache (cons (cons package data)
                                   (if (> (length jde-xref-cache)
                                          jde-xref-cache-size)
                                       (cdr jde-xref-cache)
                                     jde-xref-cache)))
        data))
    (jde-xref-create-package-hashes t)))

(defun jde-xref-get-caller-hash (package)
  (nth 0 (jde-xref-find-or-create-package-in-cache package)))

(defun jde-xref-get-interface-hash (package)
  (nth 1 (jde-xref-find-or-create-package-in-cache package)))

(defun jde-xref-get-member-hash (package)
  (nth 2 (jde-xref-find-or-create-package-in-cache package)))

(defun jde-xref-get-basic-caller (sig)
  (gethash (cdr sig) (jde-xref-get-caller-hash (jde-parse-get-package-from-name
                                                (nth 1 sig)))))

(defun jde-xref-get-members (class)
  (gethash class (jde-xref-get-member-hash (jde-parse-get-package-from-name
                                            class))))

(defun jde-xref-is-class-anonymous (class)
  (string-match "\\$[0-9]+$" class))

(defun jde-xref-is-caller-anonymous-class (caller)
  (jde-xref-is-class-anonymous (nth 0 caller)))

(defun jde-xref-is-sig-anonymous-class (sig)
  (jde-xref-is-class-anonymous (nth 1 sig)))

(defun jde-xref-get-callers (sig &optional strict)
  (let ((package (jde-parse-get-package-from-name (nth 1 sig)))
        (interface-callers nil))
    (append
     ;; if we're an anonymous class, then we want to see where we are
     ;; created, since it kind of goes along with usage of the function.
     (when (jde-xref-is-sig-anonymous-class sig)
       (jde-xref-get-basic-caller (list (nth 0 sig) (nth 1 sig)
                                        "<init>" nil nil)))
     (unless strict
       (dolist (interface (gethash (nth 1 sig)
                                   (jde-xref-get-interface-hash
                                    package))
                          interface-callers)
         (setq interface-callers
               (append interface-callers
                       (jde-xref-get-basic-caller
                        (cons 'function
                              (cons interface (cddr sig))))))))
     (jde-xref-get-basic-caller sig)
     (mapcan (lambda (subclass)
               (unless (member (cddr sig)
                               (jde-xref-get-members
                                subclass))
                 (jde-xref-get-callers
                  `(,(car sig) ,subclass ,@(cddr sig)) strict)))
             (gethash (nth 1 sig) jde-xref-subclasses)))))

(defun jde-xref-notify (widget child &optional event)
  (jde-xref-goto-caller (widget-get widget :caller)))

(defun jde-xref-caller-to-sig (caller)
  (list 'function (nth 0 caller) (nth 1 caller) (when (nth 2 caller) (jde-parse-get-unqualified-name (nth 2 caller)))
        (mapcar 'jde-parse-get-unqualified-name (nth 3 caller))))

(defun jde-xref-tree-get-children (sig)
  (when sig
    (mapcar
     (lambda (caller)
       (let  ((caller-sig (jde-xref-caller-to-sig caller)))
         (list
          'tree-widget
          :node `(push-button
                  :tag ,(jde-xref-signature-to-string caller-sig)
                  :format "%[%t%]\n"
                  :sig ,caller-sig
                  :caller ,caller
                  :notify jde-xref-notify)
          :dynargs 'jde-xref-tree-get-children-from-tree
          :sig caller-sig
          :has-children t)))
     (jde-xref-get-callers sig))))
  
(defun jde-xref-tree-get-children-from-tree (tree)
  (jde-xref-tree-get-children (widget-get tree :sig)))

;;;###autoload
(defun jde-xref-display-call-tree (strict)
  "Display an interactive call tree of which function call the current
  function, which can be expanded outward.  STRICT should be true if
  the callers of interfaces to a function should not be considered"
  (interactive "P")
  (jde-xref-load-subclasses-table-if-necessary)
  (let* ((sig (jde-xref-get-current-signature))
         (buf (get-buffer-create (concat "JDE call graph for "
                                         (jde-xref-signature-to-string
                                          sig)))))
    (switch-to-buffer buf)
    (erase-buffer)
    (widget-create 'tree-widget
                   :tag (jde-xref-signature-to-string sig)
                   :dynargs 'jde-xref-tree-get-children-from-tree
                   :has-children t
                   :sig sig)
    (use-local-map widget-keymap)
    (widget-setup)))

(defun jde-xref-get-class-variables (class-token)
  (mapcan (lambda (token) (when (eq (semantic-token-token token)
                                    'variable)))
          ;; semantic-nonterminal-children does not seem to work
          (nth 3 class-token)))

;;;###autoload
(defun jde-xref-list-uncalled-functions (strict)
  "Displays a simple list of function that are never called, at least
not directly.  Do not assume that this means this code can never be
reached, since reflection could always call any method.  Use this list
and your best judgement to figure out what are good candidates for
code cleanup.  STRICT should be set to true if the callers of
interfaces to a function should not be considered.  This function could
take a while. If it does, you might want to consider increasing
`jde-xref-cache-size'."
  (interactive "P")
  (jde-xref-load-subclasses-table-if-necessary)
  (save-excursion
    (flet ((get-unused-string (token)
             (goto-char (semantic-token-start token))
             (unless (jde-xref-get-callers
                      (jde-xref-class-and-token-to-signature
                       (jde-xref-get-current-class) token) strict)
               (list (jde-xref-signature-to-string
                      (jde-xref-class-and-token-to-signature
                       (jde-xref-get-current-class) token))))))
    (let ((uncalled-methods
            (mapcan 'get-unused-string
                    (semantic-find-nonterminal-by-token 'function
                                                        (current-buffer)
                                                        t)))
          (unreferenced-variables
            (mapcan 'get-unused-string
                    (mapcan 'jde-xref-get-class-variables
                            (semantic-find-nonterminal-by-type "class"
                                                               (current-buffer)
                                                               t))))
          (outbuf (get-buffer-create "Unreferenced Methods and Members")))
      (switch-to-buffer outbuf)
      (erase-buffer)
      (insert "The following is a list of methods and members that are\n")
      (insert "uncalled directly by any Java classes that are in the\n")
      (insert "following locations: \n")
      (insert (mapconcat (lambda (x) x) jde-xref-class-path ", "))
      (newline)
      (newline)
      (if uncalled-methods
        (progn
          (insert "Unreferenced methods:\n")
          (insert (mapconcat (lambda (x) x) uncalled-methods "\n")))
        (insert "There are no uncalled methods\n\n"))
      (if unreferenced-variables
        (progn
          (insert "\n\nUnreferenced class variables:\n")
          (insert (mapconcat (lambda (x) x) unreferenced-variables "\n")))
        (insert "\n\nThere are no unreferenced variables\n\n"))
      (toggle-read-only)
      (not-modified)))))

(defun jde-xref-remove-classes-from-subclasses-table (classes)
  (maphash (lambda (key value)
             (puthash key
                      (remove-if (lambda (item)
                                   (member item classes)) value)
                      jde-xref-subclasses))
           jde-xref-subclasses))

;;;###autoload
(defun jde-xref-update (&rest ignored)
  "Update the caller table after a recompile.  This can be called by
the user when they recompile outside of emacs.  It will update the
call list of all files modified in emacs"
  (interactive)
  (message "Updating xref tables")
  (when jde-xref-modified-classes
    (jde-xref-remove-classes-from-subclasses-table
     jde-xref-modified-classes)
    (jde-xref-update-xref-db jde-xref-modified-classes)
    (message "Finished updateing xref database"))
  (setq jde-xref-modified-classes nil))

(defun jde-xref-file-saved ()
  (when (eq major-mode 'jde-mode)
    (setq jde-xref-modified-classes
          (append jde-xref-modified-classes
                  (mapcar (lambda (class-token)
                            (concat (jde-parse-get-package-name)
                                    (when (jde-parse-get-package-name) ".")
                                    (semantic-token-name class-token)))
                          (semantic-find-nonterminal-by-type
                           "class" (current-buffer) t))))))

;;;###autoload
(defun jde-xref-customize ()
  "Display the customization buffer for the xref package."
  (interactive)
  (customize-group "jde-xref"))

(global-set-key (kbd "C-c C-v a") 'jde-xref-first-caller)
(global-set-key (kbd "C-c C-v n") 'jde-xref-next-caller)


;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-xref)

;; $Log: jde-xref.el,v $
;; Revision 1.10  2002/12/18 04:09:15  ahyatt
;; Fixes a usability problem with call-trees, and fixes a bug where some
;; classes never get added to the xref database
;;
;; Revision 1.9  2002/12/06 03:24:45  ahyatt
;; Fixed bug in call-tree, where no expansion could take place after the
;; 2nd level. Also renamed the argument to jde-xref-goto-caller, which takes
;; a caller, not a sig.
;;
;; Revision 1.8  2002/12/04 04:29:19  ahyatt
;; Got the call-tree to finally work.  Also made some adjustements for
;; anonymous classes.  These adjustments probably will disappear with
;; some later rework I have planned.
;;
;; Revision 1.6  2002/12/01 02:50:48  ahyatt
;; Major revision - I realized that the previous version had neglected to
;; factor in subclasses of a class when computing the callers to a
;; function.  I added this functionality, which meant I had to now keep
;; track of all subclasses and variables and functions of each class
;; (because we have to walk up the subclass tree until a function or
;; variable gets overriden).  Also, I simultaneously totally redid the
;; database storage.  Instead of storing the result in a huge hash table,
;; we now split the file into packages which are loaded on demand and
;; cached for later use.  I removed user visible variables
;; `jde-xref-db-file' and `jde-xref-interface-file' and replaced it with
;; `jde-xref-db-base-directory', a directory on which an "xrefdb"
;; directory is created which stores everything necessary.  BTW, there is
;; a reason I'm not using the beanshell to get this information -
;; basically I don't see how to handle arrays properly via reflection,
;; and without getting the array information I need from signatures, I
;; can't match the signatures I'm getting from the class files directly.
;;
;; Revision 1.5  2002/11/25 00:42:34  ahyatt
;; Polished jde-xref-list-uncalled-functions, making the text make more
;; sense, the buffer read-only, and converting "<init>" functions back to
;; their proper names.
;;
;; Revision 1.4  2002/11/21 04:26:33  ahyatt
;; Changed my e-mail address to andy_jde@thehyatts.net
;;
;; Revision 1.3  2002/11/21 04:18:41  paulk
;; These packages, when autoloaded, now register and initialize the customization variables
;; that they define to the values specified in the current project file.
;;
;; Revision 1.2  2002/11/21 04:03:47  ahyatt
;; Fixed a bug in jde-parse-class where two functions had the same
;; definition.  The fix involved renamed a few classes to keep
;; consistency.  jde-xref had to change as well.
;;
;; Removed a (newline) function call in jde-xref and replaced it with a "\n"
;;
;; In jde-xref, rewrote the parts of jde-xref-make-db-from-path which
;; dealt with keeping track of the already parsed classes. Previous
;; solution was really kludgey, this solution is only somewhat kludgey.
;; I'm still thinking about this problem.
;;
;; Revision 1.1  2002/11/18 07:02:18  paulk
;; Initial release.
;;

;; end of jde-xref.el