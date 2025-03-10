;;; semanticdb.el --- Semantic tag database manager

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb.el,v 1.69 2004/03/19 23:40:51 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;; 
;;; Commentary:
;;
;; Maintain a database of tags for a group of files and enable
;; queries into the database.
;;
;; By default, assume one database per directory.
;;

(require 'inversion)
(eval-and-compile
  (inversion-require 'eieio "0.18beta1"))
(require 'eieio-base)
(require 'semantic)

;;; Variables:
(defgroup semanticdb nil
  "Parser Generator Persistent Database interface."
  :group 'semantic
  )

;;;###autoload
(defcustom semanticdb-global-mode nil
  "*If non-nil enable the use of `semanticdb-minor-mode'."
  :group 'semantic
  :type 'boolean
  :require 'semanticdb
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semanticdb-minor-mode (if val 1 -1))
         (custom-set-default sym val)))

(defcustom semanticdb-mode-hooks nil
  "*Hooks run whenever `global-semanticdb-minor-mode' is run.
Use `semanticdb-minor-mode-p' to determine if the mode has been turned
on or off."
  :group 'semanticdb
  :type 'hook)

(defvar semanticdb-database-list nil
  "List of all active databases.")

(defvar semanticdb-semantic-init-hook-overload nil
  "Semantic init hook overload.
Tools wanting to specify the file names of the semantic database
use this.")

(defvar semanticdb-current-database nil
  "For a given buffer, this is the currently active database.")
(make-variable-buffer-local 'semanticdb-current-database)

(defvar semanticdb-current-table nil
  "For a given buffer, this is the currently active database table.")
(make-variable-buffer-local 'semanticdb-current-table)

(defvar semanticdb-new-database-class 'semanticdb-project-database-file
  "The default type of database created for new files.
This can be changed on a per file basis, so that some directories
are saved using one mechanism, and some directories via a different
mechanism.")
(make-variable-buffer-local 'semanticdb-new-database-class)

;;; Classes:
(defclass semanticdb-abstract-table ()
  ((parent-db ;; :initarg :parent-db
    ;; Do not set an initarg, or you get circular writes to disk.
	      :documentation "Database Object containing this table.")
   (major-mode :initarg :major-mode
	       :initform nil
	       :documentation "Major mode this table belongs to.
Sometimes it is important for a program to know if a given table has the
same major mode as the current buffer.")
   (tags :initarg :tags
	 :accessor semanticdb-get-tags
	 :documentation "The tags belonging to this table.")
   )
  "A simple table for semantic tags.
This table is the root of tables, and contains the minimum needed
for a new table not associated with a buffer."
  :abstract t)

(defmethod semanticdb-get-buffer ((obj semanticdb-abstract-table))
  "Return a buffer associated with OBJ.
If the buffer is not in memory, load it with `find-file-noselect'."
  nil)

(defclass semanticdb-table (semanticdb-abstract-table)
  ((file :initarg :file
	 :documentation "File name relative to the parent database.
This is for the file whose tags are stored in this TABLE object.")
   (pointmax :initarg :pointmax
	     :initform nil
	     :documentation "Size of buffer when written to disk.
Checked on retrieval to make sure the file is the same.")
   (unmatched-syntax :initarg :unmatched-syntax
		     :documentation
		     "List of vectors specifying unmatched syntax.")
   )
  "A single table of tags derived from file.")

(defmethod object-print ((obj semanticdb-table) &rest strings)
  "Pretty printer extension for `semanticdb-abstract-table'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (format " (%d tags)" (length (semanticdb-get-tags obj)))
	       strings)))

(defclass semanticdb-project-database (eieio-instance-tracker)
  ((tracking-symbol :initform semanticdb-database-list)
   (reference-directory :type string
			:documentation "Directory this database refers to.
When a cache directory is specified, then this refers to the directory
this database contains symbols for.")
   (new-table-class :initform semanticdb-table
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   (tables :initarg :tables
	   :type list
	   ;; Need this protection so apps don't try to access
	   ;; the tables without using the accessor.
	   :accessor semanticdb-get-database-tables
	   :protection :protected
	   :documentation "List of `semantic-db-table' objects."))
  "Database of file tables.")

;;; Code:
(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-project-database) directory)
  "Create a new semantic database of class DBC for DIRECTORY and return it.
If a database for DIRECTORY has already been created, return it.
If DIRECTORY doesn't exist, create a new one."
  (let ((db (semanticdb-directory-loaded-p directory)))
    (unless db
      (setq db (semanticdb-project-database
		(file-name-nondirectory filename)
		:tables nil))
      ;; Set this up here.   We can't put it in the constructor because it
      ;; would be saved, and we want DB files to be portable.
      (oset db reference-directory directory))
    db))

(defmethod semanticdb-flush-database-tables ((db semanticdb-project-database))
  "Reset the tables in DB to be empty."
  (oset db tables nil))

(defmethod semanticdb-create-table ((db semanticdb-project-database) file)
  "Create a new table in DB for FILE and return it.
The class of DB contains the class name for the type of table to create.
If the table for FILE exists, return it.
If the table for FILE does not exist, create one."
  (let ((newtab (semanticdb-file-table db file)))
    (unless newtab
      ;; This implementation will satisfy autoloaded classes
      ;; for tables.
      (setq newtab (funcall (oref db new-table-class)
			    (file-name-nondirectory file)
			    :file (file-name-nondirectory file)
			    ))
      (oset newtab parent-db db)
      (object-add-to-list db 'tables newtab t))
    newtab))

(defun semanticdb-get-database (filename)
  "Get a database for FILENAME.
If one isn't found, create one."
  (semanticdb-create-database semanticdb-new-database-class filename))

(defun semanticdb-directory-loaded-p (path)
  "Return the project belonging to PATH if it was already loaded."
  (eieio-instance-tracker-find path 'reference-directory 'semanticdb-database-list))

(defmethod semanticdb-file-table ((obj semanticdb-project-database) filename)
  "From OBJ, return FILENAME's associated table object."
  (object-assoc (file-relative-name (expand-file-name filename)
  				    (oref obj reference-directory))
		'file (oref obj tables)))

(defmethod semanticdb-get-buffer ((obj semanticdb-table))
  "Return a buffer associated with OBJ.
If the buffer is not in memory, load it with `find-file-noselect'."
  (find-file-noselect (semanticdb-full-filename obj)))

(defmethod semanticdb-set-buffer ((obj semanticdb-table))
  "Set the current buffer to be a buffer owned by OBJ.
If OBJ's file is not loaded, read it in first."
  (set-buffer (semanticdb-get-buffer obj)))

(defmethod semanticdb-refresh-table ((obj semanticdb-table))
  "If the tag list associated with OBJ is loaded, refresh it.
This will call `semantic-fetch-tags' if that file is in memory."
  (let ((ff (semanticdb-full-filename obj)))
    (if (get-file-buffer ff)
	(save-excursion
	  (semanticdb-set-buffer obj)
	  (semantic-fetch-tags)))))

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table))
  "Return non-nil of OBJ's tag list is out of date.
The file associated with OBJ does not need to be in a buffer."
  (let ((buff (get-file-buffer (semanticdb-full-filename obj)))
	)
    (if buff
	(save-excursion
	  (set-buffer buff)
	  ;; Use semantic's magic tracker to determine of the buffer is up
	  ;; to date or not.
	  (not (semantic-parse-tree-up-to-date-p))
	  ;; We assume that semanticdb is keeping itself up to date.
	  ;; via all the clever hooks
	  )
      ;; Buffer isn't loaded.  The only clue we have is if the file
      ;; is somehow different from our mark in the semanticdb table.
      (let* ((stats (file-attributes (semanticdb-full-filename obj)))
	     (actualmax (aref stats 7)))

	(or (not (slot-boundp obj 'tags))
	    (not (oref obj tags))
	    (/= (or (oref obj pointmax) 0) actualmax)
	    )
	))))

(defmethod semanticdb-save-db ((DB semanticdb-project-database))
  "Cause a database to save itself.
The database base class does not save itself persistently.
Subclasses could save themselves to a file, or to a database, or other
form."
  nil)

(defun semanticdb-save-current-db ()
  "Save the current tag database."
  (interactive)
  (message "Saving current tag summaries...")
  (semanticdb-save-db semanticdb-current-database)
  (message "Saving current tag summaries...done"))

(defun semanticdb-save-all-db ()
  "Save all semantic tag databases."
  (interactive)
  (message "Saving tag summaries...")
  (mapcar 'semanticdb-save-db semanticdb-database-list)
  (message "Saving tag summaries...done"))

;;; Directory Project support
;;
(defvar semanticdb-project-predicate-functions nil
  "List of predicates to try that indicate a directory belongs to a project.
This list is used when `semanticdb-persistent-path' contains the value
'project.  If the predicate list is nil, then presume all paths are valid.

Project Management software (such as EDE and JDE) should add their own
predicates with `add-hook' to this variable, and semanticdb will save tag
caches in directories controlled by them.")

(defmethod semanticdb-write-directory-p ((obj semanticdb-project-database))
  "Return non-nil if OBJ should be written to disk.
Uses `semanticdb-persistent-path' to determine the return value."
  nil)

;;; Utilities
;;
;; What is the current database, are two tables of an equivalent mode,
;; and what databases are a part of the same project.
(defun semanticdb-current-database ()
  "Return the currently active database."
  (or semanticdb-current-database
      (and default-directory
	   (semanticdb-create-database semanticdb-new-database-class
				       default-directory)
	   )
      nil))

(defmethod semanticdb-equivalent-mode ((table semanticdb-abstract-table) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (if buffer (set-buffer buffer))
    (or
     ;; nil means the same as major-mode
     (and (not semantic-equivalent-major-modes)
	  (eq major-mode (oref table major-mode)))
     (and semantic-equivalent-major-modes
	  (member (oref table major-mode) semantic-equivalent-major-modes)))
    ))

(defvar semanticdb-dir-sep-char (if (boundp 'directory-sep-char)
				    (symbol-value 'directory-sep-char)
				  ?/)
  "Character used for directory separation.
Obsoleted in some versions of Emacs.  Needed in others.")

(defun semanticdb-fix-pathname (path)
  "If PATH is broken, fix it.
Force PATH to end with a /."
  (if (not (= semanticdb-dir-sep-char (aref path (1- (length path)))))
      (concat path (list semanticdb-dir-sep-char))
    path))

(defmethod semanticdb-printable-name ((table semanticdb-table))
  "Return a string which is a short and logical printable name for TABLE.
Use this instead of getting the :file slot of the table, which can
sometimes be unbound."
  ;; I know I said that the above is sometimes unbound.
  ;; Not that if this line throws an error, you should go to
  ;; the subclass, and override this method.
  (file-name-nondirectory (file-name-sans-extension (oref table file)))
  )

;;; Associations
;;
;; These routines determine associations between a file, and multiple
;; associated databases.

(defcustom semanticdb-project-roots nil
  "*List of directories, where each directory is the root of some project.
All subdirectories of a root project are considered a part of one project.
Values in this string can be overriden by project management programs
via the `semanticdb-project-root-functions' variable."
  :group 'semanticdb
  :type '(repeat string))

(defvar semanticdb-project-root-functions nil
  "List of functions used to determine a given directories project root.
Functions in this variable can override `semanticdb-project-roots'.
Functions set in the variable are given one argument (a directory) and
must return a string, (the root directory).  This variable should be used
by project management programs like EDE or JDE.")

(defvar semanticdb-project-system-databases nil
  "List of databases containing system library information.
Mode authors can create their own system databases which know
detailed information about the system libraries for querying purposes.
Put those into this variable as a buffer-local, or mode-local
value.")
(make-variable-buffer-local 'semanticdb-project-system-databases)

(defvar semanticdb-search-system-databases t
  "Non nil if search routines are to include a system database.")

(defun semanticdb-current-database-list (&optional dir)
  "Return a list of databases associated with the current buffer.
If optional argument DIR is non-nil, then use DIR as the starting directory.
If this buffer has a database, but doesn't have a project associated
with it, return nil.
First, it checks `semanticdb-project-root-functions', and if that
has no results, it checks `semanticdb-project-roots'.  If that fails,
it returns the results of function `semanticdb-current-database'.
Always append `semanticdb-project-system-databases' if
`semanticdb-search-system' is non-nil."
  (let ((root nil)			; found root directory
	(dbs nil)			; collected databases
	(roots semanticdb-project-roots) ;all user roots
	(dir (or dir default-directory))
	)
    ;; Find the root based on project functions.
    (setq root (run-hook-with-args-until-success
		'semanticdb-project-root-functions
		dir))
    ;; Find roots based on strings
    (while (and roots (not root))
      (if (string-match (concat "^"
				(regexp-quote
				 (expand-file-name (car roots))))
			(expand-file-name dir))
	  (setq root (car roots)))
      (setq roots (cdr roots)))
    ;; Find databases based on the root directory.
    (when root
      (let ((regexp (concat "^" (regexp-quote (expand-file-name root))))
	    (adb semanticdb-database-list) ; all databases
	    )
	(while adb
	  ;; I don't like this part, but close enough.
	  (if (and (slot-exists-p (car adb) 'file)
		   (slot-boundp (car adb) 'reference-directory)
		   (string-match regexp (oref (car adb) reference-directory)))
	      (setq dbs (cons (car adb) dbs)))
	  (setq adb (cdr adb)))))
    ;; Add in system databases
    (when semanticdb-search-system-databases
      (setq dbs (append dbs semanticdb-project-system-databases)))
    ;; Return
    dbs))


;;; Hooks:
;;
(defun semanticdb-semantic-init-hook-fcn ()
  "Function saved in `find-file-hooks'.
Sets up the semanticdb environment."
  (let ((cdb nil)
	(ctbl nil))
    ;; Allow a database override function
    (when (not (and semanticdb-semantic-init-hook-overload
		    (setq cdb (run-hooks 'semanticdb-semantic-init-hook-overload))))
      (setq cdb (semanticdb-create-database semanticdb-new-database-class
					    default-directory))
      )
    ;; Get the current DB for this directory
    (setq semanticdb-current-database cdb)
    ;; Get a table for this file.
    (setq ctbl (semanticdb-create-table cdb (buffer-file-name)))
    ;; We set the major mode because we know what it is.
    (oset ctbl major-mode major-mode)
    ;; Local state
    (setq semanticdb-current-table ctbl)
    ;; Try to swap in saved tags
    (if (or (not (slot-boundp ctbl 'tags)) (not (oref ctbl tags))
	    (/= (or (oref ctbl pointmax) 0) (point-max))
	    )
	(semantic-clear-toplevel-cache)
      (condition-case nil
          (semantic-set-unmatched-syntax-cache
           (oref ctbl unmatched-syntax))
        (unbound-slot
         ;; Old version of the semanticdb table can miss the unmatched
         ;; syntax slot.  If so, just clear the unmatched syntax cache.
         (semantic-clear-unmatched-syntax-cache)
	 ;; Make sure it has a value.
	 (oset ctbl unmatched-syntax nil)
	 ))
      (semantic--set-buffer-cache (oref ctbl tags))
      (semantic--tag-link-cache-to-buffer)
      )
    ))

(defun semanticdb-synchronize-table (new-table)
  "Function run after parsing.
Argument NEW-TABLE is the new table of tags."
  (if semanticdb-current-table
      (oset semanticdb-current-table tags new-table)))

(defun semanticdb-kill-hook ()
  "Function run when a buffer is killed.
If there is a semantic cache, slurp out the overlays, an store
it in our database.  If that buffer has not cache, ignore it, we'll
handle it later if need be."
  (if (and (semantic-active-p)
	   semantic--buffer-cache
	   semanticdb-current-table)
      (progn
	(oset semanticdb-current-table pointmax (point-max))
	(condition-case nil
	    (semantic--tag-unlink-cache-from-buffer)
	  ;; If this messes up, just clear the system
	  (error
	   (semantic-clear-toplevel-cache)
	   (message "semanticdb: Failed to deoverlay tag cache."))))
    ))

(defun semanticdb-kill-emacs-hook ()
  "Function called when Emacs is killed.
Save all the databases."
  (semanticdb-save-all-db))

;;; Start/Stop database use
;;
(defvar semanticdb-hooks
  '((semanticdb-semantic-init-hook-fcn semantic-init-db-hooks)
    (semanticdb-synchronize-table semantic-after-toplevel-cache-change-hook)
    (semanticdb-kill-hook kill-buffer-hook)
    (semanticdb-kill-emacs-hook kill-emacs-hook)
    )
  "List of hooks and values to add/remove when configuring semanticdb.")

;;;###autoload
(defun semanticdb-minor-mode-p ()
  "Return non-nil if `semanticdb-minor-mode' is active."
  (member (car (car semanticdb-hooks))
	  (symbol-value (car (cdr (car semanticdb-hooks))))))

;;;###autoload
(defun global-semanticdb-minor-mode (&optional arg)
  "Toggle the use of `semanticdb-minor-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (if (not arg)
      (if (semanticdb-minor-mode-p)
	  (setq arg -1)
	(setq arg 1)))
  (let ((fn 'add-hook)
	(h semanticdb-hooks))
    (if (< arg 0)
	(setq semanticdb-global-mode nil
              fn 'remove-hook)
      (setq semanticdb-global-mode t))
    ;(message "ARG = %d" arg)
    (while h
      (funcall fn (car (cdr (car h))) (car (car h)))
      (setq h (cdr h)))
    ;; Call a hook
    (run-hooks 'semanticdb-mode-hooks)
    ))

(defun semanticdb-toggle-global-mode ()
  "Toggle use of the Semantic Database feature.
Update the environment of Semantic enabled buffers accordingly."
  (interactive)
  (if (semanticdb-minor-mode-p)
      ;; Save databases before disabling semanticdb.
      (semanticdb-save-all-db))
  ;; Toggle semanticdb minor mode.
  (global-semanticdb-minor-mode))


;;; Validate the semantic database
;;
(defun semanticdb-table-oob-sanity-check (cache)
  "Validate that CACHE tags do not have any overlays in them."
  (while cache
    (when (semantic-overlay-p (semantic-tag-overlay cache))
      (message "Tag %s has an erroneous overlay!"
	       (semantic-format-tag-summarize (car cache))))
    (semanticdb-table-oob-sanity-check
     (semantic-tag-components-with-overlays (car cache)))
    (setq cache (cdr cache))))

(defun semanticdb-table-sanity-check (&optional table)
  "Validate the current semanticdb TABLE."
  (interactive)
  (if (not table) (setq table semanticdb-current-table))
  (let* ((full-filename (semanticdb-full-filename table))
	 (buff (get-file-buffer full-filename)))
    (if buff
	(save-excursion
	  (set-buffer buff)
	  (semantic-sanity-check))
      ;; We can't use the usual semantic validity check, so hack our own.
      (semanticdb-table-oob-sanity-check (semanticdb-get-tags table)))))

(defun semanticdb-database-sanity-check ()
  "Validate the current semantic database."
  (interactive)
  (let ((tables (semanticdb-get-database-tables
		 semanticdb-current-database)))
    (while tables
      (semanticdb-table-sanity-check (car tables))
      (setq tables (cdr tables)))
    ))

;;;###autoload
(defun semanticdb-file-table-object (file)
  "Return a semanticdb table belonging to FILE.
If file has database tags available in the database, return it.
If file does not have tags available, then load the file, and create a new
table object for it."
  (setq file (expand-file-name file))
  (when (file-exists-p file)
    (let* ((default-directory (file-name-directory file))
	   (db (semanticdb-get-database default-directory))
	   )
      (or (semanticdb-file-table db file)
	  ;; We must load the file.
	  (save-excursion
	    (set-buffer (find-file-noselect file))
	    ;; Find file should automatically do this for us.
	    (prog1
		semanticdb-current-table
	      ;; If we had to find the file, then we should kill it
	      ;; to keep the master buffer list clean.
	      (kill-buffer (current-buffer))
	      ))))))

;;;###autoload
(defun semanticdb-file-stream (file)
  "Return a list of tags belonging to FILE.
If file has database tags available in the database, return them.
If file does not have tags available, then load the file, and create them."
  (let ((table (semanticdb-file-table-object file)))
    (when table
      (semanticdb-get-tags table))))

(provide 'semanticdb)

;;; semanticdb.el ends here
