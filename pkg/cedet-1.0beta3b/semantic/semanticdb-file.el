;;; semanticdb-file.el --- Save a semanticdb to a cache file.

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-file.el,v 1.14 2004/07/15 20:45:43 zappo Exp $

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
;; A set of semanticdb classes for persistently saving caches on disk.
;;

(require 'semanticdb)
(require 'inversion)

(defvar semanticdb-file-version semantic-version
  "Version of semanticdb we are writing files to disk with.")
(defvar semanticdb-file-incompatible-version "1.4"
  "Version of semanticdb we are not reverse compatible with.")

;;; Settings
;;
;;;###autoload
(defcustom semanticdb-default-file-name "semantic.cache"
  "*File name of the semantic tag cache."
  :group 'semanticdb
  :type 'string)

;;;###autoload
(defcustom semanticdb-default-save-directory nil
  "*Directory name where semantic cache files are stored.
If this value is nil, files are saved in the current directory.  If the value
is a valid directory, then it overrides `semanticdb-default-file-name' and
stores caches in a coded file name in this directory."
  :group 'semanticdb
  :type '(choice :tag "Default-Directory"
                 :menu-tag "Default-Directory"
                 (const :tag "Use current directory" :value nil)
                 (directory)))

;;;###autoload
(defcustom semanticdb-persistent-path '(project)
  "*List of valid paths that semanticdb will cache tags to.
When `global-semanticdb-minor-mode' is active, tag lists will
be saved to disk when Emacs exits.  Not all directories will have
tags that should be saved.
The value should be a list of valid paths.  A path can be a string,
indicating a directory in which to save a variable.  An element in the
list can also be a symbol.  Valid symbols are `never', which will
disable any saving anywhere, `always', which enables saving
everywhere, or `project', which enables saving in any directory that
passes a list of predicates in `semanticdb-project-predicate-functions'."
  :group 'semanticdb
  :type nil)

(defcustom semanticdb-save-database-hooks nil
  "*Hooks run after a database is saved.
Each function is called with one argument, the object representing
the database recently written."
  :group 'semanticdb
  :type 'hook)

;;; Classes
;;
;;;###autoload
(defclass semanticdb-project-database-file (semanticdb-project-database
					    eieio-persistent)
  ((file-header-line :initform ";; SEMANTICDB Tags save file")
   (semantic-tag-version :initarg :semantic-tag-version
			 :initform "1.4"
			 :documentation
			 "The version of the tags saved.
The default value is 1.4.  In semantic 1.4 there was no versioning, so
when those files are loaded, this becomes the version number.
To save the version number, we must hand-set this version string.")
   (semanticdb-version :initarg :semanticdb-version
		       :initform "1.4"
		       :documentation
		       "The version of the object system saved.
The default value is 1.4.  In semantic 1.4, there was no versioning,
so when those files are loaded, this becomes the version number.
To save the version number, we must hand-set this version string.")
   )
  "Database of file tables saved to disk.")

;;; Code:
;;
(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-project-database-file)
					       directory)
  "Create a new semantic database for DIRECTORY and return it.
If a database for DIRECTORY has already been loaded, return it.
If a database for DIRECTORY exists, then load that database, and return it.
If DIRECTORY doesn't exist, create a new one."
  (let* ((fn (semanticdb-cache-filename dbc directory))
	 (db (or (semanticdb-file-loaded-p fn)
		 (if (file-exists-p fn)
		     (progn
		       (semanticdb-load-database fn))))))
    (unless db
      (setq db (make-instance
		dbc  ; Create the database requested.  Perhaps
		(concat (file-name-nondirectory
			 (directory-file-name
			  (file-name-directory fn)))
			"/")
		:file fn :tables nil
		:semantic-tag-version semantic-version
		:semanticdb-version semanticdb-file-version)))
    ;; Set this up here.   We can't put it in the constructor because it
    ;; would be saved, and we want DB files to be portable.      
    (oset db reference-directory directory)
    db))

;;; File IO
(defun semanticdb-load-database (filename)
  "Load the database FILENAME."
  (condition-case foo
      (let* ((r (eieio-persistent-read filename))
	     (c (semanticdb-get-database-tables r))
	     (tv (oref r semantic-tag-version))
	     (fv (oref r semanticdb-version))
	     )
	;; Restore the parent-db connection
	(while c
	  (oset (car c) parent-db r)
	  (setq c (cdr c)))
	(if (not (inversion-test 'semanticdb-file fv))
	    (when (inversion-test 'semantic-tag tv)
	      ;; Incompatible version.  Flush tables.
	      (semanticdb-flush-database-tables r)
	      ;; Reset the version to new version.
	      (oset r semantic-tag-version semantic-tag-version)
	      ;; Warn user
	      (message "Semanticdb file is old.  Starting over for %s"
		       filename)
	      )
	  ;; Version is not ok.  Flush whole system
	  (message "semanticdb file is old.  Starting over for %s"
		   filename)
	  ;; This database is so old, we need to replace it.
	  ;; We also need to delete it from the instance tracker.
	  (delete-instance r)
	  (setq r nil))
	r)
    (error (message "Cache Error: %s, Restart" foo)
	   nil)))

;;;###autoload
(defun semanticdb-file-loaded-p (filename)
  "Return the project belonging to FILENAME if it was already loaded."
  (eieio-instance-tracker-find filename 'file 'semanticdb-database-list))

(defmethod semanticdb-save-db ((DB semanticdb-project-database-file))
  "Write out the database DB to its file.
If DB is not specified, then use the current database."
  (let ((objname (oref DB file)))
    (when (and (semanticdb-live-p DB)
	       (semanticdb-write-directory-p DB))
      (message "Saving tag summary for %s..." objname)
      (condition-case foo
	  (eieio-persistent-save (or DB semanticdb-current-database))
	(file-error ; System error saving?  Ignore it.
	 (message "Error saving %s" objname))
	(error
	 (if (and (listp foo)
		  (stringp (nth 1 foo))
		  (string-match "write-protected" (nth 1 foo)))
	     (message (nth 1 foo))
	   (error "%S" foo))))
      (run-hook-with-args 'semanticdb-save-database-hooks
			  (or DB semanticdb-current-database))
      (message "Saving tag summary for %s...done" objname))
    ))

;;;###autoload
(defmethod semanticdb-live-p ((obj semanticdb-project-database))
  "Return non-nil if the file associated with OBJ is live.
Live databases are objects associated with existing directories."
  (and (slot-boundp obj 'reference-directory)
       (file-exists-p (oref obj reference-directory))))

(defmethod semanticdb-live-p ((obj semanticdb-table))
  "Return non-nil if the file associated with OBJ is live.
Live files are either buffers in Emacs, or files existing on the filesystem."
  (let ((full-filename (semanticdb-full-filename obj)))
    (or (get-file-buffer full-filename)
	(file-exists-p full-filename))))

(defmethod object-write ((obj semanticdb-table))
  "When writing a table, we have to make sure we deoverlay it first.
Restore the overlays after writting.
Argument OBJ is the object to write."
  (if (semanticdb-live-p obj)
      (let ((b (get-file-buffer (semanticdb-full-filename obj))))
	(save-excursion
	  (if b (progn (set-buffer b)
		       ;; Try to get an accurate unmatched syntax table.
		       (when (and (boundp semantic-show-unmatched-syntax-mode)
				  semantic-show-unmatched-syntax-mode)
			 ;; Only do this if the user runs unmatched syntax
			 ;; mode display enties.
			 (oset obj unmatched-syntax
			       (semantic-show-unmatched-lex-tokens-fetch))
			 )
		       ;; Unlink the cache.  When there are arrors,
		       ;; reset the master cache.
		       (condition-case nil
			   (semantic--tag-unlink-cache-from-buffer)
			 (error
			  (condition-case nil
			      (semantic-clear-toplevel-cache)
			    (error
			     (semantic--set-buffer-cache nil)))))
		       (oset obj pointmax (point-max)))))
	(call-next-method)
	(save-excursion
	  (if b (progn (set-buffer b) (semantic--tag-link-cache-to-buffer)))
	  (oset obj unmatched-syntax nil))
	)))

;;; State queries
;;
(defmethod semanticdb-write-directory-p ((obj semanticdb-project-database-file))
  "Return non-nil if OBJ should be written to disk.
Uses `semanticdb-persistent-path' to determine the return value."
  (let ((path semanticdb-persistent-path))
    (catch 'found
      (while path
	(cond ((stringp (car path))
	       (if (string= (oref obj reference-directory) (car path))
		   (throw 'found t)))
	      ((eq (car path) 'project)
	       (if semanticdb-project-predicate-functions
		   (if (run-hook-with-args-until-success
			'semanticdb-project-predicate-functions
			(oref obj reference-directory))
		       (throw 'found t))
		 ;; If the mode is 'project, and there are no project
		 ;; modes, then just always save the file.  If users
		 ;; wish to restrict the search, modify
		 ;; `semanticdb-persistent-path' to include desired paths.
		 (if (= (length semanticdb-persistent-path) 1)
		     (throw 'found t))
		 ))
	      ((eq (car path) 'never)
	       (throw 'found nil))
	      ((eq (car path) 'always)
	       (throw 'found t))
	      (t (error "Invalid path %S" (car path))))
	(setq path (cdr path)))
      (call-next-method))
    ))

;;; Filename manipulation
;;
(defmethod semanticdb-file-name-non-directory :STATIC
  ((dbclass semanticdb-project-database-file))
  "Return the file name DBCLASS will use.
File name excludes any directory part."
  semanticdb-default-file-name)

(defmethod semanticdb-file-name-directory :STATIC
  ((dbclass semanticdb-project-database-file) path)
  "Return the path to where DBCLASS will save its cache file related to PATH."
  (if semanticdb-default-save-directory
      (let ((file path)
	    dir-sep-string)
        (when (memq system-type '(windows-nt ms-dos))
          ;; Normalize DOSish file names: convert all slashes to
          ;; directory-sep-char, downcase the drive letter, if any,
          ;; and replace the leading "x:" with "/drive_x".
          (or (file-name-absolute-p file)
              (setq file (expand-file-name file))) ; make defaults explicit
          ;; Replace any invalid file-name characters (for the
          ;; case of backing up remote files).
          (setq file (expand-file-name (convert-standard-filename file)))
          (setq dir-sep-string (char-to-string semanticdb-dir-sep-char))
          (if (eq (aref file 1) ?:)
              (setq file (concat dir-sep-string
                                 "drive_"
                                 (char-to-string (downcase (aref file 0)))
                                 (if (eq (aref file 2) semanticdb-dir-sep-char)
                                     ""
                                   dir-sep-string)
                                 (substring file 2)))))
        ;; Make the name unique by substituting directory
        ;; separators.  It may not really be worth bothering about
        ;; doubling `!'s in the original name...
        (setq file (subst-char-in-string
                    semanticdb-dir-sep-char ?!
                    (replace-regexp-in-string "!" "!!" file)))
        ;; Now create a filename for the cache file in
        ;; ;`semanticdb-default-save-directory'.
     (expand-file-name
         (concat (file-name-as-directory semanticdb-default-save-directory)
                 file)))
    path))

(defmethod semanticdb-cache-filename :STATIC
  ((dbclass semanticdb-project-database-file) path)
  "For DBCLASS, return a file to a cache file belonging to PATH.
This could be a cache file in the current directory, or an encoded file
name in a secondary directory."
  (concat (semanticdb-file-name-directory dbclass path)
	  (semanticdb-file-name-non-directory dbclass)))

;;;###autoload
(defmethod semanticdb-full-filename ((obj semanticdb-project-database-file))
  "Fetch the full filename that OBJ refers to."
  (oref obj file))

(defmethod semanticdb-full-filename ((obj semanticdb-table))
  "Fetch the full filename that OBJ refers to."
  (concat (file-name-as-directory
	   (oref (oref obj parent-db) reference-directory))
	  (oref obj file)))

;;; Compatibility
;;
;; replace-regexp-in-string is in subr.el in Emacs 21.  Provide
;; here for compatibility.

(if (not (fboundp 'replace-regexp-in-string))

(defun replace-regexp-in-string (regexp rep string &optional
					fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\""

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacements it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

)

(provide 'semanticdb-file)

;;; semanticdb-file.el ends here
