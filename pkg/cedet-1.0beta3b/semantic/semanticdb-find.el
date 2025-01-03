;;; semanticdb-find.el --- Searching through semantic databases.

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-find.el,v 1.20 2004/07/30 17:57:58 zappo Exp $

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
;; Databases of various forms can all be searched.
;; There are a few types of searches that can be done:
;;
;;   Basic Name Search:
;;    These searches scan a database table  collection for tags based
;;    on name.
;;
;;   Basic Brute Search:
;;    These searches allow searching on specific attributes of tags,
;;    such as name, type, or other attribute.
;;
;;   Advanced Search:
;;    These are searches that were needed to accomplish some
;;    specialized tasks as discovered in utilities.  Advanced searches
;;    include matching methods defined outside some parent class.
;;
;;    The reason for advanced searches are so that external
;;    repositories such as the Emacs obarray, or java .class files can
;;    quickly answer these needed questions without dumping the entire
;;    symbol list into Emacs for a regular semanticdb search.
;;
;;   Generic Brute Search:
;;    The generic search, `semanticdb-find-nonterminal-by-function'
;;
;; How databases are decided upon is another important aspect of a
;; database search.  When it comes to searching for a name, there are
;; these types of searches:
;;
;;   Basic Search:
;;    Basic search means that tags looking for a given name start
;;    with a specific search path.  Names are sought on that path
;;    until it is empty or items on the path can no longer be found.
;;
;;   Brute Search:
;;    Brute search means that all tables in all databases in a given
;;    project are searched.  Brute searches are the search style as
;;    written for semantic version 1.x.
;;
;; How does the search path work?
;;
;;  A basic search starts with three parameters:
;;
;;     (FINDME &optional PATH FIND-FILE-MATCH)
;;
;;  FINDME is key to be searched for dependent on the type of search.
;;  PATH is an indicator of which tables are to be searched.
;;  FIND-FILE-MATCH indicates that any time a match is found, the
;;  file associated with the tag should be read into a file.
;;
;;  The PATH argument is then the most interesting argument.  It can
;;  have these values:
;;
;;    nil - Take the current buffer, and use it's include list
;;    buffer - Use that buffer's include list.
;;    filename - Use that file's include list.  If the file is not
;;        in a buffer, see of there is a semanticdb table for it.  If
;;        not, read that file into a buffer.
;;    tag - Get that tag's buffer of file file.  See above.
;;    table - Search that table, and it's include list.
;;
;; Application:
;;
;; Here are applications where different searches are needed which
;; exist as of semantic 1.4.x
;;
;; eldoc - popup help
;;   => Requires basic search using default path.  (Header files ok)
;; tag jump - jump to a named tag
;;   => Requires a brute search useing whole project. (Source files only)
;; completion - Completing symbol names in a smart way
;;   => Basic search (headers ok)
;; type analysis - finding type definitions for variables & fcns
;;   => Basic search (headers ok)
;; Class browser - organize types into some structure
;;   => Brute search, or custom navigation.

;; TODO:
;;  During a search, load any unloaded DB files based on paths in the
;;  current project.

(require 'semanticdb)

;;; Code:

;;; Path Translations
;;
;;; OVERLOAD Functions
;;
;; These routines needed to be overloaded by specific language modes.
;; They are needed for translating an INCLUDE tag into a semanticdb
;; TABLE object.
(define-overload semanticdb-find-translate-path (path brutish)
  "Translate PATH into a list of semantic tables.
Path translation involves identifying the PATH input argument
in one of the following ways:
  nil - Take the current buffer, and use it's include list
  buffer - Use that buffer's include list.
  filename - Use that file's include list.  If the file is not
      in a buffer, see of there is a semanticdb table for it.  If
      not, read that file into a buffer.
  tag - Get that tag's buffer of file file.  See above.
  table - Search that table, and it's include list.
  find result - Search the results of a previous find.

In addition, once the base path is found, there is the possibility of
each added table adding yet more tables to the path, so this routine
can return a lengthy list.

If argument BRUTISH is non-nil, then instead of using the include
list, use all tables found in the parent project of the table
identified by translating PATH.  Such searches use brute force to
scan every available table.

The return value is a list of objects of type `semanticdb-table' or
it's children.  In the case of passing in a find result, the result
is returned unchanged.

This routine uses `semanticdb-find-table-for-include' to translate
specific include tags into a semanticdb table."
  )

(defun semanticdb-find-translate-path-default (path brutish)
  "Translate PATH into a list of semantic tables.
If BRUTISH is non nil, return all tables associated with PATH.
Default action as described in `semanticdb-find-translate-path'."
  (if (semanticdb-find-results-p path)
      ;; Perform the search over these results.
      nil
    (if brutish
	(semanticdb-find-translate-path-brutish-default path)
      (semanticdb-find-translate-path-includes-default path))))

(defun semanticdb-find-translate-path-brutish-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((basedb
	 (cond ((null path) semanticdb-current-database)
	       ((semanticdb-table-p path) (oref path parent-db))
	       (t (let ((tt (semantic-something-to-tag-table path)))
		    (save-excursion
		      (set-buffer (semantic-tag-buffer (car tt)))
		      semanticdb-current-database))))))
    (apply
     #'append
     (mapcar
      (lambda (db) (semanticdb-get-database-tables db))
      ;; FIXME:
      ;; This should scan the current project directory list for all
      ;; semanticdb files, perhaps hadding proxies for them.
      (semanticdb-current-database-list (oref basedb reference-directory)))))
  )

(defun semanticdb-find-translate-path-includes-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((includetags
	 (cond ((null path)
		(semantic-find-tags-included (current-buffer)))
	       ((semanticdb-table-p path)
		(semantic-find-tags-included (semanticdb-get-tags path)))
	       (t (semantic-find-tags-included path))))
	(matchedtables (list semanticdb-current-table))
	nexttable)
    ;; Loop over all include tags adding to matchedtables
    (while includetags
      (semantic-throw-on-input 'semantic-find-translate-path-includes-default)
      (setq nexttable (semanticdb-find-table-for-include (car includetags)))
      ;; (message "Scanning %s" (semantic-tag-name (car includetags)))
      (when (and nexttable
		 (not (memq nexttable matchedtables))
		 (semanticdb-equivalent-mode nexttable (current-buffer))
		 )
	;; Add to list of tables
	(push nexttable matchedtables)
	;; Queue new includes to list
	(setq includetags (append includetags
				  (semantic-find-tags-included
				   (semanticdb-get-tags nexttable)))))
      (setq includetags (cdr includetags)))
    (nreverse matchedtables)))

;;;###autoload
(define-overload semanticdb-find-table-for-include (includetag &optional table)
  "For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE as defined by `semantic-something-to-tag-table' to identify
where the tag came from.  TABLE is optional if INCLUDETAG has an
overlay of :filename attribute."
  )

(defun semanticdb-find-table-for-include-default (includetag &optional table)
  "Default implementation of `semanticdb-find-table-for-include'.
Uses `semanticdb-current-database-list' as the search path.
INCLUDETAG and TABLE are documented in `semanticdb-find-table-for-include'."
  (if (not (eq (semantic-tag-class includetag) 'include))
      (signal 'wrong-type-argument (list includetag 'include)))

  ;; Note, some languages (like Emacs or Java) use include tag names
  ;; that don't represent files!  We want to have file names.
  (let ((name (semantic-tag-include-filename includetag))
	(roots (semanticdb-current-database-list))
	(tmp nil)
	(ans nil))
    (cond
     ;; Relative path name
     ((file-exists-p (expand-file-name name))
      (setq ans (semanticdb-file-table-object name)))

     ;; On the path somewhere
;;;; THIS NEEDS WORK!
;;;; NOTES: Separate system includes from local includes.
;;;;        Use only system databases for system includes.
     ;((setq tmp (semantic-dependency-tag-file includetag))
     ; (setq ans (semanticdb-file-table-object tmp))
      ;; If we don't load this, then finding include tags within
      ;; the table could fail!
      ;; (when ans (save-excursion (semanticdb-set-buffer ans)))
     ;;  )
     (t
      ;; Somewhere in our project hierarchy
      ;; Remember: Roots includes system databases which can create
      ;; specialized tables we can search.
      (while (and (not ans) roots)
	(let* ((ref (if (slot-boundp (car roots) 'reference-directory)
			(oref (car roots) reference-directory)))
	       (fname (cond ((null ref) nil)
			    ((file-exists-p (expand-file-name name ref))
			     (expand-file-name name ref))
			    ((file-exists-p (expand-file-name (file-name-nondirectory name) ref))
			     (expand-file-name (file-name-nondirectory name) ref)))))
	  (if ref
	      (when fname
		;; There is an actual file.  Grab it.
		(setq ans (semanticdb-file-table-object fname)))
	    ;; No reference directory  Probably a system database
	    ;; NOTE: Systemdb will need to override `semanticdb-file-table'.
	    (setq ans (semanticdb-file-table
		       (car roots)
		       ;; Use name direct from tag.  System DB will expect it
		       ;; in the original form.
		       (semantic-tag-name includetag)))))

	(setq roots (cdr roots))))
     )
    ans))


;;; FIND results and edebug
;;
(eval-after-load "cedet-edebug"
  '(progn
     (cedet-edebug-add-print-override
      '(semanticdb-find-results-p object)
      '(semanticdb-find-result-prin1-to-string object) )
     ))



;;; API Functions
;;
;; Once you have a search result, use these routines to operate
;; on the search results at a higher level
(defun semanticdb-strip-find-results (results &optional find-file-match)
  "Strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
Optional FIND-FILE-MATCH is not yet implemented."
  (apply #'append (mapcar #'cdr results)))

(defun semanticdb-find-results-p (resultp)
  "Non-nil if RESULTP is in the form of a semanticdb search result.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions."
  (and (listp resultp)
       (listp (car resultp))
       (semanticdb-abstract-table-child-p (car (car resultp)))
       (or (semantic-tag-p (car (cdr (car resultp))))
	   (null (car (cdr (car resultp)))))))

(defun semanticdb-find-result-prin1-to-string (result)
  "Presuming RESULT satisfies `semanticdb-find-results-p', provide a shirt PRIN1 output."
  (concat "#<FIND RESULT "
	  (mapconcat (lambda (a)
		       (concat "(" (object-name (car a) ) " . "
			       "#<TAG LIST " (number-to-string (length (cdr a))) ">)"))
		     result
		     " ")
	  ">"))

(defun semanticdb-find-result-with-nil-p (resultp)
  "Non-nil of RESULTP is in the form of a semanticdb search result.
nil is a valid value where a TABLE usually is, but only if the TAG
results include overlays.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions."
  (and (listp resultp)
       (listp (car resultp))
       (let ((tag-to-test (car-safe (cdr (car resultp)))))
	 (or (and (semanticdb-abstract-table-child-p (car (car resultp)))
		  (or (semantic-tag-p tag-to-test)
		      (null tag-to-test)))
	     (and (null (car (car resultp)))
		  (or (semantic-tag-with-position-p tag-to-test)
		      (null tag-to-test))))
	 )))

(defun semanticdb-find-result-length (result)
  "Number of tags found in RESULT."
  (let ((count 0))
    (mapc (lambda (onetable)
	    (setq count (+ count (1- (length onetable)))))
	  result)
    count))

(defun semanticdb-find-result-nth (result n)
  "In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0.

The returned value is a cons cell: (TAG . TABLE) where TAG
is the tag at the Nth position.  TABLE is the semanticdb table where
the TAG was found.  Sometimes TABLE can be nil."
  (let ((ans nil)
	(anstable nil))
    ;; Loop over each single table hit.
    (while (and (not ans) result)
      ;; For each table result, get local length, and modify
      ;; N to be that much less.
      (let ((ll (length (cdr (car result))))) ;; local length
	(if (> ll n)
	    ;; We have a local match.
	    (setq ans (nth n (cdr (car result)))
		  anstable (car (car result)))
	  ;; More to go.  Decrement N.
	  (setq n (- n ll))))
      ;; Keep moving.
      (setq result (cdr result)))
    (cons ans anstable)))

(defun semanticdb-find-result-test (result)
  "Test RESULT by accessing all the tags in the list."
  (if (not (semanticdb-find-results-p result))
      (error "Does not pass `semanticdb-find-results-p.\n"))
  (let ((len (semanticdb-find-result-length result))
	(i 0))
    (while (< i len)
      (let ((tag (semanticdb-find-result-nth result i)))
	(if (not (semantic-tag-p (car tag)))
	    (error "%d entry is not a tag." i)))
      (setq i (1+ i)))))

(defun semanticdb-find-result-nth-in-buffer (result n)
  "In RESULT, return the Nth search result.
Like `semanticdb-find-result-nth', except that only the TAG
is returned, and the buffer it is found it will be made current.
If the result tag has no position information, the originating buffer
is still made current."
  (let* ((ret (semanticdb-find-result-nth result n))
	 (ans (car ret))
	 (anstable (cdr ret)))
    ;; If we have a hit, double-check the find-file
    ;; entry.  If the file must be loaded, then gat that table's
    ;; source file into a buffer.
    (if anstable (semanticdb-set-buffer anstable))
    ;; Return the tag.
    ans))

;;; Search Logging
;;
;; Basic logging to see what the search routines are doing.
(defvar semanticdb-find-log-flag nil
  "Non-nil means log the process of searches.")

(defvar semanticdb-find-log-buffer-name "*SemanticDB Find Log*"
  "The name of the logging buffer.")

(defun semanticdb-find-toggle-logging ()
  "Toggle sematnicdb logging."
  (interactive)
  (setq semanticdb-find-log-flag (null semanticdb-find-log-flag))
  (message "Semanticdb find logging is %sabled"
	   (if semanticdb-find-log-flag "en" "dis")))

(defun semanticdb-reset-log ()
  "Reset the log buffer."
  (interactive)
  (when semanticdb-find-log-flag
    (save-excursion
      (set-buffer (get-buffer-create semanticdb-find-log-buffer-name))
      (erase-buffer)
      )))

(defun semanticdb-find-log-move-to-end ()
  "Move to the end of the semantic log."
  (let ((cb (current-buffer))
	(cw (selected-window)))
    (unwind-protect
	(progn
	  (set-buffer semanticdb-find-log-buffer-name)
	  (if (get-buffer-window (current-buffer) 'visible)
	      (select-window (get-buffer-window (current-buffer) 'visible)))
	  (goto-char (point-max)))
      (if cw (select-window cw))
      (set-buffer cb))))

(defun semanticdb-find-log-new-search (forwhat)
  "Start a new search FORWHAT."
  (when semanticdb-find-log-flag
    (save-excursion
      (set-buffer (get-buffer-create semanticdb-find-log-buffer-name))
      (insert (format "New Search: %S\n" forwhat))
      )
    (semanticdb-find-log-move-to-end)))

(defun semanticdb-find-log-activity (table result)
  "Log that TABLE has been searched and RESULT was found."
  (when semanticdb-find-log-flag
    (save-excursion
      (set-buffer semanticdb-find-log-buffer-name)
      (insert "Table: " (object-print table)
	      " Result: " (int-to-string (length result)) " tags"
	      "\n")
      )
    (semanticdb-find-log-move-to-end)))

;;; Semanticdb find API functions
;;
;; These are the routines actually used to perform searches.
;;
;;;###autoload
(defun semanticdb-find-tags-collector (function &optional path find-file-match
						brutish)
  "Search for all tags returned by FUNCTION over PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.
If optional argument BRUTISH is non-nil, then ignore include statements,
and search all tables in this project tree."
  (let (found match)
    (save-excursion
      ;; If path is a buffer, set ourselves up in that buffer
      ;; so that the override methods work correctly.
      (when (bufferp path) (set-buffer path))
      (if (semanticdb-find-results-p path)
	  ;; When we get find results, loop over that.
	  (dolist (tableandtags path)
	    (semantic-throw-on-input 'semantic-find-translate-path)
	    ;; If FIND-FILE-MATCH is non-nil, skip tables of class
	    ;; `semanticdb-search-results-table', since those are system
	    ;; databases and not associated with a file.
	    (unless (and find-file-match
			 (obj-of-class-p
			  (car tableandtags) semanticdb-search-results-table))
	      (when (setq match (funcall function
					 (car tableandtags) (cdr tableandtags)))
		(when find-file-match
		  (save-excursion (semanticdb-set-buffer (car tableandtags))))
		(push (cons (car tableandtags) match) found)))
    	    )
	;; Only log searches across data bases.
	(semanticdb-find-log-new-search nil)
	;; If we get something else, scan the list of tables resulting
	;; from translating it into a list of objects.
	(dolist (table (semanticdb-find-translate-path path brutish))
	  (semantic-throw-on-input 'semantic-find-translate-path)
	  ;; If FIND-FILE-MATCH is non-nil, skip tables of class
	  ;; `semanticdb-search-results-table', since those are system
	  ;; databases and not associated with a file.
	  (unless (and find-file-match
		       (obj-of-class-p table semanticdb-search-results-table))
	    (when (and table (setq match (funcall function table nil)))
	      (semanticdb-find-log-activity table match)
	      (when find-file-match
		(save-excursion (semanticdb-set-buffer table)))
	      (push (cons table match) found))))))
    found))

;;;###autoload
(defun semanticdb-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-name-method table name tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-name-regexp-method table regexp tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-for-completion-method table prefix tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-class (class &optional path find-file-match)
  "Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-class-method table class tags))
   path find-file-match))

;;; Deep Searches
;;
;;;###autoload
(defun semanticdb-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-method table name tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-deep-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-regexp-method table regexp tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-deep-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-for-completion-method table prefix tags))
   path find-file-match))

;;; Brutish Search Routines
;;
;;;###autoload
(defun semanticdb-brute-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-method table name tags))
   path find-file-match t))

(defun semanticdb-brute-deep-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-for-completion-method table prefix tags))
   path find-file-match t))

;;;###autoload
(defun semanticdb-brute-find-tags-by-class (class &optional path find-file-match)
  "Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-class-method table class tags))
   path find-file-match t))

;;; Specialty Search Routines
;;
;;;###autoload
(defun semanticdb-find-tags-external-children-of-type
  (type &optional path find-file-match)
  "Search for all tags defined outside of TYPE w/ TYPE as a parent.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-external-children-of-type-method table type tags))
   path find-file-match))

;;; METHODS
;;
;; Default methods for semanticdb database and table objects.
;; Override these with system databases to as new types of back ends.

;;; Top level Searches
(defmethod semanticdb-find-tags-by-name-method ((table semanticdb-table) name &optional tags)
  "In TABLE, find all occurances of tags with NAME.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-name name (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-by-name-regexp-method ((table semanticdb-table) regexp &optional tags)
  "In TABLE, find all occurances of tags matching REGEXP.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-name-regexp regexp (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-table) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-for-completion prefix (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-by-class-method ((table semanticdb-table) class &optional tags)
  "In TABLE, find all occurances of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-class class (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-external-children-of-type-method ((table semanticdb-table) parent &optional tags)
   "In TABLE, find all occurances of tags whose TYPE is PARENT.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
   (semantic-find-tags-external-children-of-type parent (or tags (semanticdb-get-tags table))))

;;; Deep Searches
(defmethod semanticdb-deep-find-tags-by-name-method ((table semanticdb-table) name &optional tags)
  "In TABLE, find all occurances of tags with NAME.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name name (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method ((table semanticdb-table) regexp &optional tags)
  "In TABLE, find all occurances of tags matching REGEXP.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name-regexp regexp (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(defmethod semanticdb-deep-find-tags-for-completion-method ((table semanticdb-table) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-for-completion prefix (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(provide 'semanticdb-find)

;;; semanticdb-find.el ends here
