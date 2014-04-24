;;; fff.el --- file-finding commands and utilities

;; Copyright (C) 1996 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Status: Works in Emacs 19; believed to work in Emacs 18 and XEmacs
;; Keywords: extensions, searching, files, commands, tools
;; Created: 1996-03-26

;; LCD Archive Entry:
;; fff|Noah Friedman|friedman@prep.ai.mit.edu|
;; file-finding commands and utilities|
;; $Date: 1996/06/02 01:04:51 $|$Revision: 1.5 $|~/misc/fff.el.gz|

;; $Id: fff.el,v 1.5 1996/06/02 01:04:51 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides several shortcut commands for visiting or
;; inserting files without having to specify them by their complete name.
;; For example, you can visit emacs lisp libraries, programs in your
;; exec-path (some of which are humanly-readable shell scripts or config
;; files), or anything else which is quickly locatable via a prebuilt
;; database or path list.

;; Using a prefix arg will display a menu of all possible matches,
;; and you can select one of them by moving point and pressing return.

;; There are also documented subroutines for common searching operations
;; and basic interactive behavior to minimize any effort required to add
;; additional shortcut commands.

;; To use this package, put the following in your .emacs:
;;
;;     (autoload 'fff-find-emacs-lisp-library         "fff" nil t)
;;     (autoload 'fff-insert-emacs-lisp-library       "fff" nil t)
;;     (autoload 'fff-locate-emacs-lisp-library       "fff" nil t)
;;     (autoload 'fff-find-loaded-emacs-lisp-function "fff" nil t)
;;     (autoload 'fff-find-file-in-envvar-path        "fff" nil t)
;;     (autoload 'fff-insert-file-in-envvar-path      "fff" nil t)
;;     (autoload 'fff-find-file-in-exec-path          "fff" nil t)
;;     (autoload 'fff-insert-file-in-exec-path        "fff" nil t)
;;     (autoload 'fff-find-file-in-path               "fff" nil t)
;;     (autoload 'fff-insert-file-in-path             "fff" nil t)
;;     (autoload 'fff-find-file-in-locate-db          "fff" nil t)
;;     (autoload 'fff-insert-file-in-locate-db        "fff" nil t)
;;
;; The command `fff-install-map' will bind these commands to a common
;; prefix of "C-c C-f" (you can change this).  To find a list of them, run
;; that command and then type "C-c C-f C-h".
;;
;; If you are using Emacs 19, you can have this happen automatically by
;; putting the following in your .emacs:
;;
;;     (eval-after-load "fff" '(fff-install-map))

;; I wrote this package I got sick of typing long file names for things way
;; down in source trees, or having to go look for them first because I
;; didn't even know where they were.  This package is called fff.el because
;; originally it stood for "friedman-find-file" for lack of a better name.

;; It's most generally useful if you have an up-to-date `locate' database
;; (built by the `updatedb' program from the GNU findutils), since that
;; provides a fast way to search for files by name all over the system.

;;; Code:


(defvar fff-map-prefix "\C-c\C-f"
  "*Default prefix on which keybindings will go.
If you change this at runtime, you will need to re-invoke `fff-install-map'.")

(defvar fff-locate-program "locate"
  "*Name of program to invoke which reads the `locate' database.
This variable is used by the function `fff-locate-files-in-locate-db'.")

(defvar fff-locate-program-args nil
  "*Additional args to the program which searches the `locate' database.
This variable is used by the function `fff-locate-files-in-locate-db'.")

(defvar fff-match-predicate 'fff-file-nondirectory-p
  "*Default matching predicate for commands in this package.
If `nil', no predicate is used; all files match.

This variable only used by interactive commands defined in this package.

Utilitity functions in this package which take a predicate argument do not
refer to this variable for a default; if no predicate is specified, none is
used and all files match.")

(defvar fff-sorting-predicate nil
  "*Predicate used to sort file names in display menu.
If `nil', no predicate is used; files are presented in the order listed.
This is used by `fff-display-matches'.")

(defvar fff-map nil
  "*Keymap for FFF commands.
Type ``\\[fff-command-prefix] \\<fff-map>\\[describe-prefix-bindings]'' \
for a list of bindings.")

;; I haven't implemented menu bar support for XEmacs yet.
(defconst fff-menu-bar-support-p
  (and (string-lessp "19" emacs-version)
       (not (string-match "XEmacs\\|Lucid" (emacs-version)))))

(defconst fff-emacs-variant
  (let ((data (match-data))
        (version (cond
                  ((fboundp 'nemacs-version)
                   (nemacs-version))
                  (t
                   (emacs-version))))
        (alist '(("\\bXEmacs\\b" . xemacs)
                 ("\\bLucid\\b"  . lucid)
                 ("^Nemacs\\b"   . nemacs)
                 ("^GNU Emacs"   . gnu)))
        result)
    (while alist
      (cond
       ((string-match (car (car alist)) version)
        (setq result (cdr (car alist)))
        (setq alist nil))
       (t
        (setq alist (cdr alist)))))
    (store-match-data data)
    result)
  "A symbol indicating emacs variant.
This can be one of `gnu', `xemacs', `lucid', `epoch', `mule', etc.")


;; (consp (cdr foo)) is faster than (length foo) for merely
;; determining if the length of a list is > 1.
(defmacro fff-length1-p (l)
  (list 'and
        (list 'consp l)
        (list 'not (list 'consp (list 'cdr l)))))

;; Emacs 18 doesn't have `member'.
(defmacro fff-member (&rest args)
  (if (fboundp 'member)
      (cons 'member args)
    (cons 'fff-member-fn args)))

;; Some versions of emacs don't have ` and #' reader syntaxes.
;;
;; (defmacro fff-suffix (str suffix-list)
;;   (let ((ext (make-symbol "ext")))
;;     `(mapcar #'(lambda (,ext)
;;                  (concat ,str ,ext))
;;              ,suffix-list)))
;;
(defmacro fff-suffix (str suffix-list)
  (let ((ext (make-symbol "ext")))
    (list 'mapcar (list 'function (list 'lambda (list ext)
                        (list 'concat str ext)))
          suffix-list)))


;;; Commands/functions for locating emacs lisp libraries.

;;;###autoload
(defun fff-find-emacs-lisp-library (lib &optional allp)
  "Visit the first emacs lisp source file named LIB.
The variable `load-path' is searched for candidates.

\(Emacs 19 only\) If no matches are found in load-path but a lisp file was
loaded by that name previously, then visit that file instead.

If called interactively with a prefix argument and there is more than one
possible match, a list is displayed.  If called from a program and there is
more than one match, an error is signalled.

If no matches are found, an error is signalled.

If called interactively, you may attempt to complete a name in the
minibuffer if that library has been loaded and registered with the package
system.  Not all libraries do this, or they may register themselves under
multiple names; in the latter case, a successfully-completed name may \(in
Emacs 19\) or may not \(any other version\) have a library name associated
with it."
  (interactive (list (completing-read "Find library (fff emacs-lisp): "
                                      (fff-symbol-list->obarray features))
                     current-prefix-arg))
  (fff-<op>-emacs-lisp-library lib allp fff-match-predicate
                               'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-emacs-lisp-library (lib &optional allp)
  "Insert the emacs lisp source file named LIB in the current buffer.
This function behaves exactly like `fff-find-emacs-lisp-library', except
that the contents of the library file is inserted in the current buffer
instead of being visited in another buffer."
  (interactive (list (completing-read "Insert library (fff emacs-lisp): "
                                      (fff-symbol-list->obarray features))
                     current-prefix-arg))
  (fff-<op>-emacs-lisp-library lib allp fff-match-predicate
                               'insert-file (interactive-p)))

(defun fff-<op>-emacs-lisp-library (lib &optional allp pred op interactivep)
  (let ((file (fff-locate-emacs-lisp-library lib allp pred '(".el" ""))))
    (cond ((fff-length1-p file)
           (message "%s" (car file))
           (funcall op (car file)))
          ((null file)
           (setq file (fff-locate-loaded-emacs-lisp-library lib))
           (cond ((stringp file)
                  (funcall op file)
                  (message "Library %s not found in load-path; %s"
                           lib "but found in load-history."))
                 (t
                  (signal 'file-error
                          (list (format "Library %s not found in load-path"
                                        lib))))))
          (t
           (if interactivep
               (fff-display-matches lib file op)
             (signal 'file-error
                     (list (format "Multiple instances of %s in load-path" lib)
                           file)))))))

;;;###autoload
(defun fff-locate-emacs-lisp-library (lib &optional allp pred suffixes)
  "Return a list of all files named LIB in the Emacs Lisp load-path.
If called interactively, display the name of the first file found.  When
calling from a program, this is the same as setting the second argument
ALLP `nil'.

If called interactively with a prfix argument, display the names of those
files in a temporary buffer.

Optional third argument PREDICATE can be an arbitrary function of one
argument \(e.g. 'file-readable-p\), which should return non-`nil' if a file
name candidate should be returned.

If called from a program, the optional fourth argument SUFFIXES may
provide a list of suffixes to try before trying the literal LIB name,
e.g. '\(\".elc\" \".el\" \"\"\).  If not provided, no suffixes are tried."
  (interactive (list (completing-read "Locate library (fff emacs-lisp): "
                                      (fff-symbol-list->obarray features)
                                      nil nil nil)
                     current-prefix-arg
                     nil
                     '("" ".el" ".elc")))
  (let* ((names (if suffixes
                    (fff-suffix lib suffixes)
                  (list lib)))
         (matches (fff-files-in-directory-list names load-path
                                               (not allp) pred)))
    (and (interactive-p)
         (cond ((null matches)
                (message "%s not found in load-path" lib))
               ((and (fff-length1-p matches)
                     (> (window-width (minibuffer-window))
                        (length (car matches))))
                (message "%s" (car matches)))
               (t
                (fff-display-matches lib matches))))
    matches))

;;;###autoload
(defun fff-find-loaded-emacs-lisp-function (fnsym)
  "Visit the file which contains the loaded definition of FUNCTION.
If the definition was loaded from a byte-compiled file, an attempt is made
to locate the corresponding source file.  If that cannot be found, the
byte-compiled file is visited anyway.

Point is positioned at the beginning of the definition if it can be
located.

This command only works in those versions of emacs which have the
`load-history' variable."
  (interactive "aFind function (fff emacs-lisp): ")

  (and (subrp (symbol-function fnsym))
       (error "%s is a primitive function" fnsym))

  (let* ((data (fff-load-history-elt-by 'symbol fnsym))
         (name (fff-load-history-file-name data)))
    (and name
         (setq name (car (fff-locate-emacs-lisp-library name t))))
    (cond (name
           (let ((srcname (fff-emacs-lisp-bytecode-source-file-name name)))
             (if (and srcname
                      (file-exists-p srcname))
                 (find-file srcname)
               (find-file name)))
           (save-match-data
             (let ((p (point))
                   (re (format "^(\\bdef\\w+\\b\\s-+\\b%s\\s-" fnsym))
                   (syntable (syntax-table)))
               (set-syntax-table emacs-lisp-mode-syntax-table)
               (if (prog1
                       (re-search-forward re nil t)
                     (set-syntax-table syntable))
                   (beginning-of-line)
                 (goto-char p)
                 (error "Cannot find definition of %s" fnsym)))))
          (t
           (error "%s not defined in any currently-loaded file" fnsym)))))

;; If a library cannot be found directly in the load-path, try searching
;; for it in the list of libraries which have already been loaded.
;; `library' can be a string or a symbol; if the latter, it should be the
;; name of a feature which is known to be provided.
;; If the absolute pathname of the library cannot be found, or if the
;; file no longer seems to exists, return nil.
;;
;; This function only works in Emacs 19 (19.7 or greater), which has the
;; `load-history' variable.
(defun fff-locate-loaded-emacs-lisp-library (lib)
  (cond ((and (boundp 'load-history)
              load-history)
         (let (data)
           (and (symbolp lib)
                (featurep lib)
                (setq data (fff-load-history-elt-by 'feature lib)))
           (cond ((null data)
                  (and (symbolp lib)
                       (setq lib (symbol-name lib)))
                  (setq data (fff-load-history-elt-by 'name lib))))
           (and data
                (fff-load-history-file-name data))))))

(defun fff-load-history-elt-by (method key)
  (let ((found nil)
        (hist load-history)
        (cell (cons 'provide key)))
    (while hist
      (if (cond ((eq method 'feature)
                 (member cell (car hist)))
                ((eq method 'symbol)
                 (memq key (car hist)))
                ((eq method 'name)
                 (let ((elt (car (car hist))))
                   (or (string= key elt)
                       (string= key (setq elt (file-name-nondirectory elt)))
                       (string= key (file-name-sans-extension elt))))))
          (setq found (car hist)
                hist nil)
        (setq hist (cdr hist))))
    found))

(defun fff-load-history-file-name (data)
  (and data
       (let* ((dir (file-name-directory (car data)))
              (name (file-name-nondirectory (car data)))
              (names (fff-suffix name '("" ".el" ".elc"))))
         (cond ((null dir)
                (car (fff-files-in-directory-list names load-path t)))
               ((file-exists-p name)
                name)
               (t
                (car (fff-files-in-directory-list names (list dir) t)))))))

;; Return the name of the lisp file from which a bytecoded file was generated.
;; The returned name doesn't necessarily exist; it is extracted from the
;; bytecode file comments.
;; If no name can be found, return nil.
(defun fff-emacs-lisp-bytecode-source-file-name (elcfile)
  (let ((buf (generate-new-buffer " *emacs lisp bytecode*"))
        (magic ";ELC")
        (source-name nil)
        (size 1024)
        data)
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          (buffer-disable-undo buf)
          (emacs-lisp-mode)
          (setq data (fff-insert-file-contents-next-region elcfile size))
          (cond ((< data (length magic)))
                ((string= (buffer-substring 1 (1+ (length magic))) magic)
                 (let ((case-fold-search t)
                       (re "^;+\\s-+from\\s-+file\\s-+\\(.*\\)\n"))
                   (while (and (> data 0)
                               (null source-name))
                     (beginning-of-line)
                     (if (re-search-forward re nil t)
                         (setq source-name
                               (buffer-substring (match-beginning 1)
                                                 (match-end 1)))
                       (setq data (fff-insert-file-contents-next-region
                                   elcfile size))))))))
      (kill-buffer buf))
    source-name))

(defun fff-insert-file-contents-next-region (file size)
  (let* ((point (point))
         (beg (buffer-size))
         (end (+ beg size))
         (inserted 0))
    (goto-char (point-max))
    (setq inserted (nth 1 (insert-file-contents file nil beg end)))
    (goto-char point)
    inserted))

;;; Finding files in lisp-based paths other than load-path.

;; This might seem like a silly command, but usually there are a lot of
;; shell scripts and config files in exec-path that are humanly readable.
;; Besides, some people actually edit binaries.
;;;###autoload
(defun fff-find-file-in-exec-path (file &optional allp)
  "Visit the first file named FILE in `exec-path'.

If called interactively with a prefix argument and there is more than one
possible match, a list is displayed.  If called from a program and there is
more than one match, an error is signalled.

If no matches are found, an error is signalled."
  (interactive (list (read-string "Find file (fff exec-path): ")
                     current-prefix-arg))
  (fff-<op>-file-in-path file 'exec-path allp fff-match-predicate
                         'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-exec-path (file &optional allp)
  "Insert the file named FILE found in `exec-path' into current buffer.
This function behaves exactly like `fff-find-file-in-exec-path', except
that the contents of the file is inserted in the current buffer instead of
being visited in another buffer."
  (interactive (list (read-string "Insert file (fff exec-path): ")
                     current-prefix-arg))
  (fff-<op>-file-in-path file 'exec-path allp fff-match-predicate
                         'insert-file (interactive-p)))

;;;###autoload
(defun fff-find-file-in-envvar-path (file envvar &optional allp)
  "Visit the file named FILE in path specified by ENVIRONMENT variable.

If called interactively with a prefix argument and there is more than one
possible match, a list is displayed.  If called from a program and there is
more than one match, an error is signalled.

If no matches are found, an error is signalled."
  (interactive (list (read-string "Find file (fff envvar): ")
                     (completing-read "In path (env var): " (fff-env->obarray))
                     current-prefix-arg))
  (fff-<op>-file-in-path file (getenv envvar) allp fff-match-predicate
                         'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-envvar-path (file envvar &optional allp)
  "Insert the file named FILE found in ENVVAR path into current buffer.
This function behaves exactly like `fff-find-file-in-envvar-path', except
that the contents of the file is inserted in the current buffer instead of
being visited in another buffer."
  (interactive (list (read-string "Insert file (fff envvar): ")
                     (completing-read "In path (env var): " (fff-env->obarray))
                     current-prefix-arg))
  (fff-<op>-file-in-path file (getenv envvar) allp fff-match-predicate
                         'insert-file (interactive-p)))

;;;###autoload
(defun fff-find-file-in-path (file path &optional allp)
  "Visit the file named FILE in PATH.
PATH may be a list of directory names,
 a string consisting of colon-separated directory names,
 or a symbol name whose value is one of the above.

If called interactively with a prefix argument and there is more than one
possible match, a list is displayed.  If called from a program and there is
more than one match, an error is signalled.

If no matches are found, an error is signalled."
  (interactive (list (read-string "Find file (fff path): ")
                     (fff-read-eval-sexp "In path (sexp): ")
                     current-prefix-arg))
  (fff-<op>-file-in-path file path allp fff-match-predicate
                         'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-path (file path &optional allp)
  "Insert the file named FILE found in PATH into current buffer.
This function behaves exactly like `fff-find-file-in-path', except that the
contents of the file is inserted in the current buffer instead of being
visited in another buffer."
  (interactive (list (read-string "Insert file (fff path): ")
                     (fff-read-eval-sexp "In path (sexp): ")
                     current-prefix-arg))
  (fff-<op>-file-in-path file path allp fff-match-predicate
                         'insert-file (interactive-p)))

;;; Generic backend for finding files in a path.
;;; You can use this to implement your own commands. e.g. a command to find
;;; files located in directories specified in the TEXINPUTS environment
;;; variable.
(defun fff-<op>-file-in-path (file path allp pred op interactivep)
  (let* ((realpath (cond ((symbolp path)
                         (symbol-value path))
                        (t path)))
         (matches (fff-files-in-directory-list file realpath (not allp) pred)))
    (cond ((fff-length1-p matches)
           (message "%s" (car matches))
           (funcall op (car matches)))
          ((null matches)
           (signal 'file-error
                   (list (format "File %s not found%s" file
                                 (if (symbolp path)
                                     (format " in %s" path)
                                   "")))))
          (t
           (if interactivep
               (fff-display-matches file matches op)
             (signal 'file-error
                     (list (format "Multiple instances of %s" file)
                           (cons 'path: path)
                           (cons 'predicate: pred)
                           (cons 'matches: matches))))))))


;;; Commands/functions for locating files via `locate' database.

;;;###autoload
(defun fff-find-file-in-locate-db (file &optional allp)
  "Visit the file named FILE in a buffer.
The complete file name is searched for in an external `locate' database.
FILE must be a literal filename; no regexps are allowed.

If called interactively with a prefix argument and there is more than one
possible match, a list is displayed.  If called from a program and there is
more than one match, an error is signalled.

If no matches are found, an error is signalled."
  (interactive (list (read-string "Find file (fff locate): ")
                     current-prefix-arg))
  (funcall 'fff-<op>-file-in-locate-db
           file allp fff-match-predicate 'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-locate-db (file &optional allp)
  "Insert the file named FILE into current buffer.
This function behaves exactly like `fff-find-file-in-locate-db', except
that the contents of the file is inserted in the current buffer instead of
being visited in another buffer."
  (interactive (list (read-string "Insert file (fff locate): ")
                     current-prefix-arg))
  (funcall 'fff-<op>-file-in-locate-db
           file allp fff-match-predicate 'insert-file (interactive-p)))

(defun fff-<op>-file-in-locate-db (file allp pred op interactivep)
  (and interactivep
       (message "Searching for %s with `locate'..." file))
  (let ((matches (fff-locate-files-in-locate-db file (not allp) pred)))
    (cond ((fff-length1-p matches)
           (message "%s" (car matches))
           (funcall op (car matches)))
          ((null matches)
           (signal 'file-error
                   (list (format "No matches for %s in locate database"
                                 file)
                         (cons 'predicate pred))))
          (t
           (if interactivep
               (fff-display-matches file matches op)
             (signal 'file-error
                     (list (format "Multiple matches for %s" file)
                           (cons 'predicate pred)
                           (list 'matches: matches))))))))

(defun fff-locate-files-in-locate-db (file &optional firstp pred)
  "Return a list of files named FILE meeting PRED in a `locate' database.
FILE must be a literal filename; no regexps are allowed.
Optional PRED may be any lisp function that takes one argument, a
  string representing the name of a file.
  It should return true if the file name should be included in the list of
  return values.  One common useful predicate is 'file-readable-p .
  If no predicate is specified, all files names named FILE are matched.

Return a list of the names found, in the order they appeared in the
database, or `nil' if none.
Optional third arg FIRSTP means return only the first match found.

The `locate' database must be kept reasonably up-to-date or this function
cannot be expected to find all existing occurences of a file.  On systems
where it is installed, it is usually run once a day via a cron job.

The database is not read directly.  The program specified by the variable
``fff-locate-program'' is used to parse the database and print a list of
file names, one per line, on standard output.

Additional arguments can be specified in the variable named
``fff-locate-program-args'', which are passed to the locate
program before the name of the file."
  (let* ((re-file (format "/%s$" (regexp-quote file)))
         (found nil)
         (args (if fff-locate-program-args
                   (append (copy-sequence fff-locate-files-program-args)
                           file)
                 (list file)))
         (buf (generate-new-buffer (concat " *locate-" file "*")))
         beg end candidate)
    (save-excursion
      (set-buffer buf)
      (fundamental-mode)
      (buffer-disable-undo (current-buffer))
      (apply 'call-process fff-locate-program nil t nil args)
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward re-file nil t)
          (beginning-of-line)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (setq candidate (buffer-substring beg end))
          (cond ((or (null pred)
                     (funcall pred candidate))
                 (setq found (cons candidate found))
                 (and firstp
                      (goto-char (point-max))))))))
    (kill-buffer buf)
    found))


;;; Display mode functions

(put 'fff-display-matches-mode 'mode-class 'special)

(defvar fff-display-matches-buffer-name "*File Name Matches*")
(defvar fff-display-matches-mode-map)

(defvar fff-display-matches-mode-selection-data)

(defun fff-display-matches (file matches &optional action buffer descrip)
  (and fff-sorting-predicate
       (setq matches (sort matches fff-sorting-predicate)))
  (let* ((buf (fff-display-matches-prepare-buffer))
         (orig-buf (current-buffer))
         (display-buf (or buffer orig-buf))
         (startpos 0)
         (l matches))
    (unwind-protect
        (progn
          (set-buffer buf)
          (goto-char (point-min))
          (cond (action
                 (insert "In this buffer, type RET to select "
                         "the match near point.\n")
                 (cond (descrip
                        (insert descrip "\n"))
                       ((and (symbolp action)
                             (commandp action))
                        (insert "That selection will invoke the command `"
                                (symbol-name action)
                                "' on it.\n"))
                       (t
                        (insert "That selection will invoke the function "
                                "specified by the value of the variable "
                                "`fff-display-matches-mode-selection-action'"
                                ".\n")))
                 (insert "\n")))
          (insert "Files found matching \"" file "\":\n\n")
          (setq startpos (point))
          (while l
            (insert (car l)
                    (if (file-directory-p (car l))
                        "/\n"
                      "\n"))
            (setq l (cdr l)))
          ;; If the order or number of these args are changed, update
          ;; fff-display-matches-select-match as well.
          (fff-display-matches-mode action
                                    display-buf
                                    (buffer-name display-buf)
                                    (set-marker (make-marker) startpos))
      (set-buffer orig-buf)))
    (fff-display-buffer buf nil startpos t)
    (message "Multiple matches for %s" file)))

(defun fff-display-matches-prepare-buffer ()
  (let ((buf (get-buffer-create fff-display-matches-buffer-name)))
    (save-excursion
      (set-buffer buf)
      (widen)
      (fundamental-mode)
      (buffer-disable-undo (current-buffer))
      (setq buffer-read-only nil)
      (erase-buffer))
    buf))

(defun fff-display-matches-mode (&rest data)
  "Major mode for buffers showing lists of possible matches for fff commands.
Type RET in the list to select the match near point."
  (widen)
  (fundamental-mode)
  (kill-all-local-variables)
  (cond ((and (boundp 'fff-display-matches-mode-map)
              (keymapp fff-display-matches-mode-map)))
        (t
         (let ((map (make-sparse-keymap))
               (fn 'fff-display-matches-select-match)
               (keys '("\n" "\r")))
           (while keys
             (define-key map (car keys) fn)
             (setq keys (cdr keys)))
           (setq fff-display-matches-mode-map map))))
  (use-local-map fff-display-matches-mode-map)

  (make-local-variable 'fff-display-matches-mode-selection-data)
  (setq fff-display-matches-mode-selection-data data)

  (buffer-disable-undo (current-buffer))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (setq major-mode 'fff-display-matches-mode)
  (setq mode-name "FFF Display Matches"))

(defun fff-display-matches-select-match ()
  (interactive)
  (or (eq major-mode 'fff-display-matches-mode)
      (error "This command is inappropriate for this mode."))
  (let* ((data fff-display-matches-mode-selection-data)
         (fn      (nth 0 data))
         (buf     (nth 1 data))
         (bufname (nth 2 data))
         (pos     (nth 3 data))
         beg name)
    (cond (fn
           (and (< (point) pos)
                (error "Point is not positioned on a file name."))
           (save-excursion
             (beginning-of-line)
             (setq beg (point))
             (end-of-line)
             (setq name (buffer-substring beg (point))))
           (and (= (length name) 0)
                (error "Point is not positioned on a file name."))
           (and buf
                ;; Check that buffer still exists.
                ;; Only a killed buffer can have a null buffer name.
                (cond ((or (buffer-name buf)
                           (fff-display-matches-use-current-buffer-p bufname
                                                                     fn
                                                                     pos))
                       (fff-display-buffer buf nil nil t)
                       (funcall fn name))
                      (t
                       (error "Original buffer \"%s\" killed." bufname))))))))

(defun fff-display-matches-use-current-buffer-p (bufname op pos)
  (cond ((not (boundp 'fff-use-current-buffer-first-call-p))
         (make-local-variable 'fff-use-current-buffer-first-call-p)
         (setq fff-use-current-buffer-first-call-p t)
         (let ((p (point-marker)))
           (goto-char pos)
           (forward-line -2)
           (let ((buffer-read-only nil))
             (insert-before-markers "*** Note: original buffer \""
                                    bufname
                                    "\" no longer exists!\n\n"))
           (goto-char p))))
  (yes-or-no-p (format "Perform %s from current buffer? " op)))

;; When you modify a buffer and want to reset point, but the buffer is
;; already being displayed by a window, you can't actually change point
;; in that window unless you select it first.
(defun fff-display-buffer (buffer &optional not-this-window-p point selectp)
  (let ((old-win (selected-window))
        (old-buf (current-buffer))
        (win (display-buffer buffer not-this-window-p)))
    (and point
         (unwind-protect
             (progn
               (set-buffer buffer)
               (select-window win)
               (goto-char point)
               ;; Fake a prefix arg to keep from redrawing the whole frame.
               ;; Fortunately, this recentering doesn't cause actual redisplay
               ;; on the screen until we're done.
               ;; We need to do this to update emacs' data structures with
               ;; regard to what's going to be visible in the window when that
               ;; redisplay does actually happen.
               (recenter '(0))
               (cond ((and (pos-visible-in-window-p (point-max))
                           ;; Don't bother if whole buffer is visible
                           (not (pos-visible-in-window-p (point-min))))
                      ;; The last line is blank.
                      (goto-char (point-max))
                      (forward-line -1)
                      (recenter -1)
                      (goto-char point))))
           (select-window old-win)
           (set-buffer old-buf)))
    (and selectp
         (progn
           (set-buffer buffer)
           (select-window win)))
    win))


;;; Utility functions

(defun fff-files-in-directory-list (file path &optional firstp pred)
  "Return a list of all files named FILE located in PATH.

FILE may be a string containing a single file name or it
may be a list of file names to search for.
PATH may be a list of strings or a single string composed of
colon-separated directory names.

If more than one file name is specified, then the list returned will
contain all the matches for each element of PATH grouped together, e.g.

   \(fff-files-in-directory-list '\(\"foo\" \"bar\"\) '\(\"dir1\" \"dir2\"\)\)
   => '\(\"dir1/foo\" \"dir1/bar\" \"dir2/foo\" \"dir2/bar\"\)

   NOT '\(\"dir1/foo\" \"dir2/foo\" \"dir1/bar\" \"dir2/bar\"\)

Optional third argument PRED can be an arbitrary function of one
argument \(e.g. 'file-readable-p\), which should return non-`nil' if a file
name candidate should be returned.

If optional fourth argument FIRSTP is non-`nil', then return only the
first name found \(as a single-element list\)."
  (and (stringp file)
       (setq file (list file)))
  (and (stringp path)
       (setq path (fff-path-string->list path)))
  (let ((matches nil)
        flist f)
    (while path
      (setq flist file)
      (while flist
        (setq f (expand-file-name (concat (file-name-as-directory (car path))
                                          (car flist))))
        (setq flist (cdr flist))
        (and (file-exists-p f)
             (or (null pred)
                 (funcall pred f))
             ;; Avoid duplicates
             (not (fff-member f matches))
             (progn
               (setq matches (cons f matches))
               (and firstp
                    (setq file nil
                          flist nil
                          path nil)))))
      (setq path (cdr path)))
    (nreverse matches)))

(defun fff-symbol-list->obarray (list)
  ;; new-obarray's length needs to be prime for good hashing
  ;; characteristics.  Although some lists may be substantially larger, I
  ;; think 17 is a good choice for the average case, particularly for its
  ;; usage in this package.
  (let ((new-obarray (make-vector 17 0)))
    (while list
      (intern (symbol-name (car list)) new-obarray)
      (setq list (cdr list)))
    new-obarray))

(defun fff-env->obarray (&optional envlist)
  (let ((new-obarray (make-vector 17 0))
        (list (or envlist process-environment))
        (match-data (match-data)))
    (while list
      (and (string-match "=" (car list))
           (intern (substring (car list) 0 (1- (match-end 0))) new-obarray))
      (setq list (cdr list)))
    (store-match-data match-data)
    new-obarray))

(defun fff-path-string->list (path)
  "Convert a colon-separated path string into a list.
Any null paths are converted to \".\" in the returned list so that
elements of the path may be treated consistently when prepending them to
file names."
  (let* ((list (string-split path ":"))
         (l list))
    (while l
      (and (string= "" (car l))
           (setcar l "."))
      (setq l (cdr l)))
    list))

(defun fff-string-split (string separator)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
SEPARATOR can be any regexp, but anything matching the separator will never
appear in any of the returned substrings."
  (let ((match-data (match-data))
        (string-list nil)
        (len (length string))
        (pos 0)
        str)
    (while (<= pos len)
      (cond ((string-match separator string pos)
             (setq str (substring string pos (match-beginning 0)))
             (setq pos (match-end 0)))
            (t
             (setq str (substring string pos))
             (setq pos (1+ len))))
      (setq string-list (cons str string-list)))
    (store-match-data match-data)
    (setq string (nreverse string-list))))

;; This doesn't always eval the sexp read from the minibuffer.
;; If it's a symbol and that symbol is bound to a value, return that symbol
;; instead of the symbol's value.
(defun fff-read-eval-sexp (prompt)
  (let ((result nil)
        (sexp nil))
    (while (null result)
      (condition-case errlist
          (setq sexp (read-from-minibuffer prompt nil minibuffer-local-map t)
                result (if (and (symbolp sexp)
                                (boundp sexp))
                           sexp
                         (eval sexp)))
        (error (message "Error: %s: %s"
                        (mapconcat 'symbol-name (cdr errlist) " ")
                        (get (car errlist) 'error-message))
               (sit-for 5))))
    result))

(defun fff-file-nondirectory-p (f)
  (and (file-exists-p f)
       (not (file-directory-p f))))

;; Emacs 18 doesn't have `member'.
(defun fff-member-fn (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT."
  (while (and list
              (not (equal elt (car list))))
    (setq list (cdr list)))
  list)


;;; Keymap installation

;; I should be beaten with a stick for doing this.
(defmacro fff-controlify-key-sequence (key-sequence)
  (setq key-sequence (copy-sequence key-sequence))
  (let* ((tmpl (copy-sequence "?\\C-*"))
         (tmplidx (1- (length tmpl)))
         (len (length key-sequence))
         (i 0))
    (while (< i len)
      (aset tmpl tmplidx (aref key-sequence i))
      (aset key-sequence i (read tmpl))
      (setq i (1+ i))))
  key-sequence)

(defmacro fff-make-sparse-keymap (&optional string)
  (if (and string
           fff-menu-bar-support-p
           (eq fff-emacs-variant 'gnu))
      (list 'make-sparse-keymap string)
    '(make-sparse-keymap)))

(defmacro fff-define-key (seq fn &optional menu-descrip)
  (let ((fndef (if (and fff-menu-bar-support-p
                        menu-descrip)
                   (cons menu-descrip fn)
                 fn)))
    (list 'define-key
          'fff-map
          (macroexpand (list 'fff-controlify-key-sequence seq))
          (list 'quote fndef))))

(defun fff-install-map (&optional overridep keymap-prefix)
  "Install the fff keymap."
  (interactive "P")

  (cond ((null fff-map)
         ;(setq fff-map (fff-make-sparse-keymap "FFF"))
         (setq fff-map (fff-make-sparse-keymap))

         ;; Listed in reverse of desired order so that menu bar will be in
         ;; correct order.

         (fff-define-key "h"  describe-prefix-bindings)

         (fff-define-key "if" fff-insert-file-in-locate-db
                         "Insert file from `locate' DB")

         (fff-define-key "ip" fff-insert-file-in-path
                         "Insert file from path")

         (fff-define-key "iv" fff-insert-file-in-envvar-path
                         "Insert file from environment path")

         (fff-define-key "ie" fff-insert-file-in-exec-path
                         "Insert file from exec-path")

         (fff-define-key "il" fff-insert-emacs-lisp-library
                         "Insert emacs lisp library")

         (fff-define-key "f"  fff-find-file-in-locate-db
                         "Find file from `locate' DB")

         (fff-define-key "p"  fff-find-file-in-path
                         "Find file from path")

         (fff-define-key "v"  fff-find-file-in-envvar-path
                         "Find file from environment path")

         (fff-define-key "e"  fff-find-file-in-exec-path
                         "Find file from exec-path")

         (fff-define-key "d"  fff-find-loaded-emacs-lisp-function
                         "Find emacs lisp function definition")

         (fff-define-key "l"  fff-find-emacs-lisp-library
                         "Find emacs lisp library")
         ))

  (fset 'fff-command-prefix fff-map)
  (and keymap-prefix (setq fff-map-prefix keymap-prefix))

  (and fff-menu-bar-support-p
       (boundp 'menu-bar-final-items)
       (not (memq 'fff menu-bar-final-items))
       (setq menu-bar-final-items (cons 'fff menu-bar-final-items)))

  (let ((current-binding (key-binding fff-map-prefix))
        (description (key-description fff-map-prefix)))
    (cond ((eq current-binding 'fff-command-prefix))
          ((and current-binding
                (not overridep))
           (error "Prefix \"%s\" is already bound" description))
          (t
           (fff-uninstall-map)
           (cond (fff-map-prefix
                  (global-set-key fff-map-prefix 'fff-command-prefix)

                  (and fff-menu-bar-support-p
                       (global-set-key [menu-bar fff]
                                       (cons "FFF" fff-map)))))))
    (and (interactive-p)
         (message "fff commands are on prefix \"%s\"" description))))

(defun fff-uninstall-map ()
  (interactive)

  (and fff-menu-bar-support-p
       (global-unset-key [menu-bar fff]))

  (let ((existing (where-is-internal 'fff-command-prefix)))
    (while existing
      (global-unset-key (car existing))
      (setq existing (cdr existing)))))

(provide 'fff)

;;; fff.el ends here
