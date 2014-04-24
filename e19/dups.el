;;!emacs
;;
;; LCD-ENTRY:    dup-el|Bob Weiner|weiner@mot.com|List duplicate .el files|06-05-95|1.3
;;
;; FILE:         dup-el.el
;; SUMMARY:      Display a list of Emacs Lisp files duplicated in load-path.
;; USAGE:        Emacs 19 Lisp Library
;; KEYWORDS:     lisp, maint
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., PPG
;;
;; ORIG-DATE:     1-Jun-95 at 15:35:13
;; LAST-MOD:     12-Jun-95 at 15:32:59 by Bob Weiner
;;
;; Copyright (C) 1995  Free Software Foundation, Inc.
;;
;; This file is part of InfoDock.
;;
;; DESCRIPTION:  
;;
;;   Use {M-x duplicates-list RET} to list duplicate Emacs Lisp files
;;   within your current InfoDock or Emacs load-path.  It will display a list
;;   of files which appear 2 or more times in load-path directories together
;;   with their full pathnames.
;;
;;   Use {M-x duplicates-matches RET} to display a listing of
;;   matches for FILE-REGEXP in load-path order.  You are prompted for
;;   FILE-REGEXP.
;;
;;   Utilizes GNU or UNIX sed, uniq, join and sort commands.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar duplicates-path-column 26
  "*Column to which to indent pathnames of duplicate Emacs Lisp files.
This should be greater than the length of the longest Lisp file name, if you
want all pathnames to be aligned.  Set this to 1 for much faster operation
but less attractive output.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun duplicates-list ()
  "Display a buffer of Emacs Lisp files found 2 or more times within load-path."
  (interactive)
  (let* ((files "/tmp/f")
	 (file-buf (buffer-name (find-file-noselect files)))
	 (duplicates "/tmp/d")
	 (dup-buf (buffer-name (find-file-noselect duplicates)))
	 (out-buf (buffer-name (get-buffer-create "*Duplicated-Files*"))))
    (set-buffer dup-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((standard-output (get-buffer dup-buf)))
      (duplicates-print-files))
    (call-process-region (point-min) (point-max) "sort" t t)
    (basic-save-buffer)
    (message "")
    (set-buffer (get-buffer-create file-buf))
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-buffer dup-buf)
    (append-to-buffer file-buf (point-min) (point-max))
    (set-buffer file-buf)
    (call-process-region (point-min) (point-max) "sed" t t nil "-e" "s/ .*//")
    (call-process-region (point-min) (point-max) "uniq" t t nil "-d")
    (basic-save-buffer)
    (message "")
    (kill-buffer file-buf) (kill-buffer dup-buf)
    (switch-to-buffer out-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (call-process "join" nil out-buf nil files duplicates)
    (delete-file files) (delete-file duplicates)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (message "")))

(defun duplicates-matches (file-regexp)
  "Display a buffer listing the ordered load-path matches for FILE-REGEXP."
  (interactive "sFind regexp matches for load-path file name: ")
  (message "Searching for %s matches in load-path..." file-regexp)
  (let ((elisp-buf "*Load-Paths*"))
    (with-output-to-temp-buffer elisp-buf
      (princ "Load-path matches for regexp ")
      (prin1 file-regexp) (princ ":") (terpri)
      (if (not (string-match "\\.elc?\\(\\\\'\\|\\$\\)?$" file-regexp))
	  (setq file-regexp (concat file-regexp ".*\\.elc?$")))
      (duplicates-print-paths file-regexp))
    (set-buffer elisp-buf)
    (goto-char (point-min))
    (set-buffer-modified-p nil))
  (message ""))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun duplicates-print-files (&optional directory-list)
  "Print `lisp-file full-path-of-lisp-file' lines to standard output.
Optional argument is a list of Emacs Lisp directories to search; no
argument searches directories in the `load-path' variable."
  (or directory-list (setq directory-list load-path))
  (let ((listed-directories)
	(spaces 0)
	(expanded-directory))
    (mapcar
     (function
      (lambda (d)
	;; Nil as an entry means use current directory.
	(setq d (file-name-as-directory (or d "."))
	      expanded-directory (expand-file-name d))
	;; Omit non-existent, unreadable, or previously scanned directory
	;; entries.
	(if (not (and (file-exists-p d) (file-readable-p d)
		      (not (member expanded-directory listed-directories))))
	    nil
	  (setq listed-directories
		(cons expanded-directory listed-directories))
	  (message "Listing %s ..." d)
	  (mapcar
	   (function
	    (lambda (f) 
	      (setq spaces (max 1 (- duplicates-path-column (+ 2 (length f)))))
	      (princ f) (princ " ")
	      (while (> spaces 0)
		(princ ".") (setq spaces (1- spaces)))
	      (princ " ")
	      (princ "\"") (princ d) (princ f) (princ "\"") (terpri)))
	   (directory-files d nil "^[^=].*\\.el$")))))
     directory-list))
  (message ""))

(defun duplicates-print-paths (file-regexp)
  "Print each entry in load-path matching FILE-REGEXP to the standard output."
  (mapcar
   (function
    (lambda (d)
      ;; Nil as an entry means use current directory.
      (setq d (file-name-as-directory (or d ".")))
      ;; Omit non-existent or unreadable directory entries.
      (if (not (and (file-exists-p d) (file-readable-p d)))
	  nil
	(mapcar
	 (function
	  (lambda (f) 
	    (princ "\"") (princ d) (princ f) (princ "\"") (terpri)))
	 (nreverse (directory-files d nil file-regexp))))))
   load-path))

(provide 'dup-el)

