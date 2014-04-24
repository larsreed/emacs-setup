;;; saveplace.el --- automatically save place in files

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Maintainer: FSF
;; Last Change: Lars.Reed@mesan.no
;; Created: July, 1993 (lre 96/09/09)
;; Version: 1.2 (lre 1.28)
;; Keywords: bookmarks, placeholders, menus, files

;; This file is part of GNU Emacs.

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

;; Automatically save place in files, so that visiting them later
;; (even during a different Emacs session) automatically moves point
;; to the saved position, when the file is first found.  Uses the
;; value of buffer-local variable save-place to determine whether to
;; save position or not.
;;
;; Thanks to Stefan Schoef, who sent a patch with the
;; `save-place-version-control' stuff in it.

;;; Code:

;; this is what I was using during testing:
;; (define-key ctl-x-map "p" 'toggle-save-place)

(defgroup save-place nil
  "Automatically save place in files."
  :group 'data)

(defvar save-place-alist nil
  "Alist of saved places to go back to when revisiting files.
Each element looks like (FILENAME . POSITION);
visiting file FILENAME goes automatically to position POSITION
rather than the beginning of the buffer.
This alist is saved between Emacs sessions.")

(defcustom save-place-ignore '("^/tmp/" "\\.tmp$"
			       "~$"     "\\.bak$"
			       "\\.newsrc\.eld")
  "*List of filespecs (regexps) to exclude from save place list.
You would might like to add files like the VM crashbox etc."
  :group 'save-place
  :type '(repeat regexp))

(defcustom save-place-masters (list user-init-file)
  "*List of files to place on top of the recent file menu"
  :group 'save-place
  :type '(repeat regexp))

(defcustom save-place-keep-eof t
  "*If non-nil, save place at EOF as special marker,
rather than an absolute offset from BOF."
  :group 'save-place
  :type 'boolean)

(defcustom save-place-use-file-menu t
  "*If non-nil, add a Recent menu to the Files menu."
  :group 'save-place
  :type 'boolean)

(defcustom save-place nil
  "*Non-nil means automatically save place in each file.
This means when you visit a file, point goes to the last place
where it was when you previously visited the same file.
This variable is automatically buffer-local.

If you wish your place in any file to always be automatically saved,
simply put this in your `~/.emacs' file:

\(setq-default save-place t\)"
  :type 'boolean
  :group 'save-place)

(make-variable-buffer-local 'save-place)

(defcustom save-place-file
  (cond ((fboundp 'convert-standard-filename)
	 (convert-standard-filename "~/.emacs-places"))
	((or (eq system-type 'ms-dos)
	     (eq system-type 'windows-nt))
	 "~/emacs.pla")
	(t "~/.emacs-places"))
  "*Name of the file that records `save-place-alist' value."
  :type 'file
  :group 'save-place)

(defcustom save-place-version-control 'nospecial
  "*Controls whether to make numbered backups of master save-place file.
It can have four values: t, nil, `never', and `nospecial'.  The first
three have the same meaning that they do for the variable
`version-control', and the final value `nospecial' means just use the
value of `version-control'."
  :type '(radio (const :tag "Unconditionally" t)
		(const :tag "For VC Files" nil)
		(const never)
		(const :tag "Use value of `version-control'" nospecial))
  :group 'save-place)

(defvar save-place-loaded nil
  "Non-nil means that the `save-place-file' has been loaded.")

(defcustom save-place-limit 500
  "Maximum number of entries to retain in the list; nil means no limit."
  :type '(choice (integer :tag "Entries" :value 1)
		 (const :tag "No Limit" nil))
  :group 'save-place)

(defcustom save-place-delta (if (numberp save-place-limit)
				(/ save-place-limit 5)
			      100)
  "*No. of extra entries to delete when truncating list -
to avoid having to truncate the list each time it is saved."
  :group 'save-place
  :type 'integer)

(defcustom save-place-recent-no 20
  "*No of items in history menu.  Nil to avoid menu."
  :type '(choice (integer :tag "Entries" :value 1)
		 (const :tag "No Limit" nil))
  :group 'save-place)

(defvar save-place-recent-menu nil
  "Menu of recent files.")

(defvar save-place-recent-hooks nil
  "Hooks to run when updating recent menu")

(defun save-place-find-file ()
  ;; Help routine to open a file from a menu list.
  (interactive)
  (find-file last-command-event))

(defun save-place-find-file-other-frame ()
  ;; Help routine to open a file in a new frame from a menu list.
  (interactive)
  (find-file-other-frame last-command-event))

(defun save-place-find-file-other-window ()
  ;; Help routine to open a file in a new window from a menu list.
  (interactive)
  (find-file-other-window last-command-event))

(defun save-place-make-recent-files-menu (&optional other)
  ;; Create Recent-menu on the main File menu.
  (let ((idx save-place-recent-no)
	(alist save-place-alist)
	(mlist save-place-masters)
	(ofunc (if other (if (eq other 'frame)
			     'save-place-find-file-other-frame
			   'save-place-find-file-other-window)
		 'save-place-find-file))
	(otitle (if other (if (eq other 'frame)
			      "Open recent new frame"
			    "Open recent other window")
		  "Open recent"))
	(nbr 0)
	fnam)
    (while (and (> idx 0)
		alist)
      (setq idx (1- idx)
	    fnam (abbreviate-file-name (car (car alist)))
	    alist (cdr alist))
      (or (member fnam mlist)
	  (setq mlist (append mlist (list fnam)))))
    (cons otitle
	  (cons 'keymap
		(cons "Recently opened files"
		      (nconc
		       (mapcar
			(function (lambda (elt)
				    (setq nbr (1+ nbr))
				    (nconc
				     (list elt
					   (format "(%2d) %s" nbr elt)
					   '(nil .  nil))
				     ofunc)))
			mlist)))))))

(defun save-place-recent-files ()
  ;; Create Recent-menu on the main File menu.
  (if save-place-recent-no
      (progn
	(if (not save-place-use-file-menu) ()
	  (setq save-place-recent-menu
		(save-place-make-recent-files-menu 'other))
	  (define-key global-map [menu-bar files lambda-save] '("--"))
	  (define-key global-map [menu-bar files recent]
	    save-place-recent-menu))
	(run-hooks 'save-place-recent-hooks))))

(defun toggle-save-place (&optional parg)
  "Toggle whether to save your place in this file between sessions.
If this mode is enabled, point is recorded when you kill the buffer
or exit Emacs.  Visiting this file again will go to that position,
even in a later Emacs session.

If called with a prefix arg, the mode is enabled if and only if
the argument is positive.

To save places automatically in all files, put this in your `.emacs' file:
\(setq-default save-place t\)

The variable `save-place-ignore' can be set to a list of regular expressions
matching names of files you do NOT want to keep a record of.

Normally, if the current position of point is at *the end* of the buffer,
point will be restored to the end, even if the file has grown in the
meantime.  To always store the position as a relative distance from the
start of the file, set `save-place-keep-eof' to nil.

When using saveplace, a `recently used files' menu will also be created.
Use \(setq save-place-recent-no nil\) in .emacs to turn this off \(or
substitute nil with a number to change the default size of 20 files\).

To keep the list of remembered files at a manageable size, it will be
truncated when it contains more than `save-place-limit' \(default 500\)
items. When truncating, an additional `save-place-delta' items
\(default 100, i.e. a resulting size of 400\), will be removed, to avoid
having to continously truncate the list once the limit has been reached."
  (interactive "P")
  (if (not buffer-file-name)
      (message "Buffer `%s' not visiting a file" (buffer-name))
    (if (and save-place (or (not parg) (<= parg 0)))
	(progn
	  (message "No place will be saved in this file")
	  (setq save-place nil))
      (message "Place will be saved")
      (setq save-place t))))

(defun save-place-to-alist ()
  ;; put filename and point in a cons box and then cons that onto the
  ;; front of the save-place-alist, if save-place is non-nil.
  ;; \(This is provided the filename is not in the ignored list.\)
  ;; Otherwise, just delete that file from the alist.
  ;; first check to make sure alist has been loaded in from the master
  ;; file.  If not, do so, then feel free to modify the alist.  It
  ;; will be saved again when Emacs is killed.
  (or save-place-loaded (load-save-place-alist-from-file))
  (if buffer-file-name
      (progn
        (let ((cell (assoc buffer-file-name save-place-alist)))
          (if cell
              (setq save-place-alist (delq cell save-place-alist))))

        (if (and save-place
		 ;; Do not bother to save BOF - we end up here anyway
		 (or (> (point) 1)
		 ;; unless the menu is used - otherwise we risk losing the file
		     save-place-recent-menu)
		 (not (string-match
		       (concat "\\("
			       (mapconcat 'identity save-place-ignore
					  "\\)\\|\\(")
			       "\\)")
		       (expand-file-name buffer-file-name))))
	    (setq save-place-alist
		  (cons (cons buffer-file-name
			      (cond
			       ((and save-place-keep-eof
				     (= (point) (point-max)))
				-1) ; Special marker for EOF
			       ((eq major-mode 'hexl-mode)
				(1+ hexl-current-address))
			       (t
				(point))))
			save-place-alist))))))

(defun save-place-current ()
  ;; Hook function.  Enters the current buffer into the alist, and updates
  ;; the Recent-menu.
  (save-place-to-alist)
  (save-place-recent-files))

(defun save-place-alist-to-file ()
  (let ((file (expand-file-name save-place-file)))
    (save-excursion
      (message "Saving places to %s..." file)
      (set-buffer (get-buffer-create " *Saved Places*"))
      (delete-region (point-min) (point-max))
;;      (if (file-readable-p file)
;;          (insert-file-contents file))
;;      (delete-region (point-min) (point-max))
;;      (goto-char (point-min))
      (print save-place-alist (current-buffer))
      (let ((version-control
             (cond
              ((null save-place-version-control) nil)
              ((eq 'never save-place-version-control) 'never)
              ((eq 'nospecial save-place-version-control) version-control)
              (t
               t))))
        (write-file file)
        (kill-buffer (current-buffer))
        (message "Saving places to %s...done" file)))))

(defun load-save-place-alist-from-file ()
  (if (not save-place-loaded)
      (progn
        (setq save-place-loaded t)
        (let ((file (expand-file-name save-place-file)))
          ;; make sure that the alist does not get overwritten, and then
          ;; load it if it exists:
          (if (file-readable-p file)
              (save-excursion
                (message "Loading places from %s..." save-place-file)
                ;; don't want to use find-file because we have been
                ;; adding hooks to it.
                (set-buffer (get-buffer-create " *Saved Places*"))
                (delete-region (point-min) (point-max))
                (insert-file-contents file)
                (goto-char (point-min))
                (setq save-place-alist
                      (car (read-from-string
                            (buffer-substring (point-min) (point-max)))))
                (kill-buffer (current-buffer))
                (message "Loading places from %s...done" file)
                t)
            t)
          nil)
	(save-place-recent-files))))

(defun save-places-to-alist ()
  ;; go through buffer-list, saving places to alist if save-place is
  ;; non-nil, deleting them from alist if it is nil.
  (let ((buf-list (buffer-list)))
    (while buf-list
      ;; put this into a save-excursion in case someone is counting on
      ;; another function in kill-emacs-hook to act on the last buffer
      ;; they were in:
      (save-excursion
	(set-buffer (car buf-list))
	;; save-place checks buffer-file-name too, but we can avoid
	;; overhead of function call by checking here too.
	(and buffer-file-name (save-place-to-alist))
	(setq buf-list (cdr buf-list)))))
  (if (and (numberp save-place-limit)  ; Not nil
	   (> save-place-limit 0)      ; 0=nil, for now...
	   (> (length save-place-alist) save-place-limit))
      ;; Make sure list stays reasonably short...
      (let ((count save-place-limit))
	(if (numberp save-place-delta)
	    (setq count (- count save-place-delta)))
	(message "Truncating save-places...")
	(setq save-place-alist (reverse save-place-alist))
	(setq save-place-alist
	      (reverse (member (nth (- (length save-place-alist) count)
				    save-place-alist)
			       save-place-alist))))))

(defun save-place-find-file-hook ()
  (or save-place-loaded (load-save-place-alist-from-file))
  (let ((cell (assoc buffer-file-name save-place-alist)))
    (if cell
        (let ((cell-val (cdr cell)))
	  (if (= cell-val -1)
	      (setq cell-val (point-max)))
	  (or after-find-file-from-revert-buffer
	      (goto-char cell-val))
          ;; and make sure it will be saved again for later
          (setq save-place t)))))

(defun save-place-kill-emacs-hook ()
  ;; First update the alist.  This loads the old save-place-file if nec.
  (save-places-to-alist)
  ;; Now save the alist in the file, if we have ever loaded the file
  ;; (including just now).
  (if save-place-loaded
      (save-place-alist-to-file)))

(defun show-save-place ()
  (interactive)
  (find-file-other-window (expand-file-name save-place-file)))

(add-hook 'find-file-hooks 'save-place-find-file-hook t)

(add-hook 'kill-emacs-hook 'save-place-kill-emacs-hook)

(add-hook 'kill-buffer-hook 'save-place-current)

(provide 'saveplace)

;;; saveplace.el ends here
