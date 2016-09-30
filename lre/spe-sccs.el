;;; spe-sccs.el --- SPE SCCS minor mode

;;; Copyright (C) 1995-2004 Lars Reed

;; Author:		Lars Reed <Lars@kalars.net>
;; Version:		1.3
;; Keywords:		SPE SCCS
;; Adapted-By:

;;; Commentary:

;; ....

;;; Code:
;;; ========================================================================

(defvar spe-sccs-mode-submap nil
  "Keymap used with spe-sccs mode.")

(defvar spe-sccs-mode-map nil
  "Keymap used with spe-sccs mode.")

(defvar spe-sccs-mode-prefix-key "\C-cx"
  "*Prefix key for all spe-sccs mode commands.")

(defconst spe-sccs-mode-name " SPE"
  "Name used in mode-line.")

(defconst spe-sccs-buffer-name "*spe-sccs*"
  "Name used for buffer.")

(defvar spe-sccs-mode nil
  "Non-nil when spe-sccs-mode is activated.")


; ----------------------------------------------------------------------

(defvar sub-list-hist nil
  "History of procedure/file names in Sysdul/SCCS mode.")
(defvar fe-list-hist nil
  "History of FEids used in SCCS commands.")
(defvar spe-sccs-last-file "coadm" "Last file accessed by SCCS command.")
(defvar spe-sccs-last-fe "f950000" "Last FEid used by SCCS command.")
(defvar spe-sccs-menu-base
  '(
    [ "finfo"  spe-sccs-finfo  t]
    [ "gjort"  spe-sccs-gjort  t]
    [ "genmal" spe-sccs-genmal t]
    [ "vis"    spe-sccs-vis    t]
    )'
  "Menu")
(if (>= emacs-major-version 20)
    (progn
;;      (easy-menu-define spe-sccs-mode-menu spe-sccs-mode-submap "SCCS menu"
;;			(cons "SCCS" spe-sccs-menu-base))
      (define-key global-map [menu-bar tools spe-sccs] 
	(cons "SCCS" (easy-menu-create-menu "SCCS" spe-sccs-menu-base)))))

; ======================================================================

(if spe-sccs-mode-submap  ()
  (setq spe-sccs-mode-submap (make-sparse-keymap))
  (define-key spe-sccs-mode-submap "f" 'spe-sccs-finfo)
  (define-key spe-sccs-mode-submap "g" 'spe-sccs-gjort)
  (define-key spe-sccs-mode-submap "h" 'spe-sccs-hent)
  (define-key spe-sccs-mode-submap "m" 'spe-sccs-genmal)
  (define-key spe-sccs-mode-submap "v" 'spe-sccs-vis))

;; Set up main map, which leads via the prefix key to the submap.
(if spe-sccs-mode-map ()
  (setq spe-sccs-mode-map (make-sparse-keymap))
  (define-key spe-sccs-mode-map
    spe-sccs-mode-prefix-key
    spe-sccs-mode-submap))


;; Activate map
(or (assq 'spe-sccs-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'spe-sccs-mode spe-sccs-mode-map)
                minor-mode-map-alist)))

;; Display name
(or (assq 'spe-sccs-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(spe-sccs-mode spe-sccs-mode-name)
                minor-mode-alist)))


; ======================================================================


(defun sub-sccs-get (prompt lvar hlist)
  "General spe-sccs-mode prompt function."
  (let (ans)
    (setq ans
	  (read-from-minibuffer (concat prompt " (M-p/n for hist.): ")
				lvar nil nil hlist))
    (if (= (length ans) 0) (if lvar (setq ans lvar)))
    (setq ans ans)))

; Function to set spe-sccs-last-fe
(defun spe-sccs-get-fe ()
  "Function to prompt for and set spe-sccs-last-fe."
  (setq spe-sccs-last-fe
	(sub-sccs-get "(SCCS) FEid" spe-sccs-last-fe 'fe-list-hist)))


(defun spe-sccs-replace-in-string (s from &optional to)
  "Replace all occurences in the string S of the regexp FROM to the string TO."
  (if (and s
	   from
	   (> (length s) 0)
	   (> (length from) 0))
      (save-match-data
	(let (p)
	  (while (string-match from s (and p
					   (+ (match-beginning 0)
					      (length to))))
	    (setq p t
		  s (concat (substring s 0 (match-beginning 0))
			    to
			    (substring s (match-end 0))))))))
  s)

; Function to set spe-sccs-last-file
(defun spe-sccs-get-file (&optional curr)
  "Function to prompt for and set spe-sccs-last-file."
  (let (fn)
    (setq fn (if curr
		 (spe-sccs-replace-in-string (abbreviate-file-name
					      buffer-file-name)
					     default-directory)
						  
	       (symbol-name (symbol-at-point))))
    (if (null fn) (setq fn "*.vsd"))
    (setq spe-sccs-last-file
	(sub-sccs-get "(SCCS) File" fn 'sub-list-hist))))

; Function to get SCCS options
(defun spe-sccs-get-opts (defl)
  "Function to prompt for and get sccs options."
  (read-from-minibuffer "(SCCS) Options: " defl nil nil nil))


; ----------------------------------------------------------------------

(defun sub-sccs-scratch (flag &optional cmd)
  "Scratch pad for SCCS-functions.
FLAG==0:  create if necessary, pop return buffer
FLAG==1:  run cmd - return value is command output
FLAG==-1: close buffer, return t/nil."
  (save-mark-and-excursion
    (let (bf txt)
      (cond ((= flag 0)
	     (set-buffer (get-buffer-create spe-sccs-buffer-name)))
	    ((= flag 1)
	     (shell-command cmd t)
	     (set-buffer-modified-p nil)
	     (setq txt (buffer-substring (point) (- (point-max) 1))))
	    ((= flag -1)
	     (setq bf (get-buffer spe-sccs-buffer-name))
	     (if bf (kill-buffer bf)))))))

(defun sub-sccs-cmd (flag cmd)
  "Run SCCS command in window, flag>0 => display, prepend to buffer,
flag <0 => invisible."
  (let ((mtxt (format "SCCS %s ..." cmd))
	currb)
    (message "%s" mtxt)
    (save-mark-and-excursion
      (set-buffer (sub-sccs-scratch 0))
      (if (> flag 0) (progn
		       (goto-char (point-min))
		       (insert
			(format "\n\n\n***** End of %s *****\n\n\n" mtxt))
		       (goto-char (point-min)))
	(goto-char (point-max)))
      (message mtxt)
      (setq mtxt (sub-sccs-scratch 1 cmd)))
    (if (> flag 0) (progn
		     (setq currb (current-buffer))
		     (pop-to-buffer spe-sccs-buffer-name)
		     (goto-char (point-min))
		     (pop-to-buffer currb))
      mtxt)))

(defun spe-sccs-fexpand (name &optional flags)
  "Get full name of SCCS file (without path)."
  (let (flg)
    (setq flg (if flags flags "-f"))
    (sub-sccs-cmd -1 (format "fexpand %s %s" flg name))))

; ----------------------------------------------------------------------

(defun spe-sccs-hent ()
  "SCCS hent ..."
  (interactive)
  (let (fn opts flist cmd)
    (spe-sccs-get-file)
    (spe-sccs-get-fe)
    (setq opts (spe-sccs-get-opts "-n"))
    (setq fn (spe-sccs-fexpand spe-sccs-last-file))
    (setq cmd (concat "hent -M " spe-sccs-last-fe " " opts " " fn))
    (sub-sccs-cmd -1 cmd)
    (if (not (listp fn)) (setq flist (list fn))
      (setq flist fn))
    (while flist
      (setq fn (car flist)
	    flist (cdr flist))
      (find-file-other-window fn))))

(defun spe-sccs-finfo ()
  "SCCS finfo ..."
  (interactive)
  (let (opts)
    (spe-sccs-get-file t)
    (setq opts (spe-sccs-get-opts "-a"))
    (sub-sccs-cmd 1 (concat "finfo " opts " " spe-sccs-last-file))))

(defun spe-sccs-vis ()
  "SCCS vis..."
  (interactive)
  (spe-sccs-get-file)
  (let (opts)
    (setq opts (spe-sccs-get-opts "-k"))
    (sub-sccs-cmd 1 (concat "vis " opts " " spe-sccs-last-file))))

(defun spe-sccs-gjort ()
  "SCCS gjort..."
  (interactive)
  (save-some-buffers)
  (spe-sccs-get-file t)
  (let (opts)
    ;;    (setq opts (spe-sccs-get-opts "-i -w"))
    (setq opts (spe-sccs-get-opts ""))
    (sub-sccs-cmd 1 (concat "gjort " opts " " spe-sccs-last-file))))

(defun spe-sccs-genmal ()
  "genmal..."
  (interactive)
  (let (opts)
    (setq opts (spe-sccs-get-opts "-q"))
    (shell-command (concat "xterm -e genmal " opts " &"))))


; ======================================================================

;;;###autoload
(defun spe-sccs-mode (arg)
  "Toggle SPE-SCCS minor mode.
With ARG, turn SPE-SCCS mode on iff arg is positive.
The mode, whose commands all have prefix \\[spe-sccs-mode-prefix-key],
contains functions to access the SPE (Sysdeco Programming Environment)
SCCS (Source Code Control System) scripts.

\\{spe-sccs-mode-map}"

  (interactive "P")
  (make-local-variable 'spe-sccs-mode)
  (setq spe-sccs-mode
	(if (null arg)
	    (not spe-sccs-mode)
	  (> (prefix-numeric-value arg) 0)))

  (force-mode-line-update)

  (if spe-sccs-mode
      (progn
	(run-hooks 'spe-sccs-mode-hook))))


(provide 'spe-sccs)

;;; spe-sccs.el ends here
