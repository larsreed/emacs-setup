;;; lre-lisp.el --- Lars Reed - Emacs init file
;; Lisp utilities

;; Copyright (C) 2002-2014 Lars Reed
;;   See lresetup.el
;; Author:      Lars Reed <Lars@kalars.net>
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))

;;; Code

(require 'cl)
;; --------------------------------------------------------------


;;; ------------------------------------------------------------------
;;; Lisp

(defun lre--library-list (regx &optional elc-also quiet)
  "Lists libraries in `load-path' matching REGX.
If REGX ends with .el/.elc, no change is made to it, otherwise, .el (or .elc?,
iff ELC-ALSO is non-nil) is appended.
Displays progress message unless QUIET is non-nil."
  (let (flist
    (dot (if (string-match "\\(\\\\\\|\\[\\)[.]\\(\\]\\)?" regx) ""
           "[.]")))
    (if (not (string-match "\\.elc?\\(\\\\'\\|\\$\\)?$" regx))
    (setq regx (concat regx ".*" dot "el" (if elc-also "\\(c\\)?")
                           "\\([.]gz\\)?" "$")))
    (mapc
     (function (lambda (dir)
         (setq dir (file-name-as-directory (or dir "."))) ; Nil=curdir
         (or quiet (let ((message-log-max t))
                 (message "Searching dir %s..." dir)))
         (if (and (file-exists-p dir)
              (file-readable-p dir))
             (mapcar
              (function (lambda (file)
                  (add-to-list 'flist
                           (expand-file-name
                        (concat dir file)))))
              (nreverse (directory-files dir nil regx)))
           )))
     load-path)
    (or quiet (message ""))
    (remove-duplicates flist)))

(defun lre-list-libs (regx &optional elc-also)
  "List files in `load-path' matching REGX.
Automatically adds .*\\.elc? to REGX if not already ending in .el/.elc."
  (interactive "sSearch libraries matching: \nP")
  (save-mark-and-excursion
    (let ((lib-buffer "*Libraries*"))
      (switch-to-buffer-other-window (get-buffer-create lib-buffer))
      (goto-char (point-min))
      (if (= (point-min) (point-max)) nil
    (insert "\n")
    (forward-char -1))
      (insert (format "Libraries in load-path matching `%s':\n" regx))
      (mapc (function (lambda (s)
              (insert (format "\t%s\n" s))))
          (lre--library-list regx elc-also))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (shrink-window-if-larger-than-buffer))))

(defun lre-open-library (libf)
  "Open given lisp library for editing in other window(s)."
  (interactive "sOpen library file (regexp): ")
  (let ((f-path (lre--library-list libf))
    (openf 'find-file-other-frame)
    fn)
    (dolist (fn f-path)
      (when (file-exists-p fn)
    (funcall openf fn)
    (setq openf 'find-file-other-window)))))

(defun lre-byte-compile-file ()
  "Byte compile file - see `byte-compile-file'."
  (interactive)
  (let ((byte-compile-generate-call-tree t))
    (call-interactively 'byte-compile-file)))

(defun lre-search-defun (fun &optional deftype)
  "Search for defun in load-path.
The found instances can be opened by clicking in the grep window.
Optional argument DEFTYPE makes it possible to search for e.g. defconst
rather than defun."
  (interactive "sDefun: ")
  (let* ((grep-type (or deftype "defun"))
     (grep-cmd (concat "egrep -n \'" grep-type " +" fun "\' "
               (mapconcat 'identity
                      load-path
                      "/*.el ")
               "*.el")))
    (grep grep-cmd)))

(defun lre-lisp-mode ()
"Personal Lisp mode definitions."
  (lre-colors)
  (tplsub-mode 1)
  (setq mode-name "elisp")
  (lre-add-x-hilit 'emacs-lisp-mode)
  (setq lisp-font-lock-keywords (append lisp-font-lock-keywords-2
        lre-font-lock-specials))
  (when (lre-memb 'keys)
    (define-key emacs-lisp-mode-map [C-return] 'newline-and-indent)
    (define-key emacs-lisp-mode-map "\C-cc" 'checkdoc)
    (define-key emacs-lisp-mode-map "\C-cr" 'byte-recompile-directory)
    (define-key emacs-lisp-mode-map "\C-cf" 'lre-byte-compile-file)
    (define-key emacs-lisp-mode-map "\C-cs" 'lre-search-defun)
    (define-key emacs-lisp-mode-map "\C-ce" 'eval-buffer)
    (elisp-slime-nav-mode t)
    )
  (lre--imenu-add)
  (lre-brace-mode 1))

(defun lre-toggle-variable (var)
  "Toggle a variable (assumed to be boolean)."
  (interactive
   (list (completing-read "Variable: " obarray
              #'(lambda (v) (and (symbolp v) (boundp v)))
              t (let ((var (variable-at-point)))
                  (if var (symbol-name var))))))
  (let ((val (symbol-value var)))
    (set-variable var (not val))
    (message "%s set to %s" var (not val))))

(provide 'lre-lisp)

;;; lre-lisp.el ends here
