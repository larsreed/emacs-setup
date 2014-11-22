;; Version:    See cc-mode.el

(setq path-to-the-custom-library
      "d:/emacs/site-lisp")

(setq load-path (cons "./" load-path))
(if path-to-the-custom-library
    (setq load-path (cons path-to-the-custom-library load-path)))
(if (and (condition-case nil
	     (require 'custom)
	   (error nil))
	 ;; Stock Emacs 19.34 doesn't have this
	 (fboundp 'defcustom))
    (progn
      ;; Always get the compile time definitions
      (require 'cc-defs)
      (require 'cc-menus)
      (if (or (not (fboundp 'functionp))
	      (not (fboundp 'char-before))
	      (not (c-safe (char-after) t))
	      (not (fboundp 'when))
	      (not (fboundp 'unless)))
	  ;; cc-mode-19.el contains macros that should be compiled in.
	  (require 'cc-mode-19))
      (batch-byte-compile))
  (error "STOP! STOP! STOP! STOP!

The Custom library was not found or is out of date.  A more current
version is required to use CC Mode 5.  You MUST fix cc-make.el.  See
that file or the CC Mode README for details."))
