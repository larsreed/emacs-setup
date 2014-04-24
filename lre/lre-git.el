;;; lre-git.el --- Lars Reed - Emacs init file
;; Setup for git

;; Copyright (C) 2011-2014 Lars Reed
;;   See lresetup.el
;; Author:		Lars Reed <Lars@kalars.net>
;; Source:		http://stackoverflow.com/questions/1817370/using-ediff-as-git-mergetool
; Hi-lock: (("^;;; lre.*\\.el[^\n]*" (0 (quote hi-black-hb) t)))
; Hi-lock: (("Lars.*R[e]ed" (0 (quote hi-blue) t)))
; Hi-lock: (("^;;; [^\n]+" (0 (quote hi-green) t)))

;;; Code

;; --------------------------------------------------------------
;;
;; Setup for ediff.
;;
(require 'ediff)
(require 'advice)

(defvar lre--ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'lre--ediff-after-quit-hooks))

(defvar lre--git-mergetool-emacsclient-ediff-active nil)


(defvar lre--ediff-saved-frame-configuration nil)
(defvar lre--ediff-saved-window-configuration nil)

(defun lre--local-ediff-before-setup-hook ()
  (setq lre--ediff-saved-frame-configuration (current-frame-configuration))
  (setq lre--ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if lre--git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun lre--local-ediff-quit-hook ()
  (set-frame-configuration  lre--ediff-saved-frame-configuration)
  (set-window-configuration lre--ediff-saved-window-configuration))

(defun lre--local-ediff-suspend-hook ()
  (set-frame-configuration  lre--ediff-saved-frame-configuration)
  (set-window-configuration lre--ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'lre--local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'lre--local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'lre--local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun lre-git-mergetool-emacsclient-ediff (local remote base merged)
  (setq lre--git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun lre--git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'lre--ediff-after-quit-hooks
          'lre--git-mergetool-emacsclient-ediff-after-quit-hook
          'append)


(provide 'lre-git)

;;; lre-git.el ends here
