;;; ccasetup.el  --- GNU Emacs configuration for CCAS

;;; Copyright (C) 1995 Lars Reed

;; Author:		Lars Reed <lre@sysdeco.no>
;;       Desember 1993 kjellm
;;          Basert på tilsvarede oppsettfil ved Ifi/Uio (Anders Ellefsrud).
;; Last-Modified:	96/03/15
;; Version:		1.28
;; Keywords:		Sysdeco SCCS
;; Adapted-By:

;;; Commentary:

;; ....

;;; Code:
;;; ========================================================================






;;
;; Diverse fra Ifi/Uio.
;;
;; Rutinene for kompilering bør skrives om til å utnytte mulighetene i
;; kompileringsrutinene til emacs ver. 19.
;;
(defvar compile-command "make komp "
  "Last shell command used to do a compilation; default for next compilation.

Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:

    (setq c-mode-hook
      '(lambda () (or (file-exists-p \"makefile\") (file-exists-p \"Makefile\")
                      (progn (make-local-variable 'compile-command)
                             (setq compile-command
                                    (concat \"make -k \"
                                            buffer-file-name))))))")
(defun ifi-compile ()
  "Compile the program in the current buffer.  Make a sensible suggestion
as to the correct compilation command."
  (interactive)
  (compile (read-string "Compile command: " compile-command)))

;;
;; Lokale rebinding av tastatur (basert på tilsv. fra Ifi).
;;
(setq Info-default-directory-list '("/progs/local/lib/emacs/19.28/etc"))


;;
;; GNUS.
;;

;;
;; Diverse funksjoner.
;;

;;
;; Autoloads.
;;
;

;;; ccasetup.el ends here
