(provide 'w32-bash)
;=========================================================
;; Using both cygwin bash and cmd.exe shells under win2k
;=========================================================
;; See also shell.el and comin.el. 

;; Explicit shells - interactive shell in a buffer.  As described in shell.el
;; setting explicit-shell-name to "name" creates a variable explicit-name-args
;; which are passed to the shell command when an explicit shell is requested
;; (M-x shell command).

;; The problem with using a command like (setenv "PATH" (concat
;; "C:/usr/cygwin/bin;" (getenv "PATH"))) is this adds the cygwin bin direcotry
;; to the PATH, which may cause confusion if one switches to another shell. It
;; is far better to set the BASH_ENV to point to a file which sets the path for
;; a non-interactive shell (which doesn't read startup files - see man bash) for
;; running commands like make etc.

;; When bash is opened non-interactively by comint.el no startup files are read,
;; and so the path is not set. However, bash looks at the environment variable
;; BASH_ENV and reads that file prior to running whichever command, so this file
;; may be used to set the path.

;; Below are two functions for switching the shell type. Note also that the
;; shell used by auctex may be independently set using the variables TeX-shell
;; and TeX-shell-command-option after auctex has been loaded.

;; Under developement!

;; The following variables are all that should need to be modified.
(defvar cygwin-bash-dir "c:/usr/cygwin/bin/" 
  "*Directory where the bash executable resides. Used to set
cygwin-mount-cygwin-bin direcotry (cygwin-mount.el), cygwin-bin-dir (which is
appended to exec-path).")

(defvar cygwin-bin-dir (list cygwin-bash-dir
			     "c:/usr/cygwin/usr/local/bin/") 
  "*List of directories appended to exec-path containing binaries used by cygwin
bash.")

(defvar cygwin-info-dir (list "c:/usr/cygwin/usr/info/"
			      "c:/usr/cygwin/usr/local/info/")
  "*List of directories appended to Info-default-directory list for cygwin
bash.")

(defvar cygwin-bash_env "~/.bashrc" 
  "*File read when a non-interactive bash shell is started. This should set the
path for bash.")

(require 'shell)
;;; Some general stuff for using bash. 
;(setq exec-path (append (list "c:/usr/cygwin/bin/" "c:/usr/cygwin/usr/local/bin/") exec-path))
(setq exec-path (append cygwin-bin-dir exec-path))

(setenv "BASH_ENV" cygwin-bash_env) 
(setq Info-default-directory-list (append cygwin-info-dir Info-default-directory-list))
(setq process-coding-system-alist (append process-coding-system-alist 
					  '(("bash" . undecided-unix))))
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m) 

;; load cygwin-mount so emacs understands cygwin style directories
(require 'cygwin-mount)
(cygwin-mount-activate)
(setq cygwin-mount-cygwin-bin-directory cygwin-bash-dir)

;; load the ansi-color.el package so that colours don't screw up the shell buffer.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; shell switching functions
(defun set-shell-bash ()
  "Set the default shell to be bash."
  (interactive)
  (if (boundp 'w32-quote-process-args)
      (setq w32-quote-process-args ?\")) ; this seems to fuck up latex/miktex.
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name) 
  (setq explicit-shell-file-name shell-file-name) 
  (defvar explicit-bash-args '("--login" "-i")) ; see shell.el 
  (setq shell-command-switch "-c")
  (message "Shell is now bash")
  )

(defun set-shell-cmd ()
  "Set the default shell to be cmd."
  (interactive)
  (if (boundp 'w32-quote-process-args)
      (setq w32-quote-process-args t))
  (setq shell-file-name "cmd")
  (setq explicit-shell-file-name shell-file-name)
  (setenv "SHELL" shell-file-name)
  (setq shell-command-switch "/c")
  (defvar explicit-cmd-args '("/q"))
  ;;(setq comint-process-echoes t)
  (message "Shell is now cmd")
  )

;; note that in the case of cmd it is also possible to get around the shell
;; echoing problem by doing (defvar explicit-cmd-args '("/q")) to turn off
;; echoing. This would remove the need for (setq comint-process-echoes t).

;; to look for passowrd prompt:
;;;(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)


