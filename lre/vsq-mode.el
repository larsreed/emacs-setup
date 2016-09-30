;;; vsq-mode.el  --- Addtitions to sql-mode for vsq-files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:		Lars Reed <Lars@kalars.net>
;; Version:		1.3
;; Keywords:		programming SQL
;;
;;; History:
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

(eval-when-compile
  (if (lre-memb 'e21+)
      (require 'sql)
    (require 'sql-mode))
  (require 'cc-mode))

(eval-and-compile
  (require 'derived))

(defconst vsq-sep "§§\\"
  "Macro line separator")

(defun vsq-define-macro (name &optional P)
  "Define macro, with pair of ifdefs and comments."
  (interactive (list (read-from-minibuffer "Macro name: ")
		     current-prefix-arg))
  (beginning-of-line)
  (insert "#ifndef " name "\n"
	  "#define " name " " vsq-sep "\n")
  (insert (if P "\t"
	    (concat "\t; <\""  name "\" " vsq-sep "\n\t")))
  (save-mark-and-excursion
    (if P (insert "\n#endif\n")
      (insert " " vsq-sep "\n"
	      "\t; \"" name "\">\n"
	      "#endif\n"))))

(defun vsq-linefeed (P)
  "Legger inn `vsq-sep', linjeskift og indent."
  (interactive "P")
  (if P (insert vsq-sep)
    (insert " " vsq-sep)
    (newline-and-indent)))


(define-derived-mode vsq-mode sql-mode "vsq"
  "vsq-mode is an extension of sql-mode,
containing additional commands only applicable for  vsq-files."
  (if (or (not sql-mode-menu) (lre-not-memb 'keys)) ()
    (easy-menu-add-item sql-mode-menu nil
			[ "Insert macro definition" vsq-define-macro
			  (not buffer-read-only) ]))
  (when (lre-memb-all 'imenu 'e21+)
    (setq imenu-generic-expression
	  (append (list '("*Macros*" 
			  "^#\\(define\\|undef\\)\\s-+\\([a-zA-Z0-9_|]+\\)" 2)
			'("*Includes*"
			  "^#include\\s-+.\\([^\n]+\\)." 1))
		  imenu-generic-expression)))
  (define-key vsq-mode-map [kp-enter] 'vsq-linefeed)
  (define-key vsq-mode-map "\C-cd" 'vsq-define-macro)
  (define-key vsq-mode-map [f12 S] 'sql-mode)
  (if (fboundp 'tplsub-register-mode)
      (tplsub-register-mode 'vsq-mode
			    tplsub-sql-tmpl-list
			    nil       ; no help
			    'default  ; template regexp
			    'default  ; case sens.
			    " : @@\\" ; cont string
			    'default  ; expansion key
			    )))

(provide 'vsq-mode)

;;; vsq-mode.el ends here
