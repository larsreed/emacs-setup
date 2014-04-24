;;; wml-mode.el  --- Addtitions to html-mode for wml-files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:		Lars Reed <Lars@kalars.net>
;; Version:		1.2
;; Keywords:		WAP WML HTML
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

(require 'derived)

(defun wml-add-hyph ()
  "Add soft hyphen in WML"
  (interactive)
  (insert "&shy;"))

(defun wml-add-var ()
  "Add variable reference in WML"
  (interactive)
  (insert "$(")
  (save-excursion
    (insert ")")))


(define-derived-mode wml-mode html-mode "Wml"
  "wml-mode is an extension of html-mode,
containing additional commands only applicable for Wireless Markup Language."
  (define-key wml-mode-map [menu-bar wml]
    (cons "WML" (make-sparse-keymap "WML")))
  (define-key wml-mode-map [menu-bar wml hyph]
    '("Soft hyphen" . wml-add-hyph))
  (define-key wml-mode-map [menu-bar wml var]
    '("Variable reference" . wml-add-var))
  (define-key wml-mode-map "\C-c-" 'wml-add-hyph)
  (define-key wml-mode-map [?\C-c ?\M-4] 'wml-add-var)
  (if (fboundp 'tplsub-register-mode)
      (tplsub-register-mode 'wml-mode
			    tplsub-wml-tmpl-list
			    tplsub-wml-help-list
			    'default  ; template regexp
			    'default  ; case sens.
			    'default  ; cont string
			    'default  ; expansion key
			    )))

(provide 'wml-mode)

;; wml-mode.el ends here
