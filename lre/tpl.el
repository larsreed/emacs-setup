§default Year (eval (int-to-string (nth 5 (decode-time))))
§default Author Lars Reed
§default MailTo (eval user-mail-address)
§default FileName (eval (file-name-nondirectory (buffer-file-name)))
§ask Desc1 File description - 1-liner
§ask FileName File name
;;; §FileName§  --- §Desc1§

§ask Year This year
§ask Author
;;; Copyright (C) §Year§ §Author§

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
§ask MailTo Mail address
;; Author:		(c) §Author§ <§MailTo§>
;; Version:		TODO 0.1 ???
§ask Keywords
;; Keywords:		§Keywords§
;; Adapted-By:
;;
;;; History:
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;; ========================================================================

§default Mode (eval (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
§ask Mode Mode name
(defvar §Mode§-submap nil
  "Keymap used with §Mode§.")

(defvar §Mode§-map nil
  "Keymap used with §Mode§.")

(defvar §Mode§-prefix-key "\C-cx"
  "*Prefix key for all §Mode§ commands.")

§ask ModeString Name for mode-line
(defconst §Mode§-name " §ModeString§"
  "Name used in mode-line.")

§ask ModeBuffer Temp.buffer name
(defconst XXX-buffer-name "*§ModeBuffer§*"
  "Name used for §Mode§ buffer.")

(defvar §Mode§ nil
  "Non-nil when §Mode§ is activated.")

(defvar §Mode§-hook nil
  "Functions to call after loading §Mode§.")

; ======================================================================

(if §Mode§-submap  ()
  (setq §Mode§-submap (make-sparse-keymap))
  (define-key §Mode§-submap "a" '§Mode§-a)
  (define-key §Mode§-submap "b" '§Mode§-b))

;; Set up main map, which leads via the prefix key to the submap.
(if §Mode§-map ()
  (setq §Mode§-map (make-sparse-keymap))
  (define-key §Mode§-map §Mode§-prefix-key §Mode§-submap))


;; Activate map
(or (assq '§Mode§ minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons '§Mode§ §Mode§-map)
                minor-mode-map-alist)))

;; Display name
(or (assq '§Mode§ minor-mode-alist)
    (setq minor-mode-alist
          (cons '(§Mode§ §Mode§-name)
                minor-mode-alist)))


; ======================================================================

;;;###autoload
(defun §Mode§ (arg)
  "Toggle §Mode§ minor mode.
With ARG, turn §Mode§ on iff arg is positive.
The mode, whose commands all have prefix \\[§Mode§-prefix-key],
...

\\{§Mode§-map}"
  (interactive "P")
  (make-local-variable '§Mode§)
  (setq §Mode§
	(if (null arg)
	    (not §Mode§)
	  (> (prefix-numeric-value arg) 0)))

  (force-mode-line-update)
  (if §Mode§
      (progn
	(run-hooks '§Mode§-hook))))

(provide '§Mode§)

;;; §FileName§ ends here
