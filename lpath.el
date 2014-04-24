;; Shut up.

;; Bind functions that are used but unique to one Emacs.
(defun maybe-fbind (args)
  (while args 
    (or (fboundp (car args))
	(fset (car args) 'ignore))
    (setq args (cdr args))))

;; We attempt to use `font.el'.
(maybe-fbind '(font-italic-p font-bold-p font-size font-family))

;; Mule features.
(maybe-fbind '(coding-system-list widget-coding-system-prompt-value))

;; Emacs 20.1 variable.
(defvar preloaded-file-list nil)

(if (string-match "XEmacs" emacs-version)
    (progn 
      ;; This stuff is used in the Emacs only part of the code.
      (defvar track-mouse nil)
      (defvar global-face-data nil)
      (maybe-fbind '(posn-point event-start x-popup-menu
		     facemenu-get-face window-at
		     coordinates-in-window-p compute-motion
		     x-defined-colors easy-menu-create-keymaps
		     set-font-family set-font-size read-event
		     internal-find-face internal-next-face-id
		     make-face-internal set-frame-face-alist
		     frame-face-alist facemenu-add-new-face
		     make-face-x-resource-internal event-end
		     event-basic-type))
      ;; This is defined in newer version of `custom.el'.
      (autoload 'customize-menu-create "cus-edit"))
  
  ;; This stuff is used in the XEmacs only part of the code.
  
  ;; This is part of bytecomp.el in 19.35:
  (put 'custom-declare-variable 'byte-hunk-handler
       'byte-compile-file-form-custom-declare-variable)
  (defun byte-compile-file-form-custom-declare-variable (form)
    (if (memq 'free-vars byte-compile-warnings)
	(setq byte-compile-bound-variables
	      (cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
    form)

  (defvar browse-url-browser-function nil)
  (defvar zmacs-region-stays nil)
  (maybe-fbind '(color-instance-rgb-components make-color-instance
	         color-instance-name specifier-instance device-type
		 device-class get-popup-menu-response event-object
		 x-defined-colors read-color add-submenu set-font-family
		 font-create-object set-font-size frame-device find-face
		 set-extent-property make-extent characterp display-error
		 make-glyph set-glyph-image set-glyph-property
		 event-glyph glyph-property make-gui-button face-property
		 set-face-property device-on-window-system-p 
		 button-press-event-p next-command-event glyphp
		 color-name extent-property extent-at extent-start-position
		 image-instance-file-name event-glyph-extent
		 button-release-event-p next-event mouse-event-p
		 event-point map-extents valid-image-instantiator-format-p
		 locate-file valid-instantiator-p set-extent-end-glyph
		 set-face-display-table read-color-completion-table)))

(setq load-path (cons "." load-path))
(require 'custom)
