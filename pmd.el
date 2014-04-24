;;; pmd.el --- Major mode for pluging PMD into Emacs.

;; Author: John Russell <drjimmy42 at yahoo.com>
;; Maintainer: Nascif A. Abousalh-Neto <nascif at acm.org>
;; Created: Dec 30 2002
;; Version: $Revision: 0.4 $
;; Keywords: PMD major-mode

;; Copyright (C) 2002 John Russell
;; Copyright (C) 2003 Nascif A. Abousalh-Neto

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; Installation:
;;  1) Install the core PMD application (not part of this download, get it
;;     from http://pmd.sourceforge.net/);
;;  2) Make sure the directory of pmd.el is part of your Emacs load-path;
;; 	3) Add this line to your .emacs file:
;; 	    (autoload 'pmd-current-buffer "pmd" "PMD Mode" t)
;; 	    (autoload 'pmd-current-dir "pmd" "PMD Mode" t)
;;  4) Load pmd.el
;;  5) Configure the location of the Java executable and the PMD installation
;;     directory using the command pmd-customize.
;;
;; 	Once this is done, you can call the pmd-current-buffer and pmd-current-dir
;;  functions using M-x <function name> or by bind them to key sequences.

;; Configuration:
;; 	Besides the variables described above, you can also compile the rulesets used by PMD.
;;  Type:
;; 	    M-x pmd-customize
;; 		--or--
;; 	    M-x customize-group <RET>
;; 	    pmd <RET>

;; Description:
;; 	This mode uses a compilation buffer to display the output of PMD
;; (http://pmd.sourceforge.net/).
;; It provides two commands, pmd-current-buffer and pmd-current-dir, which as you
;; guessed run PMD on the contents of the current buffer (must be a .java file)
;; or on all .java files in the directory associated with the current buffer.
;; You can defining the PMD rulesets by customizing the variable pmd-ruleset-list.

;; Change History

;; 03/24/03 - 0.4: Nascif A. Abousalh-Neto
;; Update to align with PMD 1.04.
;; - Added new method, pmd-current-dir
;; - Removed variable pmd-version
;; - Changed customization: string variable pmd-rulesets replaced by list-based variable pmd-ruleset-list

;; 02/04/03 - 0.3: Nascif A. Abousalh-Neto
;; Updated the primary plugin with
;; contributions.  Note that these changes include an updated pmd-1.02.jar file
;; to support them.  These changes will be part of pmd-1.03 when it comes out;
;; this is just an updated release of pmd-1.02 to support this Emacs plugin.

;; 01/16/03 - 0.2 - John Russell
;; Completely rewritten

;; 06/06/02 - 0.1
;; First version of PMD for Emacs
;; Defined one function "pmd-current-buffer" which will
;; run PMD on the current buffer, and write the output
;; to *PMD*. Output format is in XML.  I will work in trying to
;; get a better, easier to read format for everyone.


(defgroup pmd nil "PMD"
  :group 'emacs)

(defcustom pmd-java-home "/usr/local/bin/java"
  "Java binary to run PMD with."
  :type 'file
  :group 'pmd )

(defcustom pmd-home "~/pmd"
  "Directory where PMD is installed."
  :type 'directory
  :group 'pmd)

(defcustom pmd-ruleset-list (list "rulesets/basic.xml")
  "A list of Rulesets to apply. Rulesets are specified in XML files in a subdirectory of the PMD install dir (typically called \"rulesets\")."
  :type '(repeat (file :tag "Ruleset"))
  :group 'pmd)

(defcustom pmd-append-output nil
  "If non-nil, append each output to the end of the *PMD* buffer,
else clear buffer each time."
  :type 'boolean
  :group 'pmd)

(defun pmd-mode ()
  "Plugin for PMD (http://pmd.sourceforge.net).  Opens a buffer which \
displays the output of PMD when run on the Java file in the current buffer.
Uses the standard compilation mode for navigation."

  (interactive)
  (kill-all-local-variables)
  (setq mode-name "PMD Mode"
	truncate-lines nil)
  (compilation-mode))

;;-------------------------
;;Inner workings
;;-------------------------

(defun pmd-help ()
  "Help for `pmd-mode'."
  (interactive)
  (describe-function 'pmd-mode))

;;-------------------------
;; Main functions
;;-------------------------


;;;###autoload
(defun pmd-customize ()
  "Customization of group `pmd' for Pmd-mode."
  (interactive)
  (customize-group "pmd"))

;;;###autoload
(defun pmd-current-buffer ()
  "Run PMD on the contents of the current buffer."
  (interactive)
  (if (string-equal (file-name-extension
                        (buffer-file-name)) "java")
         (pmd-file-or-dir (buffer-file-name))
    (message "Current buffer does not contain a Java file.  Aborting.")))

(defun pmd-current-dir ()
  "Run PMD on the contents of the current directory (recursively)."
  (interactive)
  (pmd-file-or-dir (file-name-directory (buffer-file-name))))

(defun pmd-classpath ()
  (let* ((path-separator (if (eq system-type 'windows-nt) ";" ":"))
         (path-slash     (if (eq system-type 'windows-nt) "\\" "/"))
         (pmd-lib     (concat pmd-home path-slash "lib" path-slash)))
    (mapconcat
     (lambda (path)
       path)
     (directory-files pmd-lib t "\\.jar$")
     path-separator)))

(defun pmd-file-or-dir (target)
  "Run PMD on the given target (file or dir)"
  (if (eq (count-windows) 1)
      (split-window-vertically))
  (other-window 1)
  (switch-to-buffer (get-buffer-create "*PMD*"))
  (if pmd-append-output
      (goto-char (point-max))
    (erase-buffer))
  (pmd-mode)
  (insert-string (concat " PMD output for " target "\n\n"))
  (insert (concat (shell-command-to-string
                   (concat pmd-java-home " -cp " (pmd-classpath) " net.sourceforge.pmd.PMD "
                           target " emacs " (mapconcat (lambda (path) path) pmd-ruleset-list ","))) "\n"))
  (insert-string "Done.\n")
  (goto-char (point-min)))


(provide 'pmd)
