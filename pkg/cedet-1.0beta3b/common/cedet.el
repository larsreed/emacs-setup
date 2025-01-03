;;; cedet.el --- Setup CEDET environment

;; Copyright (C) 2002, 2003, 2004 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: CEDET developers <http://sf.net/projects/cedet>
;; Created: 09 Dec 2002
;; Keywords: syntax
;; X-RCS: $Id: cedet.el,v 1.11 2004/07/30 17:56:41 zappo Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library automatically setups your [X]Emacs to use CEDET tools.
;;
;; First download the latest CEDET distribution, provided in a
;; cedet-<VERSION>.tar.gz tarball, from the project page at:
;; <http://sf.net/projects/cedet>.
;;  
;; Unpack the tarball in a directory of your choice.  It will install
;; the following directory tree:
;;
;;   cedet
;;     |
;;     +- common
;;     |
;;     +- cogre
;;     |
;;     +- ede
;;     |
;;     +- eieio
;;     |
;;     +- semantic
;;     |
;;     +- speedbar
;;     |
;;     \- contrib
;;
;; Then, add the following into your ~/.emacs startup file:
;;
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;
;; If you want to turn on useful or all Semantic features by default,
;; respectively add:
;;
;;   (setq semantic-load-turn-useful-things-on t)
;; or
;;   (setq semantic-load-turn-everything-on t)
;;
;; before loading this file, like this:
;;
;;   (setq semantic-load-turn-useful-things-on t)
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;
;; That's it!
;;

;;; History:
;;

;;; Code:
(eval-when-compile
  (require 'cl))

(defconst cedet-version "1.0beta3"
  "Current version of CEDET.")

(defconst cedet-packages
  `(
    ;;PACKAGE   MIN-VERSION      INSTALLDIR
    (cedet      ,cedet-version   "common"  )
    (cogre      "0.4beta1"                 )
    (ede        "1.0beta3"                 )
    (eieio      "0.18beta1"                )
    (semantic   "2.0beta3"                 )
    (speedbar   "0.15beta1"                )
    (cedet-contrib "0.0"         "contrib" )
    )
  "Table of CEDET packages to install.")

;; This file must be in "<INSTALL-DIR>/cedet/common"!
(let ((default-directory
        (file-name-directory
         (or load-file-name (buffer-file-name)))))
  
  ;; Add "<INSTALL-DIR>/cedet/common" to `load-path'.
  (add-to-list 'load-path default-directory)
  (message "%S added to `load-path'" default-directory)
  ;; Require the inversion library.
  (require 'inversion)
  
  ;; Go up to the parent "<INSTALL-DIR>/cedet" directory.
  (let ((default-directory (expand-file-name ".."))
        package min-version installdir)

    ;; Add the CEDET packages subdirectories to the `load-path' if
    ;; necessary.
    (dolist (package-spec cedet-packages)
      (setq package     (nth 0 package-spec)
            min-version (nth 1 package-spec)
            installdir  (nth 2 package-spec))
      (when installdir
        (setq installdir (expand-file-name installdir)))
      (inversion-add-to-load-path package min-version installdir))

    ;; Then run every package setup.
    (dolist (package-spec cedet-packages)
      (setq package (nth 0 package-spec))
      (message "Setting up %s..." package)
      (condition-case err
          (progn
            (require (intern (format "%s-load" package)))
            (message "Setting up %s...done" package))
        (error
         (message "%s" (error-message-string err)))))
    ))

(provide 'cedet)

;;; cedet.el ends here
