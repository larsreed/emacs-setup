;;; ede-proj-aux.el --- EDE Generic Project auxilliary file support

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-aux.el,v 1.6 2000/09/24 15:33:08 zappo Exp $

;; This software is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Handle auxiliary files (README, FAQ, etc) in and EDE Project file.

(require 'ede-proj)
(require 'ede-pmake)

;;; Code:
(defclass ede-proj-target-aux (ede-proj-target)
  ((sourcetype :initform (ede-aux-source)))
  "This target consists of aux files such as READMEs and COPYING.")

(defvar ede-aux-source
  (ede-sourcecode "ede-aux-source"
		  :name "Auxiliary"
		  :sourcepattern "^[A-Z]+$\\|\\.txt$")
  "Miscelaneous fiels definition.")

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-aux))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_AUX"))

(provide 'ede-proj-aux)

;;; ede-proj-aux.el ends here
