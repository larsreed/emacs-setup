;;; jsp-html-helper-mode.el -- JSP add-on for html-helper-mode.el

;; Author: Ben Tindale <ben@bluesat.unsw.edu.au>
;; Maintainer: Ben Tindale <ben@bluesat.unsw.edu.au>
;; Created: 19 April 2000

;; LCD Archive Entry:
;; jsp-html-helper-mode|Ben Tindale|ben@bluesat.unsw.edu.au|
;; JSP add-on for html-helper-mode.el.|
;; 20-April-00|Version 0.1

;; Copyright (C) 2000 Ben Tindale

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
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;; Module to add jsp support to html-helper-mode.
;; This code must be called *after* html-helper-mode loads. It can be
;; run nicely from the html-helper-load-hook.

;;; Commentary:
;;{{{ 

;; Installation:
;;   Add this line in your .emacs:
;;    (add-hook 'html-helper-load-hook (function (lambda () (load "jsp-html-helper-mode.el"))))
;;    This code must be called *after* html-helper-mode loads.

;;
;; Description:
;;   Module to add jsp support to html-helper-mode.
;;   Supports: 
;;             Scriptlets eg <% ... %>
;;             Actions eg <%useBean ... />
;;             Directives eg <@ page import ... %>
;;             Expressions eg <%= ... %>
;;             Comments eg <%-- ... --%>

;; Thank yous:
;;   Nelson Minar for writing html-helper-mode.

;;}}}



;; C-c C-j is the prefix binding.
;; first, make html-helper-mode aware of the new type:
(html-helper-add-type-to-alist
 '(jsp-hhm . (my-jsp-hhm-map "\C-c\C-j" my-jsp-hhm-menu "Insert JSP Elements")))
;; now let's install the type into the keymap and menu:
(html-helper-install-type 'jsp-hhm)
;; finally, we need some tags:
(mapcar
 'html-helper-add-tag
 '(
   ;; Scriplets
   (jsp-hhm "s" "<% " "Scriplet" ("<% " p " %>"))
   ;; Actions
   (jsp-hhm "u" "<jsp:useBean" "Usebean" ("<jsp:useBean ID=\"" (p "Bean ID: ") "\" class=\"" (p "Bean class: ") "\" scope=\"" (p "Scope: ") "\"/>"))
   (jsp-hhm "p" "<jsp:setProperty" "setProperty" ("<jsp:setProperty name=\"" (p "Property name: ") "\" property=\"" (p "Property: ") "\" value=\"" (p "Value: ") "\"/>"))
   (jsp-hhm "g" "<jsp:getProperty" "getProperty" ("<jsp:getProperty name=\"" (p "Property name: ") "\" property=\"" (p "Property: ") "\" />"))
   (jsp-hhm "f" "<jsp:forward" "Forward" ("<jsp:forward page=\"" (p "Page: ") "\" />"))
   (jsp-hhm "d" "<jsp:include" "Include" ("<jsp:include page=\"" (p "Include: ") "\" />"))
   ;; Directives
   (jsp-hhm "i" "<@ page import=\"" "Import" ("<@ page import=\"" (p "Import classes: ") "\" errorPage=\"" (p "Error Page: ") "\" %>"))
   (jsp-hhm "l" "<@ page =\"" "Language" ("<@ page language=\"" (p "Language: ") "\" %>"))
   (jsp-hhm "t" "<@ page =\"" "IsThreadSafe" ("<@ page isThreadSafe=\"" (p "Toggle isThreadSafe (true|false): ") "\" %>"))
   ;; Expressions
   (jsp-hhm "e" "<%= " "Expression" ("<%= " p " %>"))
   ;; Comments
   (jsp-hhm "c" "<%-- " "Comment" ("<%-- " p " --%>"))
   ))

;; now rebuild the menu so all my new tags show up.
(html-helper-rebuild-menu)
