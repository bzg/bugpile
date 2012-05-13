;;; iorg.el --- A webframework based on Org-mode and elnode

;; Programming interactive web-applications with Org-mode 
;; Copyright (C) 2012  Thorsten Jolitz 
;; [not yet: Free Software Foundation, Inc.]
;;
;; Author: Thorsten Jolitz <tjolitz at gmail dot com>
;; Co-Author: Eric Schulte  <... at...dot...>
;; Co-Author: Nic Ferrier  <... at...dot...>
;; Keywords: web-applications, interactive, Org-mode, elnode, dvcs
;; Homepage:
;; http://orgmode.org/worg/org-contrib/gsoc2012/student-projects/bugpile/i.org
;;
;; This file is [not yet!] part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; iOrg is a framework for developing web-applications with GNU Emacs
;; Org-mode, using Emacs and its libraries (Org-mode, elnode, VC, etc)
;; as development environment as well as runtime environment. 
;;
;; iOrg enables declarative programming of dynamic web-applications. For
;; basic functionality, the programmer does not need to write any Emacs 
;; Lisp code, but rather specifies the application within .org files, 
;; using Org-mode syntax and a default directory structure, and then 
;; exports the application and starts the web-server with Org-mode commands. 
;; 
;; For application specific functionality, Emacs Lisp libraries may be added, 
;; with functions that handle the http-requests ('handlers') and functions that
;; implement the actual application logic ('workers'). While the 'handlers' are
;; only useful for web-frontends of iOrg applications, the 'workers' contain 
;; essential application logic independent from the kind of frontend. They are
;; needed too when power users access the application directly via Emacs. 
;;
;; Installation and Activation
;; ---------------------------
;; See the [not yet existing!] iOrg manual at
;;
;;   http://orgmode.org/org.html#iOrg    ???
;;
;; Documentation
;; -------------
;; The documentation of iOrg can be found ????.  The Org-mode
;; distribution also contains a PDF version of it. At Worg, the Org-mode
;; community page, you can read the same text online as HTML. There is also an
;; extended tutorial on Worg, showing how to approach the development of
;; web-applications with iOrg in a systematic way (using a bugtracker as example
;; application). 
;; 
;; A list of recent changes can be found at
;; http://orgmode.org/????
;;
;;; Code:

;;;; Require other packages

;; (eval-when-compile
;;   (require 'gnus-sum))

(require 'org)

;;;; Other stuff we need.
;; (unless (fboundp 'time-subtract) (defalias 'time-subtract 'subtract-time))

;; (declare-function org-inlinetask-at-task-p "org-inlinetask" ())
;; (declare-function org-inlinetask-outline-regexp "org-inlinetask" ())


;; Customization variables
;; Define the iOrg-mode
;; ...
;; Key bindings
;; Documentation
;; Miscellaneous stuff
;; Integration with and fixes for other packages
;; Experimental code
;; Finish up
