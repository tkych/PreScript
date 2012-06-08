;;;; Last Updated : 2012/06/08 20:15:20 tkych


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-memo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; System for PreScript

;; Copyright (c) 2012 Takaya OCHIAI

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;====================================================================
;; PreScript: PostScript Interface for Common Lisp
;;====================================================================
;; PreScript/
;;   README.markdown
;;   prescript.asd
;;   internal-package.lisp
;;   api-package.lisp
;;   cl-utils.lisp
;;   src/
;;     space.lisp
;;     draw-ops.lisp
;;     def-ops.lisp
;;     control-ops.lisp
;;     basic-obj.lisp
;;     output.lisp
;;     ps-utils.lisp
;;   doc/
;;     index.org
;;     index.html
;;     index-ja.org
;;     index-ja.html
;;     images/

;;--------------------------------------------------------------------
(in-package :cl-user)
(defpackage #:prescript-asd (:use :cl :asdf))
(in-package #:prescript-asd)

(defsystem :prescript
  :description "Page Description DSL for Common Lisp"
  :name        "PreScript"
  :version     "0.0.~~ (experimental alpha)"
  :licence     "MIT licence"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :long-description
  "PreScript is Drawing & Typesetting DSL (or PostScript Interface) for Common Lisp.
It requires the PostScript Interpreter (for example, http://www.ghostscript.com/)."
  :depends-on (:trivial-shell)
  :serial t
  :components ((:file "internal-package")
               (:file "cl-utils")
               (:module "src"
                        :components ((:file "space")
                                     (:file "draw-ops")
                                     (:file "def-ops")
                                     (:file "control-ops")
                                     (:file "basic-objs")
                                     (:file "output")
                                     (:file "ps-utils")
                                     ))
               (:file "api-package")
               ))

;;====================================================================