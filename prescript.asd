;;;; Last Updated : 2012/06/06 20:22:34 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.


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
  :description "Graphic and Typsetting DSL for Common Lisp"
  :name        "Donuts"
  :version     "0.0.~~ (experimental alpha)"
  :licence     "MIT licence"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :long-description
  "PreScript is PostScript interface for common lisp.
It requires the PostScript Interpreter (http://www)."
  :depends-on (:cl-ppcre :trivial-shell)
  :serial t
  :components (;(:file "internal-package")
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
               ;(:file "api-package")
               ))

;;====================================================================