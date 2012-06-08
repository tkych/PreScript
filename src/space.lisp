;;;; Last Updated : 2012/06/08 20:05:07 tkych


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-memo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; User-Space for PreScript

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
;; USER-SPACE
;;====================================================================
(in-package #:in-prescript)

(defclass user-space ()
     ((vars  :accessor :vars  :initarg :vars  :initform nil)
      (procs :accessor :procs :initarg :procs :initform nil)
      (oprd  :accessor :oprd  :initarg :oprd  :initform nil)
      (dict  :accessor :dict  :initarg :dict  :initform (make-ht))
      ))

(defun space? (x) (typep x 'user-space))

(defun make-space ()
  (make-inst 'user-space
             ))

(defun copy-space (space
                   &key (vars t) (procs t) (oprd t) (dict t))
  (make-inst 'user-space
             :vars  (if (eql t vars)  (copy-list (:vars space))  vars)
             :procs (if (eql t procs) (copy-list (:procs space)) procs)
             :oprd  (if (eql t oprd)  (copy-list (:oprd space))  oprd)
             :dict  (if (eql t dict)  (copy-hash (:dict space))  dict)
                         ))

(defun space-equal (s1 s2)
  (and (equal (:oprd  s1) (:oprd  s2))
       (equal (:vars  s1) (:vars  s2))
       (equal (:procs s1) (:procs s2))
       (equal (:dict  s1) (:dict  s2))))

;;====================================================================