;;;; Last Updated : 2012/06/08 20:06:18 tkych


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-memo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Control Operators in PreScript

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
;; Control-Operetors
;;====================================================================
;; ify ifelse loopy repeat fory forall

(in-package #:in-prescript)

;;--------------------------------------
;; (IFY space test procs) -> test {procs} if 
;; (ps-output (ify (make-space) (ne 1 2) (moveto 2 3)))
;; (ps-output (ify (make-space) (ne 1 2) (2 3)))
;; (ps-output (ify (make-space) (ne 1 2) 3))
;; (ps-output (ify (make-space) (ne 1 2) ((moveto 4 7) (moveto 2 3))))
;; (ps-output (ify (make-space) (ne 1 2) (moveto 4 7) (moveto 2 3)))
(defmacro ify (space test &rest procs)
  (with-gensyms (s)
    `(let ((,s ,space))
       (-> ,s ,@(trans-args (list test))
              (any-fstring "{ %if~&")
              ,@(trans-args procs)
              (any-fstring " } if")))))

;;--------------------------------------
;; (IFELSE space test procs1 procs2) -> test {procs1} {procs2} ifelse
;; (ps-output (ifelse (make-space) (ne 2 3)
;;                    (moveto 3 4)
;;                    (lineto 5 6)))
;; (ps-output (ifelse (make-space) (ne 2 3)
;;                    ((moveto 3 4) (moveto 3 3))
;;                    (lineto 5 6)))
(defmacro ifelse (space test procs1 procs2)
  (with-gensyms (s)
    `(let ((,s ,space))
       (-> ,s ,@(trans-args (list test))
              (any-fstring "{ %ifelse true~&")
              ,@(trans-args (list procs1))
              (any-fstring " }")
              (any-fstring "{ %ifelse false~&")
              ,@(trans-args (list procs2))
              (any-fstring " } ifelse")))))

;;--------------------------------------
;; (ps-output (loopy (make-space) (ne 2 3)
;;                   (moveto 3 4) (lineto 5 6)))
(defmacro loopy (space &rest procs)
  (with-gensyms (s)
    `(let ((,s ,space))
       (-> ,s (any-fstring "{ %loop~&")
              ,@(trans-args procs)
              (any-fstring " } loop")))))

;;--------------------------------------
;; (REPEAT space times procs) -> times {procs} repeat
;; (ps-output (repeat (make-space) 29 (moveto 3 4)))
;; (ps-output (repeat (make-space) 29 ((lineto 9 9) (moveto 3 4))))
;; (ps-output (repeat (make-space) (add 3 29) (moveto 3 4)))
(defmacro repeat (space n &rest procs)
  (with-gensyms (s)
    `(let ((,s ,space))
       (-> ,s ,@(trans-args (list n))
              (any-fstring "{ %repeat")
              ,@(trans-args procs)
              (any-fstring " } repeat")))))

;;--------------------------------------
;; (FORY space start step end proc1 proc2 ...)
;; -> start step end {proc1 proc2 ...} for
;; (ps-output (fory (make-space) 1 2 29 (moveto 3 4)))
;; (ps-output (fory (make-space) 1 (neg 2) (add 3 29) (moveto 3 4)))
;; !!start = procs??
(defmacro fory (space start step end &rest procs)
  (with-gensyms (s)
    `(let ((,s ,space))
       (-> ,s ,@(trans-args (list start step end))
              (any-fstring "{ %for")
              ,@(trans-args procs)
              (any-fstring " } for")))))


;;--------------------------------------
;; (forall space ary/dict/str proc1 proc2 ...)
;; -> ary/dict/str {proc1 proc2 ...} forall

;; (defmacro forall (space ary/dict/str &rest procs)


;;====================================================================