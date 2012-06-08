;;;; Last Updated : 2012/06/08 20:03:46 tkych


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-memo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Output Operators in PreScript

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
;; PS-OUTPUT, PS-VIEW
;;====================================================================
(in-package #:in-prescript)

(defun ps-output (space)
  (output-prolog space)
  (output-script space)
  (output-epilog space))

(defun output-prolog (space)
  (format t "~&%!PS-Adobe-3.0")
  (output-vars space)
  (format t "~%%%END PROLOG"))

(defun output-vars (space)
  (with-slots (vars dict) space
     (when vars
       (dolist (var (reverse vars))
         (let ((body (gethash var dict)))
           (format t "~%~A~%" (if (lazy? body)
                                  (let ((slender (diet body)))
                                    (push-hash var slender dict)
                                    slender)
                                  body)))))))

(defun output-script (space)
  (princ (with-output-to-string (s)
           (format s "~&~{~A~^~&~}" (reverse (:oprd space))))))

(defun output-epilog (space)
  (declare (ignore space))
  (format t "~%%%END"))

(defmacro make-ps-file (space &key (outfile "PRESCRIPT-TMP.ps")
                                   (show nil))
  (with-gensyms (s)
    `(progn
       ,(when show `(ps-output ,space))
       (with-open-file (,s ,outfile :direction :output
                           :if-exists :supersede)
         (let ((*standard-output* ,s))
           (ps-output ,space))))))

(defparameter *viewer* #+windows           "start"
                       #+(or macos macosx) "open"
                       #+linux             "xdg-open")

(defun show-image (ps-file)
  (trivial-shell:shell-command
   (format nil "~A ~A" *viewer* ps-file)))

(defmacro ps-view (space &key (outfile "PRESCRIPT-TMP.ps" file?))
  `(progn
     (make-ps-file ,space :outfile ,outfile :show nil)
     (show-image ,outfile)
     ,(when (not file?) `(delete-file ,outfile))))

;;====================================================================