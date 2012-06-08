;;;; Last Updated : 2012/06/08 20:03:16 tkych


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-memo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Basic Objects in PreScript 

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
;; BASIC OBJECTS
;;====================================================================
;; STRING:  $"string" -> "(string)"
;; FONT:    $Times-Roman -> "/Times-Roman"
;; ARRAY:   [ **** ] -> (any "[ ") **** (any " ]")
;; EXE-ARRAY: { **** } -> (any "{ ") **** (any " }")
;; DICTIONARY: <<

(in-package #:in-prescript)

;;--------------------------------------
;; SYMBOL(FONT), STRING
;;--------------------------------------
;; $"\"some-string\"" -> "(\"some-string\")"
;; $Helvetica-BoldOblique -> "/Helvetica-BoldOblique"
(set-macro-character #\$
  (^ (stream char)
     (declare (ignore char))
     (with-read-preserve-case
       (let ((x (read stream t nil t)))
         (etypecase x
           (symbol (format nil "/~A" (symbol-name x))) ;for ps-symbol
           (string (format nil "(~A)" x)))))))         ;for ps-string

;;--------------------------------------
;; ARRAY: [ ], { }
;;--------------------------------------
(set-macro-character #\[ (^ (stream char)
                            (declare (ignore stream char))
                            `(any "[ ")))

(set-macro-character #\[ (^ (stream char)
                            (declare (ignore stream char))
                            `(any " ]")))

(set-macro-character #\{ (^ (stream char)
                            (declare (ignore stream char))
                            `(any "{ ")))

(set-macro-character #\} (^ (stream char)
                            (declare (ignore stream char))
                            `(any " }")))

;;--------------------------------------
;; DICT: << >>
;;--------------------------------------



;;====================================================================