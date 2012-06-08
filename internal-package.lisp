;;;; Last Updated : 2012/06/08 21:31:23 tkych

;; Internal Package for PreScript

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
;; Internal Package
;;====================================================================
(in-package :cl-user)

(defpackage #:in-prescript
  (:use :cl)
  (:export :make-space :space? :copy-space :space-equal
           :def :defproc
           :ify :ifelse :loopy :repeat :fory ;:forall
           ;:$ :[ :] :{ :}
           :ps-output :make-ps-file :ps-view
           :defop
           :->

           :getinterval :transform :itransform :dtransform :concatematrix
           :exec :stop :start :pathbbox :flattenpath :currentfont
           :findfont :setdash :makefont :charpath
           :currentfile :readhexstring :cvx :cvlit
           :currenttransfer :settransfer :arc :arcn :arcto :curveto :scale
           :sub :add :div :idiv :mul :translate :moveto :rmoveto :lineto :rlineto
           :show :ashow :kshow :widthshow :awidthshow :stringwidth
           :matrix :currentmatrix :setmatrix
           :clear :dup :exch :roll :aload :astore :mark
           :neg :== :pstack
           :ne :gt :ge :lt :le :cvs :exit :dict :begin :end :true :false
           :newpath :stroke :showpage :closepath :setlinewidth :setlinecap
           :setlinejoin :setmiterlimit :setrgbcolor :setcmykcolor
           :setgray :rotate :newline
           :currentpoint :gsave :grestore :clip :eoclip :initclip :scalefont
           :setfont

           :eqy :mody :filly :popy :stringy :lengthy :arrayy :siny :cosy :atany :sqrty
           :andy :ory :noty :ready :nully :expy :logy :absy :cellingy
           :floory :roundy :truncatey :loady :quit

           :any :any-fstring
           )
  )

;;====================================================================