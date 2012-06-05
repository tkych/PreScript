;;;; Last Updated : 2012/06/05 16:54:46 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.


;(in-package :in-prescript)
;;====================================================================
;; Drawing Operators
;;====================================================================
(defmacro def-ps-op (name)
  `(defun ,name (space &rest args)
     (push (format nil ,(format nil "~~{~~A ~~}~(~A~)" name) args)
           (:oprd space))
     space))

(defmacro defops (def-type &rest names)
  `(progn
     ,@(mapcar (^ (name) `(,def-type ,name))
               names)))

;; [ ] { } << >>
(defops def-ps-op
    getinterval transform itransform dtransform concatematrix
    exec stop start pathbbox flattenpath currentfont
    findfont setdash makefont charpath
    currentfile readhexstring cvx cvlit
    currenttransfer settransfer arc arcn arcto curveto scale
    sub add div idiv mul translate moveto rmoveto lineto rlineto
    show ashow kshow widthshow awidthshow stringwidth
    matrix currentmatrix setmatrix
    clear dup exch roll aload astore mark
    neg == pstack
    ne gt ge lt le cvs exit dict begin end true false
    newpath stroke showpage closepath setlinewidth setlinecap   
    setlinejoin setmiterlimit setrgbcolor setcmykcolor 
    setgray rotate newline
    currentpoint gsave grestore clip eoclip initclip scalefont
    setfont
    )

(defmacro def-ps-opy (name)
  (let ((namey (intern (format nil "~AY" name))))
    `(defun ,namey (space &rest args)
       (push (format nil ,(format nil "~~{~~A ~~}~(~A~)" name) args)
             (:oprd space))
       space)))

(defops def-ps-opy
    eq mod fill pop string length array sin cos atan sqrt
    and or not read null exp log abs celling floor round truncate
    load quit
    )

(defun any (space &rest args)
  (push (format nil "~&~{~A~^ ~}" args) (:oprd space))
  space)

(defun any-fstring (space fstring &rest args)
  (push (apply #'format nil fstring args) (:oprd space))
  space)

;;====================================================================