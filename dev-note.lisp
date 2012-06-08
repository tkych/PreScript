;;;; Last Updated : 2012/06/08 20:02:51 tkych


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;====================================================================
;; Development Note
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
;;     output.lisp
;;     ps-utils.lisp
;;   doc/
;;     index.org
;;     index.html
;;     index-ja.org
;;     index-ja.html
;;     images/

(pushnew #P"/home/tkych/projects/PreScript/" asdf:*central-registry*)
(ql:quickload :prescript)

;;--------------------------------------------------------------------
;; PS-Utils
;;--------------------------------------------------------------------

(defop set-font (font scale)
  (findfont font)
  (scalefont scale)
  (setfont))
  
(defop fill-gray (gray-scale)
  (setgray gray-scale)
  (filly))


;;--------------------------------------------------------------------
;; Idea Memo
;;--------------------------------------------------------------------

(def 
increment ++ 

/counter 0 def
/counter ++
/counter ++
counter ==

;;--------------------------------------------------------------------
;; TODO
;;--------------------------------------------------------------------
;; defproc <- local-dict, local-gstate

;; t, nil -> true, false

;; bind def

;; in ->context, trans-args
(ps-view
 (-> (make-space)
     (def vpos 720)
     (def wordy $"Typefaces")
     (def choose-font {(findfont) (scalefont 15) (setfont)})
     (def newline {(def vpos (sub (vpos) 15))
          (moveto 72 (vpos))})
     (def print-word {(choose-font) (show (wordy)) (newline)})
     (vpos 72) (moveto)   ;;!!!!
     (print-word $Times-Roman)
     (print-word $Times-Bold)
     (print-word $Times-Italic)
     (print-word $Times-BoldItalic)
     (newline)))

;; Example 5.4, p.42 [Diamond Club]
;; stringwidth
;; string-width
(def rightshow {(rmoveto (sub (exch (popy (stringwidth (dup))) ;!!!
                                    120))
                         0)
                (show)})

;; Example 6.2, p.51 [Star]
;; (with-gstate
(ps-view
 (-> (make-space)
     (def starside {(rlineto 72 0)
          (translate (currentpoint))
          (rotate -144)})
     (def star {(moveto) (translate (currentpoint))
          (repeat 4 (starside))
          (closepath)
          (gsave)
          (setgray .5) (filly)
          (grestore)
          (stroke)})
     (star 200 200)
     (showpage)))

;; in defproc in defproc, + trans-args

;; (def space name {,@body}) <=> (defproc space name () . body)

;; Example 7.4, p.69-70 [Line of Circle]
;; call var-name that define in def
(ps-view
 (-> (make-space)
     (def pagewidth (mul 8.5 72))
     (def doCircle {(stroke (arc (xpos) (ypos) (radius) 0 360))}) ;!!!
     (def inc-x {(def xpos (add (xpos) (radius)))})
     (def lineofcircle {(def ypos (exch))
          (def radius (exch))
          (def xpos 0)
          (loopy (ifelse (le (xpos) (pagewidth))
                         (inc-x (doCircle)) ;!!!
                         (exit)))})
     (lineofcircle 10 400)
     (showpage)))


;; ps op でスタックが陽に現れないユーティリティを定義
;; スタックを陽に用いるパターンを収集する 

;; //はletで

;; <hex bytes>

;; escape: \), \\

(ps-view
 (-> (make-space)
     (fory 0 18 (mul 18 24)
           (moveto (dup) 0)
           (lineto 600))
     (stroke)
     (fory 0 18 (mul 18 36)
           (moveto (dup) (exch 0))
           (lineto (exch 436)))
     (stroke)))

(ps-view
 (-> (make-space)
     (fory i 0 18 (mul 18 24)
           (moveto i 0)
           (lineto i 600))
     (stroke)
     (fory j 0 18 (mul 18 36)
           (moveto 0 j)
           (lineto 436 j))
     (stroke)))
  

  0 18 18 24 mul { %for
          dup 0 moveto
          600 lineto
  } for stroke
  % 18-pt spacing, 36 lines
  0 18 18 36 mul { %for
          dup 0 exch moveto
          436 exch lineto
  } for stroke

;;--------------------------------------------------------------------
;; Scratch
;;--------------------------------------------------------------------


;;--------------------------------------------------------------------

;; Example 7.4, p.69-70 [Line of Circle]
;; call fn before defun 
;;   -> lazy-body

(ps-view
 (-> (make-space)
     (def pagewidth (mul 8.5 72))
     (def doCircle {(stroke (arc (xpos) (ypos) (radius) 0 360))}) ;!!!
     (def inc-x {(def xpos (add (xpos) (radius)))})
     (def lineofcircle {(def ypos (exch))
          (def radius (exch))
          (def xpos 0)
          (loopy (ifelse (le (xpos) (pagewidth))
                         (inc-x (doCircle)) ;!!!
                         (exit)))})
     (lineofcircle 10 400)
     (lineofcircle 30 400)
     (lineofcircle 90 400)
     (showpage)))

(defun push-var (var-tag var-body space)
  (pushnew var-tag (:vars space))
  (push-hash var-tag var-body (:dict space)))

(defmacro def (space name &body body)
  (let ((var-key (as-key name)))
    (with-gensyms (s)
      `(let ((,s ,space)) ;for (-> (make-space) (defvar name ...))
         (push-var ,var-key ,(make-var-body ,s ,name ,body) ,s)
         (defun-var ,name ,var-key)
         (values ,s ',name)))))

(defmacro make-var-body (space name body)
  (with-gensyms (stream tmp-space)
    `(with-output-to-string (,stream)
       (let ((,tmp-space (copy-space ,space :oprd nil :dict nil)))
         (-> ,tmp-space ,@(trans-args body))
         (format ,stream "~&/~A~{ ~A~^~&~} def" ',name
                 (nreverse (:oprd ,tmp-space)))))))

;;--------------------------------------
(defmacro make-lazy-body (space name body)
  (with-gensyms (stream tmp-space)
    `(cons :lazy
           (^ () (with-output-to-string (,stream)
                   (let ((,tmp-space
                          (copy-space ,space :oprd nil :dict nil)))
                     (-> ,tmp-space ,@(trans-args body))
                     (format ,stream "~&/~A~{ ~A~^~&~} def" ',name
                             (nreverse (:oprd ,tmp-space)))))))))

(defun lazy? (x) (and (consp x) (eql :lazy (car x))))
(defun diet (lazy-body) (funcall (cdr lazy-body)))

(defmacro def (space name &body body)
  (let ((var-key (as-key name)))
    (with-gensyms (s)
      `(let ((,s ,space)) ;for (-> (make-space) (defvar name ...))
         (push-var ,var-key (make-lazy-body ,s ,name ,body) ,s)
         (defun-var ,name ,var-key)
         (values ,s ',name)))))


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


;;--------------------------------------------------------------------
;; $"\"some-string\"" -> "(\"some-string\")"
;; $Helvetica-BoldOblique -> "/Helvetica-BoldOblique"
(set-macro-character #\$
  (^ (stream char)
     (declare (ignore char))
     (with-read-preserve-case
       (let ((x (read stream t nil t)))
         (etypecase x
           (symbol (format nil "/~A" (symbol-name x))) ;for ps-quote
           (string (format nil "(~A)" x)))))))         ;for ps-string


;;--------------------------------------------------------------------
(defun trans-args (forms)
  (labels ((rec (form op args result)
             (if (null form)
                 (cons (cons op (nreverse args)) result)
                 (if (atom form)        ;for (trans-args '(2))
                     (cons `(any ,form) result)
                     (dbind (x . xs) form
                       (etypecase x
                         (symbol
                          (case x
                            (def (rec (append1 (cdr xs) "def")
                                      'any
                                      (cons (format nil "/~A" (1st xs)) args)
                                      result))
                            ((ify ifelse loopy repeat fory forall)
                             (cons form result))
                            (t (rec xs x args result))))
                         (string (rec xs op (cons x args) result))
                         (number (rec xs op (cons x args) result))
                         (list   (rec xs op nil
                                      (rec x op args result)))))))))
    (mappend (^ (x) (nreverse (rec x nil nil nil)))
             forms)))

;;--------------------------------------------------------------------

= wanted =>
((any "{ ")
 (any "/VPOS") (vpos) (sub 15) (any "def")
 (vpos 72) (moveto) (any " }"))


(trans-args '({(def vpos (sub (vpos) 15))
                   (moveto 72 (vpos))}))
->
(trans-args '((any "{ ")
              (def vpos (sub (vpos) 15))
              (moveto 72 (vpos)) (any " }")))
=>
((any "{ ")
 (vpos) (sub 15) (vpos)
 (vpos 72) (moveto) (any " }"))

= wanted =>
((any "{ ")
 (any "/VPOS") (vpos) (sub 15) (any "def")
 (vpos 72) (moveto) (any " }"))



(ps-output
 (-> (make-space)
     (def vpos 720)
     (def newline {(def vpos (sub (vpos) 15))
                   (moveto 72 (vpos))})))
=>
/VPOS 720 def
/NEWLINE {
 VPOS 15 sub VPOS
 72 VPOS moveto  } def

= wanted =>
/VPOS 720 def
/NEWLINE {
 /VPOS VPOS 15 sub def
 72 VPOS moveto} def

= wanted =>
((any "{ ") (any "/VPOS") (vpos) (sub 15) (any "def")
 (vpos 72) (moveto) (any " }"))





;;====================================================================
;; PS-PARSER
;;====================================================================

(defun escape-elt (elt)
  (cond ((null       elt) nil)
        ((eql 'false elt) " false")
        ((eql 'true  elt) " true")
        ((stringp    elt) (format nil " (~A)" elt))
        ((ary?       elt) (str "[" (escape-elt elt) "]"))
        ((exe-ary?   elt) (str "{" (escape-elt elt) "}"))
        ((numberp    elt) (format nil " ~A" elt))
        ((space?     elt) elt)
        ((token?     elt) (format nil " /~(~A~)" elt))
        (t                (error "~A is not PreScript type." elt))))


;;====================================================================