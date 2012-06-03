;;;; Last Updated : 2012/06/02 15:33:18 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.
 

;;--------------------------------------------------------------------
;; Memo
;;--------------------------------------------------------------------

;; (push #P"/home/tkych/projects/PreScript/" asdf:*central-registry*)
;; (ql:quickload :prescript)

;; defproc <- local-dict, local-gstate



;;====================================================================
;; PreScript: Lispized PostScript
;;====================================================================

;; PreScript/
;;   README.markdown
;;   prescript.asd
;;   in-package.lisp
;;   api-package.lisp
;;   cl-utils.lisp
;;   src/
;;     space.lisp
;;     draw-ops.lisp
;;     def-ops.lisp
;;     control-ops.lisp
;;     output.lisp
;;     prescript-utils.lisp
;;   doc/
;;     index.org
;;     index.html
;;     index-ja.org
;;     index-ja.html
;;     images/


;;====================================================================
;; CL-UTILS
;;====================================================================
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ppcre)
  (ql:quickload :trivial-shell))

(defun as-key (sym) (intern (symbol-name sym) :keyword))

;; (defmacro -> (x &rest form)
;;   (cond ((null    form) x)
;;         ((single? form) (let ((first (1st form)))
;;                           (if (consp first)
;;                               `(,(1st first) ,x ,@(rest first))
;;                               `(,first ,x))))
;;         (t `(-> (-> ,x ,(1st form)) ,@(rest form)))))


;;====================================================================
;; USER-SPACE
;;====================================================================
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
  
(defun copy-space (space &rest copy-slots)
  (if (not copy-slots)
      (with-slots (vars procs oprd dict) space
         (make-inst 'user-space :vars  (copy-list vars)
                    :procs (copy-list procs)
                    :oprd  (copy-list oprd)
                    :dict  (copy-hash dict)))
      (make-inst 'user-space :vars  (copy-list vars)
                 :procs (copy-list procs)
                 :oprd  (copy-list oprd)
                 :dict  (copy-hash dict))))

(defun copy-space (space &optional
                         (slots '(:vars :procs :oprd :dict) slotts?))
  (with-slots (vars procs oprd dict) space
     (make-inst 'user-space :vars  (copy-list vars)
                            :procs (copy-list procs)
                            :oprd  (copy-list oprd)
                            :dict  (copy-hash dict))))

(defun space-equal (s1 s2)
  (and (equal (:oprd  s1) (:oprd  s2))
       (equal (:vars  s1) (:vars  s2))
       (equal (:procs s1) (:procs s2))
       (equal (:dict  s1) (:dict  s2))))

;;====================================================================


;;====================================================================
;; OPERATORS
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


;;====================================================================
;; BASIC OBJECT
;;====================================================================
;; STRING:  #"string" -> "(string)"
;; FONT:    $Times-Roman -> "/Times-Roman"
;; ARRAY:   ['(1 1) 'ss "ahaha" (moveto 2 (neg 3))]
;;          -> []
;; DICTIONARY: 

;;--------------------------------------
;; STRING
;;--------------------------------------
;; #"some-string" ~> "(some-string)"
(defun ps-string-reader (input-stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((acc nil))
    (do ((char (read-char input-stream) (read-char input-stream))
         (pre nil char))
        ((and (char= char #\")
              (char/= pre #\\)))  ;for #"\"string\""
      (push char acc))
    (ppcre:regex-replace-all      ;"(\\\"string\\\")" -> "(\"string\")"
     "\\\\\"" (format nil "(~A)" (coerce (nreverse acc) 'string))
     "\"" :preserve-case t)))

(set-dispatch-macro-character #\# #\" #'ps-string-reader)


;;--------------------------------------
;; FONT
;;--------------------------------------
;; $Times-Roman ~> "/Times-Roman"
(set-macro-character #\$
  (^ (stream char)
     (declare (ignore char))
     (format nil "/~{~@(~A~)~^-~}"
             (ppcre:split #\- (symbol-name (read stream t nil t))))))

;;--------------------------------------
;; ARRAY: [], {}
;;--------------------------------------

;; ([] space (moveto 2 3) (lineto 30 40))
;; ->
;; (-> space
;;     (any "[ ")
;;     (moveto 2 3) (lineto 30 40)
;;     (any " ]"))


;; ([] (make-space) (moveto 2 3) (lineto 30 40))
(defmacro [] (space &rest args)
  (with-gensyms (s)
    `(let ((,s ,space))
       (-> ,s (any "[ ") ,@(trans-args args) (any " ]")))))

(defclass ps-ary ()
     ((cont :accessor :cont :initarg :cont :initform nil)))

(defclass ary (ps-ary) ())
(defun ary? (x) (typep x 'ary))

(defclass exe-ary (ps-ary) ())
(defun exe-ary? (x) (typep x 'exe-ary))

(defun token? (x) (and (atom x) (not (numberp x))))

;; ;; ({} (make-space) (moveto 2 3) (lineto 30 40))
;; (defmacro {} (space &rest args)
;;   (with-gensyms (s)
;;     `(let ((,s ,space))
;;        (-> ,s (any "{ ") ,@(trans-args args) (any " }")))))


;; ;; {'(1 1) 'ss "ahaha" (moveto 2 3) (lineto 4 4)}
;; ;; => { [1 1] /ss (ahaha) 2 3 moveto 4 4 lineto}

;; ;; {'(1 1) 'ss "ahaha" (moveto 2 3) (lineto 4 4)}
;; ;; -> (any "{") (any "[1 1]") (any "/ss") (moveto 2 3) (lineto 4 4) (any "}")
;; (defun exe-ary-reader (input-stream macro-char)
;;   (declare (ignore macro-char))
;;   (list '(any " {")
;;         (loop :for elt :in (read-delimited-list #\} input-stream nil)
;;               :if (consp elt) :collect elt
;;               :else :collect `(any ,(escape-elt elt)))
;;         '(any " {")))

;; (set-macro-character #\{ #'exe-ary-reader)
;; (set-macro-character #\} (get-macro-character #\)))


;;--------------------------------------------------------------------



;;--------------------------------------------------------------------

;; (ps-parser1 1 2 #(3 4 #("og \"u" 'mog)) 'aAa (moveto 6 7) "agaga")
;; => 6 7 moveto 1 2 [ 3 4 [ (og "u) /mog ] ] /aaa (agaga)
;; (defun ps-parser1 (&rest objs)
;;   (mapc (^ (obj)
;;              (cond ((null        obj)  nil)
;;                    ((eql 'false  obj) (any " false"))
;;                    ((eql 'true   obj) (any " true"))
;;                    ((stringp     obj) (any " (~A)" obj))
;;                    ((arrayp      obj) (ps-ary-parser obj))
;;                    ((sym-in-ary? obj) (any " /~(~A~)" (2nd obj)))
;;                    ((token?      obj) (any " /~(~A~)" obj))
;;                    ((numberp     obj) (any " ~A" obj))
;;                    (t obj)))
;;         objs)
;;   nil)


;; ({} (make-space) (moveto 2 3) (lineto 30 40))
;; (defmacro {} (space &rest args)
;;   (with-gensyms (s)
;;     `(let ((,s ,space))
;;        (-> ,s (any "{ ") ,@(trans-args args) (any " }")))))


;; {'(1 1) 'ss "ahaha" (moveto 2 3) (lineto 4 4)}
;; => { [1 1] /ss (ahaha) 2 3 moveto 4 4 lineto}
;; (defun ps-ex-ary-reader (input-stream macro-char)
;;   (declare (ignore macro-char))
;;   `(progn (fout " {")
;;           (ps-parser2 ,@(read-delimited-list #\} input-stream nil))
;;           (fout "}")))

;; (set-macro-character #\{ #'ps-ex-ary-reader)
;; (set-macro-character #\} (get-macro-character #\)))


;;; #"Times-Roman"
;;; -> (FOUT "~&/~A" "Times-Roman")
;;; => /Times-Roman
;; (defun font-reader (input-stream sub-char numarg)
;;   (declare (ignore sub-char numarg))
;;   (let ((font nil))
;;     (do ((char (read-char input-stream) (read-char input-stream)))
;;         ((char= char #\"))
;;       (push char font))
;;     `(fout "~&/~A" ,(coerce (nreverse font) 'string))))

;; (set-dispatch-macro-character #\# #\" #'font-reader)



;;====================================================================



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


;; Thank Mr.294 in [Intro] Common Lisp No.9 [Ten Thousand FAQ] at 2ch
(defun trans-args (forms)
  (labels ((rec (form op args result)
             (if (null form)
                 (cons (cons (if op op 'any) (nreverse args))
                       result)
                 (if (atom form)        ;for (trans-args '(2))
                     (cons (list 'any form) result)
                     (dbind (x . xs) form
                       (etypecase x
                         (symbol (rec xs x args result))
                         (string (rec xs op (cons x args) result))
                         (number (rec xs op (cons x args) result))
                         (list   (rec xs op nil
                                      (rec x op args result)))))))))
    (mapcan (^ (x) (nreverse (rec x nil nil nil)))
            forms)))


;;====================================================================


;;====================================================================
;; DEF, DEFPROC
;;====================================================================
;; (def space name {,@body}) <=> (defproc space name () . body)

(defun have-var? (var-tag space)
  (member var-tag (:vars space)))

(defun push-var (var-tag var-body space)
  (pushnew var-tag (:vars space))
  (push-hash var-tag var-body (:dict space)))

(defmacro make-var-body (name body)
  (with-gensyms (s tmp-space)
    `(with-output-to-string (,s)
       (let ((,tmp-space (make-space)))
         (-> ,tmp-space ,@(trans-args body))
         (format ,s "~&/~A~{ ~A~^~&~} def" ',name
                 (nreverse (:oprd ,tmp-space)))))))

(defmacro defun-var (var-name var-key)
  `(defun ,var-name (space)
     (if (not (have-var? ,var-key space))
         (error
          ,(format nil "Space ~~A does not have variable ~A." var-name)
          space)
         (progn (push ,(format nil "~(~A~)" var-name)
                      (:oprd space))
                space))))

(defmacro def (space name &body body)
  (let ((var-key (as-key name)))
    (with-gensyms (s var-body)
      `(let ((,s ,space) ;for (-> (make-space) (defvar name ...))
             (,var-body (make-var-body ,name ,body)))
         (push-var ,var-key ,var-body ,s)
         (defun-var ,name ,var-key)
         (values ,s ',name)))))

;; (defmacro def (space name &body body)
;;   (with-gensyms (s)
;;     `(let ((,s ,space))
;;        (-> ,s
;;            (any-fstring "/~A" ',name)
;;            ,@(loop :for elt :in body
;;                    :if (consp elt) :collect elt
;;                    :else :collect `(any ,(escape-elt elt)))))))

;;--------------------------------------------------------------------
(defun have-proc? (proc-tag space)
  (member proc-tag (:procs space)))

(defun push-proc (proc-tag proc-body space)
  (pushnew proc-tag (:procs space))
  (push-hash proc-tag proc-body (:dict space)))


(defmacro make-proc-body (name args body)
  (with-gensyms (s tmp-space)
    `(with-output-to-string (,s)
       (let ((,tmp-space (make-space)))
         (-> ,tmp-space
             ,@(trans-args
                (sublis (loop :for arg :in args
                              :collect (cons arg (prin1-to-string arg)))
                        body)))
         (format ,s "/~(~A~) { %def" ',name)
         ,(when args
            `(format ,s "~&~{ /~A exch def~}" ',(nreverse args)))
         (format ,s "~&~{  ~A~^~&~} } bind def" ;for early name binding
                 (nreverse (:oprd ,tmp-space)))))))

(defmacro defun-proc (proc-name proc-key)
  `(defun ,proc-name (space &rest args)
     (if (not (have-proc? ,proc-key space))
         (error
          ,(format nil "Space ~~A does not have ps-proc ~A." proc-name)
          space)
         (progn
           (push (format nil ,(format nil "~~{~~A ~~}~(~A~)" proc-name)
                         args)
                 (:oprd space))
           space))))

(defmacro defproc (space name (&rest args) &body body)
  (let ((proc-key (as-key name)))
    (with-gensyms (s proc-body)
      `(let ((,s ,space) ;for (-> (make-space) (defproc name ...))
             (,proc-body (make-proc-body ,name ,args ,body)))
         (push-proc ,proc-key ,proc-body ,s)
         (defun-proc ,name ,proc-key)
         (values ,s ',name)))))

;;====================================================================


;;====================================================================
;; CONTROL-OPERETORS: ify ifelse loopy repeat fory forall
;;====================================================================

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


;;====================================================================
;; PS-OUTPUT, PS-VIEW
;;====================================================================
(defun ps-output (space)
  (output-prolog space)
  (output-script space)
  (output-epilog space))

(defun output-prolog (space)
  (format t "~&%!PS-Adobe-3.0")
  (output-vars-procs space)
  (format t "~%%%END PROLOG"))

(defun output-vars-procs (space)
  (with-slots (vars procs dict) space
    (when vars
      (format t "~%%%------------------- Variable~P -------------------"
              (length vars))
      (dolist (var vars) (format t "~%~A~%" (gethash var dict))))
    (when procs
      (format t "~%%%------------------- Procedure~P ------------------"
              (length procs))
      (dolist (proc procs) (format t "~%~A~%" (gethash proc dict))))))


(defun output-script (space)
  (format t "~%%%------------------- Script ---------------------~%")
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


;;====================================================================
;; PreScript-UTILS
;;====================================================================
(defmacro defop (name args &body body)
  `(defun ,name (space ,@args)
     (-> space
         ,@body)))

;;====================================================================
