;;;; Last Updated : 2012/06/06 12:55:05 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.


;(in-package :in-prescript)
;;====================================================================
;; DEF, DEFPROC
;;====================================================================
;; (def space name {,@body}) <=> (defproc space name () . body)

(defun have-var? (var-tag space)
  (member var-tag (:vars space)))

(defun push-var (var-tag var-body space)
  (pushnew var-tag (:vars space))
  (push-hash var-tag var-body (:dict space)))

;; Thank Mr.294 in [Intro] Common Lisp No.9 [Ten Thousand FAQ] at 2ch
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

(defmacro make-var-body (space name body)
  (with-gensyms (stream tmp-space)
    `(with-output-to-string (,stream)
       (let ((,tmp-space (copy-space ,space :oprd nil :dict nil)))
         (-> ,tmp-space ,@(trans-args body))
         (format ,stream "~&/~A~{ ~A~^~&~} def" ',name
                 (nreverse (:oprd ,tmp-space)))))))

(defmacro defun-var (var-name var-key)
  `(defun ,var-name (space &rest args)
     (if (not (have-var? ,var-key space))
         (error
          ,(format nil "Space ~~A does not have variable ~A." var-name)
          space)
         (progn
           (push (format nil ,(format nil "~~{~~A ~~}~A" var-name)
                         args)
                 (:oprd space))
           space))))

(defmacro def (space name &body body)
  (let ((var-key (as-key name)))
    (with-gensyms (s var-body)
      `(let* ((,s ,space) ;for (-> (make-space) (defvar name ...))
              (,var-body (make-var-body ,s ,name ,body)))
         (push-var ,var-key ,var-body ,s)
         (defun-var ,name ,var-key)
         (values ,s ',name)))))

;;--------------------------------------------------------------------
(defun have-proc? (proc-tag space)
  (member proc-tag (:procs space)))

(defun push-proc (proc-tag proc-body space)
  (pushnew proc-tag (:procs space))
  (push-hash proc-tag proc-body (:dict space)))

;; + local dict
(defmacro make-proc-body (space name args body)
  (with-gensyms (stream tmp-space)
    `(with-output-to-string (,stream)
       (let ((,tmp-space (copy-space ,space :oprd nil :dict nil)))
         (-> ,tmp-space
             ,@(trans-args
                (sublis (loop :for arg :in args :collect
                              (cons arg (prin1-to-string arg)))
                        body)))
         (format ,stream "/~(~A~) { %def" ',name)
         ,(when args
            `(format ,stream "~&~{ /~A exch def~}" ',(reverse args)))
         (format ,stream "~&~{  ~A~^~&~} } bind def" ;early name binding
                 (reverse (:oprd ,tmp-space)))))))

(defmacro defun-proc (proc-name proc-key)
  `(defun ,proc-name (space &rest args)
     (if (not (have-proc? ,proc-key space))
         (error
          ,(format nil "Space ~~A does not have procedure ~A." proc-name)
          space)
         (progn
           (push (format nil ,(format nil "~~{~~A ~~}~(~A~)" proc-name)
                         args)
                 (:oprd space))
           space))))

(defmacro defproc (space name (&rest args) &body body)
  (let ((proc-key (as-key name)))
    (with-gensyms (s proc-body)
      `(let* ((,s ,space) ;for (-> (make-space) (defproc name ...))
              (,proc-body (make-proc-body ,s ,name ,args ,body)))
         (push-proc ,proc-key ,proc-body ,s)
         (defun-proc ,name ,proc-key)
         (values ,s ',name)))))

;;====================================================================