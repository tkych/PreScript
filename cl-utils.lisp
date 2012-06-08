;;;; Last Updated : 2012/06/08 20:17:26 tkych


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-memo.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Common Lisp Utils for PreScript


;;====================================================================
;; Common Lisp Utilities
;;====================================================================
(in-package #:in-prescript)

(defmacro another-name (alias name)
  (cond ((special-operator-p name) `(defmacro ,alias (&rest args)
                                     `(,',name ,@args)))
        ((macro-function name)     `(setf (macro-function ',alias)
                                          (macro-function ',name)))
        ((fboundp name)            `(setf (symbol-function ',alias)
                                          (function ,name)))
        (t (error "The name, ~A is not binding to a special-operator, macro, or function."
                name))))

;;; On Lisp
(eval-when (:compile-toplevel :load-toplevel)
  (defun group (n lst)
    (if (zerop n) (error "zero length"))
    (labels ((rec (lst acc)
               (let ((rest (nthcdr n lst)))
                 (if (consp rest)
                     (rec rest (cons (subseq lst 0 n)
                                     acc))
                     (nreverse (cons lst acc))))))
      (if lst (rec lst nil) nil))))

(defmacro another-names (&rest names)
  `(progn ,@(mapcar (lambda (pair) `(another-name ,@pair))
                    (group 2 names))))

(another-names  1st  first
                2nd  second
                3rd  third
                make-ht  make-hash-table
                make-inst make-instance
                dbind  destructuring-bind)

;;; ^ -> lambda
;;; !!! Don't use 1st place. EX. ((^ (x) (1+ x)) 3) => error !!!
;;;                              (fc (^ (x) (1+ x)) 3) => 4
(defmacro ^ (lambdalist &rest body)
  "Abbrev: (^ (x) body) <-> (lambda (x) body)"
  `(lambda ,lambdalist ,@body))

(defun as-key (sym) (intern (symbol-name sym) :keyword))

(defun append1 (lst elt) (append lst (list elt)))

;;; On Lisp
(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-gensyms (syms &body body)
    `(let ,(mapcar (lambda (s) `(,s (gensym ,(symbol-name s))))
                   syms)
       ,@body)))

(defmacro with-read-preserve-case (&body body)
  (with-gensyms (curr-case)
    `(let ((,curr-case (readtable-case *readtable*)))
       (setf (readtable-case *readtable*) :preserve)
       (unwind-protect (progn ,@body)
         (setf (readtable-case *readtable*) ,curr-case)))))

;;; On Lisp
(defun single? (lst) (and (consp lst) (not (cdr lst))))

(defmacro -> (x &rest form)
  "From clojure. Inserts x as the second item in the first form,
making a list of it if it is not a list already. If there are more
forms, inserts the first form as the second item in second form, etc."
  (cond ((null    form) x)
        ((single? form) (let ((first (1st form)))
                          (if (consp first)
                              `(,(1st first) ,x ,@(rest first))
                              `(,first ,x))))
        (t `(-> (-> ,x ,(1st form)) ,@(rest form)))))

;;====================================================================