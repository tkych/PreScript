;;;; Last Updated : 2012/06/05 16:50:13 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.


;(in-package :in-prescript)
;;====================================================================
;; Common Lisp Utilities
;;====================================================================
(defun as-key (sym) (intern (symbol-name sym) :keyword))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (^ (s) `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

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