;;;; Last Updated : 2012/06/05 16:49:22 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.


;(in-package :in-prescript)
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

(defun copy-space (space
                   &key (vars t) (procs t) (oprd t) (dict t))
  (make-inst 'user-space
             :vars  (if (eql t vars)  (copy-list (:vars space))  vars)
             :procs (if (eql t procs) (copy-list (:procs space)) procs)
             :oprd  (if (eql t oprd)  (copy-list (:oprd space))  oprd)
             :dict  (if (eql t dict)  (copy-hash (:dict space))  dict)
                         ))

;; (defun copy-space (space &rest copy-slots)
;;   (if (not copy-slots)
;;       (with-slots (vars procs oprd dict) space
;;          (make-inst 'user-space :vars  (copy-list vars)
;;                     :procs (copy-list procs)
;;                     :oprd  (copy-list oprd)
;;                     :dict  (copy-hash dict)))
;;       (make-inst 'user-space :vars  (copy-list vars)
;;                  :procs (copy-list procs)
;;                  :oprd  (copy-list oprd)
;;                  :dict  (copy-hash dict))))

;; (defun copy-space (space &optional
;;                          (slots '(:vars :procs :oprd :dict) slotts?))
;;   (with-slots (vars procs oprd dict) space
;;      (make-inst 'user-space :vars  (copy-list vars)
;;                             :procs (copy-list procs)
;;                             :oprd  (copy-list oprd)
;;                             :dict  (copy-hash dict))))

(defun space-equal (s1 s2)
  (and (equal (:oprd  s1) (:oprd  s2))
       (equal (:vars  s1) (:vars  s2))
       (equal (:procs s1) (:procs s2))
       (equal (:dict  s1) (:dict  s2))))

;;====================================================================