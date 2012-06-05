;;;; Last Updated : 2012/06/05 16:55:28 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.


;(in-package :prescript)
;;====================================================================
;; PreScript-Utils
;;====================================================================
(defmacro defop (name args &body body)
  `(defun ,name (space ,@args)
     (-> space
         ,@body)))

;;====================================================================