;;;; Last Updated : 2012/06/05 16:50:33 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.

 
;(in-package :in-prescript)
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
      (dolist (var (reverse vars))
        (format t "~%~A~%" (gethash var dict))))
    (when procs
      (format t "~%%%------------------- Procedure~P ------------------"
              (length procs))
      (dolist (proc (reverse procs))
        (format t "~%~A~%" (gethash proc dict))))))


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