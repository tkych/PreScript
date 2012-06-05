;;;; Last Updated : 2012/06/05 16:49:00 tkych


;; !!!Warning!!!
;; Current version of PreScript is 0.0.~~ (experimental alpha).
;; Current purpose of PreScript repository is to back up files.
;; Current status of this code is a mere devlopment-note.
 
;(in-package :in-prescript)
;;====================================================================
;; BASIC OBJECT
;;====================================================================
;; STRING:  #"string" -> "(string)"
;; FONT:    $Times-Roman -> "/Times-Roman"
;; ARRAY:   [ **** ] -> (any "[ ") **** (any " ]")
;; EXE-ARRAY: { **** } -> (any "{ ") **** (any " }")
;; DICTIONARY: <<

;;--------------------------------------
;; STRING
;;--------------------------------------
;; #"some-string" ~> "(some-string)"
;; !!!! #"" -> error !!!!
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
;; ARRAY: [ ], { }
;;--------------------------------------
(set-macro-character #\[ (^ (stream char)
                            (declare (ignore stream char))
                            `(any "[ ")))

(set-macro-character #\[ (^ (stream char)
                            (declare (ignore stream char))
                            `(any " ]")))

(set-macro-character #\{ (^ (stream char)
                            (declare (ignore stream char))
                            `(any "{ ")))

(set-macro-character #\} (^ (stream char)
                            (declare (ignore stream char))
                            `(any " }")))

;;--------------------------------------
;; DICT: << >>
;;--------------------------------------



;;====================================================================