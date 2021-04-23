(in-package "NYAML")

(defstruct schema
  (null-names (list "null" "Null" "NULL" "~"))
  (true-names (list "true" "True" "TRUE"))
  (false-names (list "false" "False" "FALSE"))
  (decimal-integer-scanner (ppcre:create-scanner "^([-+]?[0-9]+)$"))
  (octal-integer-scanner (ppcre:create-scanner "^0o([0-7]+)$"))
  (hex-integer-scanner (ppcre:create-scanner "^0x([0-9a-fA-F]+)$"))
  (float-scanner (ppcre:create-scanner "^([-+])?(\\.[0-9]+|[0-9]+(?:\\.[0-9]*)?)([eE][-+]?[0-9]+)?$"))
  (nan-names (list ".nan" ".NaN" ".NAN"))
  (positive-infinity-scanner (ppcre:create-scanner "^[+]?(\\.inf|\\.Inf|\\.INF)$"))
  (negative-infinity-scanner (ppcre:create-scanner "^-(\\.inf|\\.Inf|\\.INF)$")))

(defparameter +yaml-12-schema+ (make-schema))

(defparameter +match-nothing+
  (cl-ppcre:create-scanner `(:filter ,(lambda (x) (declare (ignore x))nil))))

(defparameter +json-schema+ (make-schema
			     :null-names '("null")
			     :true-names '("true")
			     :false-names '("false")
			     :decimal-integer-scanner (cl-ppcre:create-scanner "^-?(0|[1-9][0-9]*)$")
			     :octal-integer-scanner +match-nothing+
			     :hex-integer-scanner +match-nothing+
			     :float-scanner (cl-ppcre:create-scanner "^-?(0|[1-9][0-9]*)(\\.[0-9]*)?([eE][-+][0-9]+)?$")
			     :nan-names nil
			     :positive-infinity-scanner +match-nothing+
			     :negative-infinity-scanner +match-nothing+))
			     
;; TODO support base-60 and base-2 types
(defparameter +yaml-11-schema+
  (make-schema
   ;; null-names same as 12
   :true-names (let ((v '("y" "yes" "true" "on")))
		 (concatenate 'list
			      v
			      (map 'list 'string-upcase v)
			      (map 'list 'string-capitalize v)))
   :false-names (let ((v '("n" "no" "false" "off")))
		 (concatenate 'list
			      v
			      (map 'list 'string-upcase v)
			      (map 'list 'string-capitalize v)))
   :decimal-integer-scanner (cl-ppcre:create-scanner "^([-+])?(0|[1-9][0-9_]*)$")
   :octal-integer-scanner (cl-ppcre:create-scanner "^([-+])?0([0-7_]+)$")
   :hex-integer-scanner (cl-ppcre:create-scanner "^([-+])?0x([0-9a-fA-F_]+)$")
   :float-scanner (cl-ppcre:create-scanner "^([-+])?([0-9][0-9_]*)?(\\.[0-9_]*)([eE][-+][0-9]+)?$")
   ;;nan-names same as 12 core
   ;;positive-infinity same as 12 core
   ;;negative-infinity same as 12 core
   ))
