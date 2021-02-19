;;; scalar.lisp taken from Fernando Borretti's cl-yaml
;;; licensed under MIT/X11 license
;;; Modified by Jason Miller 2015
;;; Changes also usable under terms of MIT/X11 license

(in-package :nyaml)

;;; Regular expressions or lists of names

(defparameter +null-names+
  (list "null" "Null" "NULL" "~"))

(defparameter +true-names+
  (list "true" "True" "TRUE"))

(defparameter +false-names+
  (list "false" "False" "FALSE"))

(defparameter +integer-scanner+
  (ppcre:create-scanner "^([-+]?[0-9]+)$"))

(defparameter +octal-integer-scanner+
  (ppcre:create-scanner "^0o([0-7]+)$"))

(defparameter +hex-integer-scanner+
  (ppcre:create-scanner "^0x([0-9a-fA-F]+)$"))

(defparameter +float-scanner+
  (ppcre:create-scanner
   "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$"))

(defparameter +nan-names+
  (list ".nan" ".NaN" ".NAN"))

(defparameter +positive-infinity-scanner+
  (ppcre:create-scanner "^[+]?(\\.inf|\\.Inf|\\.INF)$"))

(defparameter +negative-infinity-scanner+
  (ppcre:create-scanner "^-(\\.inf|\\.Inf|\\.INF)$"))

;;; The actual parser

(defun parse-scalar (string)
  "Parse a YAML scalar string into a Lisp scalar value."
  (print *tag*)
  (cond
    ((eql *tag* :|tag:yaml.org,2002:str|)
     string)
    ((eql *tag* 'nonspecific)
     string)
    ;; Null
    ((member string +null-names+ :test #'equal)
     *null*)
    ;; Truth and falsehood
    ((member string +true-names+ :test #'equal)
     t)
    ((member string +false-names+ :test #'equal)
     *false*)
    ;; Integers
    ((ppcre:scan +integer-scanner+ string)
     (parse-integer string))
    ((ppcre:scan +octal-integer-scanner+ string)
     (parse-integer (subseq string 2) :radix 8))
    ((ppcre:scan +hex-integer-scanner+ string)
     (parse-integer (subseq string 2) :radix 16))
    ;; Floating-point numbers
    ((ppcre:scan +float-scanner+ string)
     (parse-number:parse-real-number string))
    ;; Special floats
    ((member string +nan-names+ :test #'equal)
     (not-a-number))
    ((ppcre:scan +positive-infinity-scanner+ string)
     (positive-infinity))
    ((ppcre:scan +negative-infinity-scanner+ string)
     (negative-infinity))
    ;; Just a string
    (t
     string)))
