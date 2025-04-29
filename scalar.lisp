;;; scalar.lisp taken from Fernando Borretti's cl-yaml
;;; licensed under MIT/X11 license
;;; Modified by Jason Miller 2015-2020
;;; Changes also usable under terms of MIT/X11 license

(in-package :nyaml)

(defun yaml-parse-number (scanner string radix)
  "Parse a string as a number given a scanner and radix

First remove all parts from STRING that aren't in register groups for SCANNER
Then remove underscores
then parse what's left as an integer in the RADIX"
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings scanner string)
    (declare (ignore match))
    #+(or)(print regs)
    (parse-number:parse-real-number
     (remove #\_ (apply #'concatenate 'string (coerce regs 'list)))
     :radix radix
     :float-format 'double-float)))

;;; The actual scalar parser
(declaim (notinline parse-scalar))
(defun parse-scalar (string)
  "Parse a YAML scalar string into a Lisp scalar value."
  (cond
    ((eql *tag* :|tag:yaml.org,2002:str|)
     string)
    ((eql *tag* 'nonspecific)
     string)
    ((eql *tag* :|tag:yaml.org,2002:bool|)
     (if (member string (schema-true-names *default-schema*) :test #'equal)
	 t
	 (if (member string (schema-false-names *default-schema*) :test #'equal)
	     nil
	     (error "Could not parse boolean ~S" string))))
    ((eql *tag* :|tag:yaml.org,2002:null|)
     *null*)
    ((eql *tag* :|tag:yaml.org,2002:int|)
     (cond
       ((ppcre:scan (schema-decimal-integer-scanner *default-schema*) string)
	(yaml-parse-number (schema-decimal-integer-scanner *default-schema*)
			    string
			    10))
       ((ppcre:scan (schema-octal-integer-scanner *default-schema*) string)
	(yaml-parse-number (schema-octal-integer-scanner *default-schema*)
			    string
			    8))
       ((ppcre:scan (schema-hex-integer-scanner *default-schema*) string)
	(yaml-parse-number (schema-hex-integer-scanner *default-schema*)
			    string
			    16))
       (t (error "Unable to parse integer ~S" string))))
    ((eql *tag* :|tag:yaml.org,2002:float|)
     (cond
       ((ppcre:scan (schema-float-scanner *default-schema*) string)
	(float (yaml-parse-number (schema-float-scanner *default-schema*) string 10) 1.0d0))
       ;; Special floats
       ((member string (schema-nan-names *default-schema*) :test #'equal)
	(not-a-number))
       ((ppcre:scan (schema-positive-infinity-scanner *default-schema*) string)
	(positive-infinity))
       ((ppcre:scan (schema-negative-infinity-scanner *default-schema*) string)
	(negative-infinity))
       ;; Integers as floats
       ((ppcre:scan (schema-decimal-integer-scanner *default-schema*) string)
	(float (yaml-parse-number (schema-decimal-integer-scanner *default-schema*) string 10)) 1.0d0)
       ((ppcre:scan (schema-octal-integer-scanner *default-schema*) string)
	(float (yaml-parse-number (schema-octal-integer-scanner *default-schema*) string 8)) 1.0d0)
       ((ppcre:scan (schema-hex-integer-scanner *default-schema*) string)
	(float (yaml-parse-number (schema-hex-integer-scanner *default-schema*) string 16)) 1.0d0)
       (t (error "Unable to parse floating point number ~S" string))))
    ((unless (member *tag* '(:!? nil))
       (warn "Unknown tag ~A" *tag*)
       nil))
    ;; At this point it's not a tag we know about, so use schema to detect
    ;; Null
    ((member string (schema-null-names *default-schema*) :test #'equal)
     *null*)
    ;; Truth and falsehood
    ((member string (schema-true-names *default-schema*) :test #'equal)
     t)
    ((member string (schema-false-names *default-schema*) :test #'equal)
     *false*)
    ;; Integers
    ((ppcre:scan (schema-decimal-integer-scanner *default-schema*) string)
     (yaml-parse-number (schema-decimal-integer-scanner *default-schema*)
			string 10))
    ((ppcre:scan (schema-octal-integer-scanner *default-schema*) string)
     (yaml-parse-number (schema-octal-integer-scanner *default-schema*)
			string 8))
    ((ppcre:scan (schema-hex-integer-scanner *default-schema*) string)
     (yaml-parse-number (schema-hex-integer-scanner *default-schema*)
			string 16))
    ;; Floating-point numbers
    ((ppcre:scan (schema-float-scanner *default-schema*) string)
     (yaml-parse-number (schema-float-scanner *default-schema*) string 10))
    ;; Special floats
    ((member string (schema-nan-names *default-schema*) :test #'equal)
     (not-a-number))
    ((ppcre:scan (schema-positive-infinity-scanner *default-schema*) string)
     (positive-infinity))
    ((ppcre:scan (schema-negative-infinity-scanner *default-schema*) string)
     (negative-infinity))
    ;; Just a string
    (t
     string)))
