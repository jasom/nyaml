(in-package "NYAML")

;;; Scalar values

(defparameter *null* :null
  "The NULL constant. Nil by default.")

(defparameter *false* nil
  "The falsehood constant. Nil by default.")

;; Collection types
(defparameter *make-map* (lambda () (make-hash-table :test #'equal)))
(defparameter *map-insert* (lambda (map key value)
			     (setf (gethash key map) value)
			     map))

(defparameter *list-to-seq* (lambda (x) (coerce x 'simple-vector)))


;;; Tag and anchor state for walking parse tree
(defparameter *tag* nil)

(defparameter *anchors* nil)

(defparameter *tag-handle* '(("!" . "!") ("!!" . "tag:yaml.org,2002:")))
