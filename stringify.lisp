(in-package "NYAML")

;;; customization
(defparameter *indent* "  "
  "The string to use as a single indent.")
(defparameter *indent-level* 0)


(defparameter *document-separator*
  (format nil "~%~%---~%~%")
  "The string to separate yaml documents.")

(defun output-current-indent (stream &optional (offset 0))
  (loop :repeat (- *indent-level* offset) :do (write-sequence *indent* stream)))

(defun stringify (yaml)
  (with-output-to-string (s)
    (output yaml s)))

(defgeneric output (yaml stream)
  (:documentation "Output yaml to stream"))

;;; special
;; not a number
(defmethod output ((yaml (eql :nan)) stream)
  (write-sequence ".nan" stream))

#+sbcl
(defmethod output ((yaml (eql *sbcl-nan-value*)) stream)
  (write-sequence ".nan" stream))

#+allegro 
(defmethod output ((yaml (eql #.excl:*nan-double*)) stream)
  (write-sequence ".nan" stream))

;; positive infinity
(defmethod output ((yaml (eql :+inf)) stream)
  (write-sequence "+.inf" stream))

#+sbcl 
(defmethod output ((yaml (eql sb-ext:double-float-positive-infinity)) stream)
  (write-sequence "+.inf" stream))

#+allegro 
(defmethod output ((yaml (eql #.excl:*infinity-double*)) stream)
  (write-sequence "+.inf"))

;; negative infinity
(defmethod output ((yaml (eql :-inf)) stream)
  (write-sequence "-.inf" stream))

#+sbcl 
(defmethod output ((yaml (eql sb-ext:double-float-negative-infinity)) stream)
  (write-sequence "-.inf" stream))

#+allegro 
(defmethod output ((yaml (eql #.excl:*negative-infinity-double*)) stream)
  (write-sequence "-.inf" stream))

;; custom
(defmethod output (yaml stream)
  (cond
    ((equal *false* yaml) (write-sequence "false" stream))
    ((equal *null* yaml) (write-sequence "null" stream))
    (t (error "Can't stringify ~a to yaml" yaml))))

;;; boolean
(defmethod output ((yaml (eql t)) stream)
  (write-sequence "true" stream))

;;; number
(defmethod output ((yaml integer) stream)
  (format stream "~D" yaml))

(defmethod output ((yaml float) stream)
  (let ((*read-default-float-format* (type-of yaml)))
    (princ yaml stream)))

;;; string
(defrule c-unquoted
    (and
     (! (character-ranges #\Newline #\" #\\))
     c-printable)
  (:lambda (x) (second x)))

(defrule unquoted-chars
    (* c-unquoted)
  (:text t))

(defun quote-char (c s)
  (cond
    ((eql c #\Newline)
     (write-sequence "\\n" s))
    ((eql c #\\)
     (write-sequence "\\\\" s))
    ((< (char-code c) 255)
     (format s "\\x~2,'0x" (char-code c)))
    ((< (char-code c) 65536)
     (format s "\\u~4,'0x" (char-code c)))
    (t
     (format s "\\U~4,'0x" (char-code c)))))

(defmethod output ((yaml string) stream)
  (write-char #\" stream)
  (loop for (some next _) = (multiple-value-list (esrap:parse 'unquoted-chars yaml :junk-allowed t))
	  then (multiple-value-list (esrap:parse 'unquoted-chars yaml :start next :junk-allowed t))
	do (write-sequence some stream)
	while next
	do (quote-char (char yaml next) stream)
	   (incf next))
    (write-char #\" stream))

;;; sequence
(defmethod output ((yaml array) stream)
  (if (emptyp yaml)
      (write-sequence "[]" stream)
      (let* ((*indent-level* (1+ *indent-level*)))
	(loop :for yaml :across yaml
	      :for first = t :then nil
	      :unless first
		:do (write-char #\Newline stream)
		    (output-current-indent stream 1)
	      :do (write-sequence "- " stream)
		  (output yaml stream)))))

(defun output-documents (yaml stream)
  (loop for (item . rest) on (rest yaml)
	do (write-sequence item stream)
	when rest
	  do (write-sequence *document-separator* stream)))

(defun output-list (yaml stream)
  (let* ((*indent-level* (+ *indent-level* 1)))
    (loop :for (yaml . rest) :on yaml
	  :do (write-sequence "- " stream)
	      (output yaml stream)
	  :when rest
	    :do
	       (write-char #\Newline stream)
	       (output-current-indent stream 1))))

(defmethod output ((yaml cons) stream)
  (cond
    ;; documents
    ((eq :documents (first yaml))
     (output-documents yaml stream))
    ;; alist
    ((alistp yaml)
     (output-alist yaml stream))
    ;; regular list
    (t (output-list yaml stream))))

;;; mapping
;; hash-table
(defmethod output ((yaml hash-table) stream)
  (if (= 0 (hash-table-count yaml))
      (write-sequence "{}" stream)
      (let* ((*indent-level* (+ *indent-level* 1)))
	(loop :for key :being :the :hash-keys :of yaml
		:using (:hash-value value)
	      :for first = t then nil
	      :unless first :do
		(write-char #\Newline stream)
		(output-current-indent stream)
	      :if (typep value '(or (and array (not string))
				 cons hash-table))
		:do (output key stream)
		    (format stream ":~%")
		    (output-current-indent stream)
		    (output value stream)
	      :else
		:do
		   (output key stream)
		   (write-sequence ": " stream)
		   (output value stream)))))

;; alist
(defun alistp (list)
  (and (listp list)
       (every 'consp list)
       (every (lambda (pair) (typep (first pair) 'alexandria:string-designator))
              list)))

(defun output-alist (yaml stream)
  (if (null yaml)
      (write-sequence "{}" stream)
      (let* ((*indent-level* (+ *indent-level* 1)))
	(loop :for (key . value) :in (remove-duplicates  yaml :test 'equal :key #'car)
	      :for first = t :then nil
	      :unless first :do
		(output-current-indent stream)
		(write-char #\Newline stream)
	      :if (typep value '(or (and array (not string))
				 cons hash-table))
		:do
		   (output key stream)
		   (format stream ":~%")
		   (output-current-indent stream)
		   (output value stream)
	      :else
		:do (output key stream)
		    (write-sequence ": " stream)
		    (output value stream)))))

;;; dump to file
(defun dump (yaml path &key if-exists (if-does-not-exist :create))
  "Stringify a lisp data structure and the resulting yaml string to a file."
  (with-open-file (out path
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist if-does-not-exist)
    (output yaml out)))
