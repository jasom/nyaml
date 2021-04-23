(in-package "NYAML")	
(named-readtables:in-readtable :fare-quasiquote)

(defgeneric parse (input &key schema multi-document-p))

(defun parse-no-schema (input)
  "Build parse tree of input"
  (let ((input (concatenate 'string (string #\Newline) input)))
    (esrap:parse 'l-yaml-stream input)))

(define-condition yaml-error ()
  ()
    (:documentation "The base class of all YAML conditions."))


(defun process-tag (tag)
  (trivia:match tag
    ('(tag nonspecific) 'nonspecific)
    (`(tag ,handle ,name)
      (let ((decoded-handle (assoc handle *tag-handle* :test #'string=)))
	(unless decoded-handle (error 'parse-error))
      (make-keyword (format nil "~A~A" (cdr decoded-handle) name))))
    (`(tag ,name)
      (make-keyword name))
    (nil nil)
    (_ (error 'parse-error))))

(defvar *directive* nil)

(defun process-document (doc &optional prefix)
  (trivia:match prefix
    (`((directive (tag ,handle ,prefix)) ,@rest)
	(let ((*tag-handle* (cons (cons handle prefix) *tag-handle*)))
	  (process-document doc rest)))
    (`((directive (yaml ,version)) ,@rest)
      (unless (string= version "1.2")
	(warn "Parsing document version ~a as if it were 12" version))
      (when *directive*
	(error "Multiple YAML directives in a single document"))
      (let ((*directive* t))
      (process-document doc rest)))
    (`((directive (reserved ,name ,@args)) ,@rest)
      (warn "ignoring reserved directive ~a with args ~a" name args)
      (process-document doc rest))
    (nil
     (trivia:match doc
       ((type string) (parse-scalar doc))
       (`(dq-string ,x) x)
       ('yaml-null
	(case *tag*
	  ((nil) *null*)
	  ;(:|tag:yaml.org,2002:str| "")
	  (:|tag:yaml.org,2002:str| "")
	  (t (warn "Unknown tag ~A" *tag*))))
       ((cons 'seq rest)
	(let (*tag*)
	  (funcall *list-to-seq*
		   (loop for item in rest
			 collect (process-document item)))))
       (`((properties ,@x) ,y)
	 (let* ((*tag* (process-tag (find 'tag x :key #'car)))
		(x (remove 'tag x :key #'car))
		(anchor (cadr (find 'anchor x :key #'car)))
		(x (remove 'anchor x :key #'car)))
	   (unless (emptyp x)
	     (warn "Ignoring properties ~A" x))
	   (let ((value (process-document y)))
	     (when anchor (push (cons anchor value) *anchors*))
	     value)))
       ((cons 'map rest)
	(let (*tag*)
	  (loop with result = (funcall *make-map*)
		for item in rest
		do (trivia:match item
		     ((list 'entry key value)
		      (setf result
			    (funcall *map-insert*
				     result 
				     (process-document key)
				     (process-document value))))
		     (_ (error "non-entry inside map")))
		finally (return result))))
       (`(alias ,name)
	 (let ((anchor (assoc name *anchors* :test #'string=)))
	   (if anchor
	       (cdr anchor)
	       (error "Unknown alias ~A" name))))
       (x (error "Unexpected parse tree ~A" x))))
    (x (error "Unknown document prefix ~A" x))))

(defun slurp-bytes (stream)
  (let ((v (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil)
	  while b do (vector-push-extend b v))
    v))

(defmethod parse ((input string) &key (schema *default-schema*) multi-document-p)
  (let ((parsed (parse-no-schema input))
	(*anchors* nil)
	(*default-schema* schema))
    (trivia:match parsed
      ((cons 'documents docs)
       (assert (or (= (length docs) 1) multi-document-p))
       (if multi-document-p
	   (cons :documents
		 (loop for (prefix meat) in docs
		       for d in docs
		       when d
			 collect (process-document meat prefix)))
	   (process-document (cadar docs) (caar docs))))
      (x (error "Unexpected parse tree ~a" x)))))

(defmethod parse ((input stream) &key (schema *default-schema*) multi-document-p)
  (if (subtypep (stream-element-type input) 'character)
      (parse (uiop:slurp-input-stream :string input) :schema schema :multi-document-p multi-document-p)
      (parse (slurp-bytes input) :schema schema :multi-document-p multi-document-p)))

(defun eltn (seq n)
  (when (> (length seq) n)
    (elt seq n)))

(defmethod parse ((input vector) &key (schema *default-schema*) multi-document-p)
  (let ((encoding
	     (case (eltn input 0)
	       (0 (case (eltn input 1)
		    (0 :utf-32be)
		    (t :utf-16be)))
	       (#xff  (case (eltn input 1)
			(#xfe 
			 (if (and (zerop (eltn input 2))
				  (zerop (eltn input 3)))
			     :utf-32le
			     :utf-16le))
			(t :utf-8)))
	       (#xfe (case (eltn input 1)
		       (#xff (if (and
				  (zerop (eltn input 2))
				  (zerop (eltn input 3)))
				 :utf-32be
				 :utf-16be))
		       (t :utf-8)))
	       (#xef :utf-8)
	       (t (case (eltn input 1)
		    (0 (if (and
			    (zerop (eltn input 2))
			    (zerop (eltn input 3)))
			   :utf-32le
			   :utf-16le))
		    (t :utf-8))))))
    (parse (babel:octets-to-string input :encoding encoding)
	   :schema schema
	   :multi-document-p multi-document-p)))
      

(defmethod parse ((input pathname) &key (schema *default-schema*) multi-document-p)
  (with-open-file (f input :element-type '(unsigned-byte 8))
    (parse f :schema schema :multi-document-p multi-document-p)))


(defmacro with-cl-yaml-semantics (() &body b)
  `(let ((*null* nil)
	 (*false* nil)
	 (*list-to-seq* #'identity))
     ,@b))
