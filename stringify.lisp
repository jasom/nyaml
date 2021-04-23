(in-package "NYAML")

;;; customization
(defparameter *indent* "  "
  "The string to use as a single indent.")
(defparameter *indent-level* 0)
(defparameter *current-indent* "")
(defparameter *newline*
  #-windows
  (string (code-char 10))
  #+windows
  (coerce (list (code-char 13)
                (code-char 10))
          'string)
  "The string that separates lines.")
(defparameter *document-separator*
  (concatenate
   'string
   *newline* *newline*
   "---"
   *newline* *newline*)
  "The string to separate yaml documents.")

(defun strjoin (delimiter strings)
  (format nil (concatenate 'string "~{~a~^" delimiter "~}")
          strings))

(defun calculate-current-indent ()
  (strjoin "" (loop :repeat *indent-level*
                    :collect *indent*)))

(defgeneric stringify (yaml)
  (:documentation "Turn a lisp data structure into a yaml string."))

;;; special
;; not a number
(defmethod stringify ((yaml (eql :nan)))
  ".nan")

#+sbcl
(defmethod stringify ((yaml (eql *sbcl-nan-value*)))
  ".nan")

#+allegro 
(defmethod stringify ((yaml (eql #.excl:*nan-double*)))
  ".nan")

;; positive infinity
(defmethod stringify ((yaml (eql :+inf)))
  "+.inf")

#+sbcl 
(defmethod stringify ((yaml (eql sb-ext:double-float-positive-infinity)))
  "+.inf")

#+allegro 
(defmethod stringify ((yaml (eql #.excl:*infinity-double*)))
  "+.inf")

;; negative infinity
(defmethod stringify ((yaml (eql :-inf)))
  "-.inf")

#+sbcl 
(defmethod stringify ((yaml (eql sb-ext:double-float-negative-infinity)))
  "-.inf")

#+allegro 
(defmethod stringify ((yaml (eql #.excl:*negative-infinity-double*)))
  "-.inf")

;; custom
(defmethod stringify (yaml)
  (cond
    ((equal *false* yaml) "false")
    ((equal *null* yaml) "null")
    (t (error "Can't stringify ~a to yaml" yaml))))

;;; boolean
(defmethod stringify ((yaml (eql t)))
  "true")

;;; number
(defmethod stringify ((yaml integer))
  (princ-to-string yaml))

(defmethod stringify ((yaml float))
  (princ-to-string yaml))

;;; string
(defmethod stringify ((yaml string))
  (if (stringp (handler-case
                   (parse yaml)
                 (t () nil)))
      ;; yaml cannot be misinterpreted as another type:
      ;; quotes not needed
      yaml
      ;; single quotes needed
      (format nil "'~a'"
              ;; escape single quotes
              (ppcre:regex-replace-all "'" yaml "''"))))

;;; sequence
(defmethod stringify ((yaml array))
  (strjoin
   (concatenate 'string *newline* *current-indent*)
   (let* ((*indent-level* (+ *indent-level* 1))
          (*current-indent* (calculate-current-indent)))
     (loop :for yaml :across yaml
           :collect
           (format nil "- ~a"
                   (stringify yaml))))))

(defun stringify-documents (yaml)
  (strjoin
   *document-separator*
   (map 'list 'stringify (rest yaml))))

(defun stringify-list (yaml)
  (strjoin
   (concatenate 'string *newline* *current-indent*)
   (let* ((*indent-level* (+ *indent-level* 1))
          (*current-indent* (calculate-current-indent)))
     (loop :for yaml :in yaml
           :collect
           (format nil "- ~a"
                   (stringify yaml))))))

(defmethod stringify ((yaml cons))
  (cond
    ;; documents
    ((eq :documents (first yaml))
     (stringify-documents yaml))
    ;; alist
    ((alistp yaml)
     (stringify-alist yaml))
    ;; regular list
    (t (stringify-list yaml))))

;;; mapping
;; hash-table
(defmethod stringify ((yaml hash-table))
  (if (= 0 (hash-table-count yaml))
      "{}"
      (strjoin
       (concatenate 'string *newline* *current-indent*)
       (let* ((*indent-level* (+ *indent-level* 1))
              (*current-indent* (calculate-current-indent)))
         (loop :for key :being :the :hash-keys :of yaml
                 :using (:hash-value value)
               :if (typep value '(or (and array (not string))
                                     cons hash-table))
                 :collect (format nil "~a:~a~a~a"
                                  key *newline*
                                  *current-indent*
                                  (stringify value))
               :else
                 :collect (format nil "~a: ~a"
                                  key (stringify value)))))))

;; alist
(defun alistp (list)
  (and (listp list)
       (every 'consp list)
       (every (lambda (pair) (typep (first pair) 'alexandria:string-designator))
              list)))

(defun stringify-alist (yaml)
  (if (null yaml)
      "{}"
      (strjoin
       (concatenate 'string *newline* *current-indent*)
       (let* ((*indent-level* (+ *indent-level* 1))
              (*current-indent* (calculate-current-indent)))
         (loop :for (key . value) :in (remove-duplicates  yaml :test 'equal :key 'first)
               :if (typep value '(or (and array (not string))
                                     cons hash-table))
                 :collect (format nil "~a:~a~a~a"
                                  key *newline*
                                  *current-indent*
                                  (stringify value))
               :else
                 :collect (format nil "~a: ~a"
                                  key (stringify value)))))))
;;; dump to file
(defun dump (yaml path &key if-exists (if-does-not-exist :create))
  "Stringify a lisp data structure and the resulting yaml string to a file."
  (with-open-file (out path
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist if-does-not-exist)
    (write-sequence (stringify yaml) out)))
