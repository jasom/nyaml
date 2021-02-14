(in-package :nyaml-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun all-test-inputs ()
    (loop for item in
		   (uiop:directory-files
		    (asdf:system-relative-pathname :nyaml "test/data/")
		    "*.input")
	  collect item)))

(defun slurp-file (fname)
  (with-output-to-string (s)
    (with-open-file (f fname :element-type 'character)
      (loop with buffer = (make-array 4096 :element-type 'character)
	 for count = (read-sequence buffer f)
	 do (write-sequence buffer s :end count)
	 while (= count 4096)))))

#.`(define-test yaml-reference
     ,@(loop for item in (all-test-inputs)
			 collect `(define-test ,(pathname-name item)
				    (true (nth-value 2 (run-test ,item))))))

#+(or)(defun run-tests ()
  (loop for item in (all-test-inputs)
       unless (nth-value 2 (run-test item ))
     do (format t "~&Test ~a failed.~%" item)
     and sum 1 into failed
     else sum 1 into success
       finally (format t "~&~A Tests passed, ~A Tests failed~%" success failed)
       ))

(defun output-error-p (output-file)
  (with-open-file (f output-file)
    (let ((last-line (lastcar
		      (loop for line = (read-line f nil :eof)
			 while (not (eql line :eof))
			 collect line))))
      (and (stringp last-line)
	   (> (length last-line) 0)
	   (char= #\!
		  (char last-line
		   0))))))

(defun run-test (pathname)
  (let* ((info (split-sequence #\. (pathname-name pathname)))
	 (invalid
	  (or
	   (member "invalid" info :test #'string=)
	   (member "unparsed-" info :test #'alexandria:starts-with-subseq)
	   (output-error-p (uiop:merge-pathnames* (make-pathname :type  "output") pathname))))
	 (input (slurp-file pathname))
	 (args (loop for item in (remove-if-not (curry #'find #\=) (cdr info))
		  for argument =  (second (split-sequence #\= item))
		  collect (handler-case
			      (parse-integer argument)
			    (parse-error () (make-keyword (string-upcase argument))))))
	 (parse-function
	  (if args
	      (apply #'rcurry (intern (string-upcase (car info)) '#:nyaml)
		     0 (length input) args)
	      (alexandria:curry #'esrap:parse
				(intern (string-upcase (car info)) '#:nyaml)))))
    (cond
      (invalid
       (handler-case
	   (multiple-value-bind (production offset result)
	       (funcall parse-function input)
	     ;(format t "~&~A ~A ~A" production offset result)
	     (if
	      (or
	       (not (or result production))
	       offset)
	      (values t t t)
	      nil))
	 (esrap:esrap-parse-error () (values t t t))))
      (t
       (handler-case
	   (funcall parse-function input)
	 (esrap:esrap-parse-error () nil))))))
  

