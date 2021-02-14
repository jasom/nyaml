(in-package "NYAML-TEST")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *yaml-test-suite-dirs*
    (remove-if-not #'uiop:directory-pathname-p
		   (uiop:directory* (asdf:system-relative-pathname "nyaml" #p"test/yaml-test-suite-data/yaml-test-suite-data-*/*")))))

(defmacro generate-tests ()
  `(define-test yaml-test-suite
     ,@(loop for item in *yaml-test-suite-dirs* 
	    for name = (car (last (pathname-directory item)))
	    for json = (uiop:merge-pathnames* "in.json" item)
	    for yaml = (uiop:merge-pathnames* "in.yaml" item)
	     append
	     (cond
	       ((uiop:file-exists-p json)
		(list `(define-test ,name
			 (is equalp
			     (with-open-file (f ,json)
			      (loop for x = (handler-case
						(yason:parse f)
					      (end-of-file () nil))
				    while x collect x))
			     (cdr (nyaml::parse-yaml-file ,yaml :multi-document-p t))))))
	       ((uiop:file-exists-p (uiop:merge-pathnames* "error" item))
		(list `(define-test ,name
			  (fail (nyaml::parse-yaml-file ,yaml)))))
	       (t #+(or)(warn "Don't know what to do with ~A" item) nil)))))

(generate-tests)
