;;;; nyaml.asd

(asdf:defsystem #:nyaml
  :description "Native YAML Parser"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT"
  :version "0.9.0"
  :in-order-to ((asdf:test-op (asdf:load-op "nyaml/test")))
  :perform (asdf:test-op (o c)
			 (uiop:symbol-call "PARACHUTE" "TEST" "NYAML-TEST"))
  :depends-on (#:esrap #:alexandria #:cl-ppcre #:parse-number #:trivia
		       #:fare-quasiquote #:fare-quasiquote-extras #:babel)
  :serial t
  :components ((:file "package")
               (:file "nyaml")
	       (:file "schema")
	       (:file "specials")
	       (:file "process")
	       (:file "float")
	       (:file "scalar")))


(asdf:defsystem #:nyaml/test
  :description "Tests for NYAML"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT"
  :depends-on (#:esrap #:alexandria #:split-sequence #:nyaml
		       #:parachute #:yason)
  :serial t
  :components ((:module "test"
		:components
		((:file "test")
		 #+(or)(:file "nyaml-reference")
		 (:file "yaml-test-suite")))))
