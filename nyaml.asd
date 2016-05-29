;;;; nyaml.asd

(asdf:defsystem #:nyaml
  :description "ESRAP based YAML parser"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT/X11"
  :depends-on (#:esrap #:alexandria #:cl-ppcre #:parse-number #:optima)
  :serial t
  :components ((:file "package")
               (:file "nyaml")
	       (:file "float")
	       (:file "scalar")))


(asdf:defsystem #:nyaml/test
  :description "Tests for NYAML"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT/X11"
  :depends-on (#:esrap #:alexandria #:split-sequence #:nyaml)
  :serial t
  :components ((:module "test"
			:components ((:file "test")))))
