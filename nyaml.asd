;;;; nyaml.asd

(asdf:defsystem #:nyaml
  :description "Describe nyaml here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:esrap #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "nyaml")))

