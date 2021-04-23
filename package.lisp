;;;; package.lisp

(defpackage #:nyaml
  (:use #:cl
	#:esrap
	#:alexandria)
  (:shadow "PARSE")
  (:export "PARSE" "*NULL*" "*FALSE*" "*MAKE-MAP*" "*MAP-INSERT*" "*LIST-TO-SEQ*"
	   "+YAML-11-SCHEMA+" "+YAML-12-SCHEMA+" "+JSON-SCHEMA+" "*DEFAULT-SCHEMA*"))

