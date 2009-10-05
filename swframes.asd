(in-package :asdf)

(defsystem :swframes
  :name "Semantic Web Frame system"
  :author "Mike Travers"
  :serial t
  :depends-on
  (:mtlisp
   :s-xml
   :cl-json)
  :components
  (;; setup and utilities
   ;; basics
   (:file "package")
   (:file "utils")
   (:file "xmlu")
   (:file "source")
   (:file "swframes-0")
   (:file "namespace")
   (:file "standard-namespaces")
   (:file "swframes")
   (:file "compat")
   (:file "rdfs")
   ;; sources
   (:file "dereference")
   (:file "k-sparql")
   (:file "lsparql")
   (:file "sparul")
   (:file "files")
   (:file "mql")
   (:file "xml")
   (:module "tuples"
	    :serial t
	    :components
	    ((:file "tuples")
	     (:file "tuple-sparql")
	     (:file "files")
	     (:file "pickle")
	     (:file "memory")))
   )
)

;;;; eof


