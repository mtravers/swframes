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
   (:file "swframes-0")
   (:file "swframes")
   (:file "namespace")
   (:file "compat")
   (:file "source")
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
	     (:file "files")
	     (:file "pickle")
	     (:file "memory")))
   )
)

;;;; eof


