(in-package :asdf)

(defsystem :swframes
  :name "swframes"
  :description "Semantic Web frame system, with SPARQL and RDFS support"
  :author "Mike Travers"
  :serial t
  :depends-on
  (:mtlisp :3utils
   :s-xml
   :cl-json)
  :components
  (;; setup and utilities
   ;; basics
   (:module :blisp
	    :serial t 
	    :components 
	    ((:file "package")
	     (:file "redefinitions")))
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
   (:file "dump")
   ;; sources
   (:file "dereference")
   (:file "k-sparql")
   (:file "lsparql")
   (:file "sparul")
   (:file "files")
   (:file "mql")
   )
)

;;;; eof


