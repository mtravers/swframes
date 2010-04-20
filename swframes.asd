(in-package :asdf)

(defsystem :swframes
  :name "swframes"
  :description "Semantic Web frame system, with SPARQL and RDFS support"
  :author "Mike Travers"
  :serial t
  :depends-on
  (:mtlisp :3utils :s-xml) 
  :components
  (;; setup and utilities
   ;; basics
   (:module :blisp
	    :serial t 
	    :components 
	    ((:file "package")
	     (:file "redefinitions")))
   #+BIOLISP
   (:file "bio-package")
   (:file "package")
   (:file "source")
   (:file "code-source")
   (:file "swframes-0")
   (:file "namespace")
   (:file "standard-namespaces")
   (:file "swframes")
   #+BIOLISP
   (:file "compat")
   (:file "clos")
   (:file "rdfs")
   (:file "session")
   (:file "dump")
   (:file "versioning")
   (:file "homology")
   ;; sources
   (:file "dereference")
   (:file "dereference-server")
   (:file "k-sparql")
   (:file "lsparql")
   (:file "sparul")
   (:file "discovery")
   (:file "files")
   )
)

;;;; eof


