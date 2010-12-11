(in-package :asdf)

#+:ALLEGRO
(require :aserve)

(defsystem :swframes
  :name "swframes"
  :description "Semantic Web frame system, with SPARQL and RDFS support"
  :author "Mike Travers"
  :serial t
  :depends-on
  (:mtlisp :s-xml :cl-json
	   ;; really we just use the client part of aserve, so probably should split that out
	   #-:ALLEGRO :aserve) 
  :components
  (;; setup and utilities
   (:module :lib
	    :serial t
	    :components
	    ((:file "time")
	     (:file "lxml")))
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
   ;; Allegro has a problem with this file, so skip it until resolved (+++)
   #-:ALLEGRO
   (:file "special-slots")
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


