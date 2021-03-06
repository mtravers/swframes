(in-package :asdf)

#+:ALLEGRO
(require :aserve)

(defsystem :swframes
  :name "swframes"
  :description "Semantic Web frame system, with SPARQL and RDFS support"
  :author "Mike Travers"
  :serial t
  :depends-on
  (:mtlisp :s-xml :cl-json #-:ALLEGRO :aserve 
	   :puri  ;; +++ puri is not needed for much, maybe get rid of it
	   :drakma ;; +++ brings in a ton of stuff and is somewhat broken, so maybe get rid of dependencies
	   ::acl-compat)			;; +++ ditto
  :components
  ((:static-file "swframes.asd")

   ;; setup and utilities
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

   (:module :src
	    :serial t
	    :components
	    (
	     #+BIOLISP
	     (:file "bio-package")
	     (:file "package")
	     (:file "source")
	     (:file "code-source")
	     (:file "swframes-0")
	     (:file "namespace")
	     (:file "standard-namespaces")
	     (:file "swframes")
	     (:file "swframes-2")
	     #+BIOLISP
	     (:file "compat")
	     (:file "clos")
	     (:file "rdfs")
	     ;; Allegro has a problem with this file, so skip it until resolved (+++)   #-:ALLEGRO
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
	     )))
  )

;;;; eof


