(in-package :asdf)

(defsystem :swframes
  :name "Semantic Web Frame system"
  :author "Mike Travers"
  :serial t
  :components
  (;; setup and utilities
   (:file "package")
   (:file "utils")
   (:file "swframes-0")
   (:file "swframes")
   (:file "compat")
   (:file "source")
   (:file "dereference")
   (:file "namespace")
   (:file "lsparql")
   (:file "sparul")
   (:file "browse")
   (:file "sw-frame-grid")
   (:file "xml")
   (:file "drugbank")
   (:file "druggy")
   (:file "genes")
   (:file "sw-pathwaycommons")
   (:file "history")
   )
  :depends-on
  ())

;;;; eof


