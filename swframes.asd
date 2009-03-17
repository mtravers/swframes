(in-package :asdf)

(defsystem :swframes
  :name "Semantic Web Frame system"
  :author "Mike Travers"
  :serial t
  :components
  (;; setup and utilities
   (:file "package")
   (:file "swframes")
   (:file "namespace")
   (:file "lsparql")
   (:file "xml")
   )
  :depends-on
  ())

;;;; eof


