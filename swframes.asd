(in-package :asdf)

(defsystem :swframes
  :name "Semantic Web Frame system"
  :author "Mike Travers"
  :serial t
  :components
  (;; setup and utilities
   (:file "package")
   (:file "swframes-0")
   (:file "swframes")
   (:file "namespace")
   (:file "lsparql")
   (:file "browse")
   (:file "sw-frame-grid")
   (:file "xml")
   (:file "druggy")
   )
  :depends-on
  ())

;;;; eof


