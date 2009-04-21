(in-package :asdf)

(defsystem :swframes
  :name "Semantic Web Frame system"
  :author "Mike Travers"
  :serial t
  :components
  (;; setup and utilities
   ;; basics
   (:file "package")
   (:file "utils")
   (:file "swframes-0")
   (:file "swframes")
   (:file "namespace")
   (:file "compat")
   ;; sources
   (:file "source")
   (:file "dereference")
   (:file "lsparql")
   (:file "sparul")
   (:file "files")
   ;; web ui stuff
   (:file "browse")
   (:file "sw-frame-grid")
   (:file "xml")
   (:file "history")
   ;; particular knowledge sources
   (:file "drugbank")
   (:file "druggy")
   (:file "sw-pathwaycommons")
   )
  :depends-on
  ())

;;;; eof


