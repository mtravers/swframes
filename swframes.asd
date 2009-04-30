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
   (:file "source")
   (:file "rdfs")
   ;; sources
   (:file "dereference")
   (:file "lsparql")
   (:file "sparul")
   (:file "files")
   (:file "mql")
   ;; web ui stuff
   (:file "browse")
;moved   (:file "sw-frame-grid")
   (:file "xml")
; particulars moved up.
   )
  :depends-on
  ())

;;;; eof


