(in-package :sw)

;;; Some standard namespaces
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *standard-namespaces*
    '(;; SemWeb infrastructure
      ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
      ("xsd" "http://www.w3.org/2001/XMLSchema#")
      ("owl" "http://www.w3.org/2002/07/owl#")

      ;; Common schemas
      ("dc" "http://purl.org/dc/terms/")
      ("foaf" "http://xmlns.com/foaf/0.1/")
      ("skos" "http://www.w3.org/2004/02/skos/core#")

      ;; Local
      ("sw" "http://swframes.org/rdf/")	;+++ well, who should own this?
      ))

(dolist (n *standard-namespaces*)
  (register-namespace (car n) (cadr n) t))

)
