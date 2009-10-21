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

    ;; Linked Data related
    ("linkedct" "http://data.linkedct.org/resource/")
    ("linkedct" "http://data.linkedct.org/resource/linkedct/")
    ("d2r" "http://sites.wiwiss.fu-berlin.de/suhl/bizer/d2r-server/config.rdf#")
    ("dbpedia" "http://dbpedia.org/property/")
    ("drugbank" "http://www4.wiwiss.fu-berlin.de/drugbank/resource/")
    ("dailymed" "http://www4.wiwiss.fu-berlin.de/dailymed/resource/")
    ;; +++ These namespaces are fucked.  Playing with diseasome, should deal with the other wiwiss ones
    ("diseasome" "http://www4.wiwiss.fu-berlin.de/diseasome/resource/diseasome/")
    ("diseasomer" "http://www4.wiwiss.fu-berlin.de/diseasome/resource/")
    ;; Other
    ("nci" "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#")
    ("go" "http://www.geneontology.org/go#")
    ("go1" "http://www.geneontology.org/dtds/go.dtd#") ;slots use this
    ("bp" "http://www.biopax.org/release/biopax-level2.owl#")

    ;; Local
    ("crx" "http://collabrx.com/rdf/")

    ))

  (dolist (n *standard-namespaces*)
    (register-namespace (car n) (cadr n) t)))
