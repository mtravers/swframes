(in-package :sw)

-----------------------------------

;;; This source won't work with POST, only GET.   It's also apparently different from the Uniprot RDF downloads
;;; see run-sparql-0
(setq ep 
      (make-instance 'sparql-endpoint :url "http://labs.intellidimension.com/uniprot/sparql"))

(sanity-check ep)

Also returns URIs with bad namespaces.

------------------------------------
;;; Sample query library

;;; This returns 10000 rows, must be an internal  limit
(do-sparql *default-frame-source* '(:select (?t) () (?s #$rdf:type ?t)))

;;; Should return all types, but times out
(do-sparql *default-frame-source* '(:select (?t) (:distinct ?t) (?s #$rdf:type ?t)))

(do-sparql *default-frame-source* '(:select (?t) (:distinct ?t :limit 10) (?s #$rdf:type ?t)))

;;; only returns a few things
(do-sparql *default-frame-source* '(:select (?s) () (?s  #$rdf:type #$rdfs:Class)))




;;; PubMed articles are like this:
; #$http://purl.org/science/article/pmid/17108814

(defun go-term-lookup (name)
  (do-sparql *default-frame-source*
    '(:select (?g) (:distinct t)
      (?b #$http://www.geneontology.org/formats/oboInOwl#hasURI ?g)
      (?b 
      



;;; PubMed articles are like this:
; #$http://purl.org/science/article/pmid/17108814
