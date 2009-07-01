(in-package :sw)

;;; ordinary
(DO-SPARQL *bioblog-store*
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name "Leprosy")
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME)))


;;; case-insensitive -- times out, it's not very smart apparently
(DO-SPARQL *bioblog-store*
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name ?cname)
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME)
    (:filter (:regex ?cname "leprosy" "i"))))

;;; this works OK
(DO-SPARQL *bioblog-store*
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name ?cname)
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME)
    (:filter (:regex ?cname "^leprosy$" "i"))))



(DO-SPARQL *bioblog-store*
  (case-insensitize
  '(:SELECT
    (?TRIAL ?TITLE ?INTTYPE ?INTNAME)
    (:OFFSET 0 :LIMIT 20)
    (?TRIAL #$linkedct:condition ?CONDITION)
    (?CONDITION #$linkedct:condition_name "Leprosy")
    (?TRIAL #$linkedct:brief_title ?TITLE)
    (?TRIAL #$linkedct:intervention ?INTERVENTION)
    (?INTERVENTION #$linkedct:intervention_type ?INTTYPE)
    (?INTERVENTION #$linkedct:intervention_name ?INTNAME))))


;;; This source won't work with POST, only GET.   It's also apparently different from the Uniprot RDF downloads

(setq ep 
      (make-instance 'sparql-endpoint :uri "http://labs.intellidimension.com/uniprot/sparql"))

(sanity-check ep)


;;; Sample query library

;;; This returns 10000 rows, must be an internal  limit
(do-sparql *default-frame-source* '(:select (?t) () (?s #$rdf:type ?t)))

;;; Should return all types, but times out
(do-sparql *default-frame-source* '(:select (?t) (:distinct ?t) (?s #$rdf:type ?t)))

(do-sparql *default-frame-source* '(:select (?t) (:distinct ?t :limit 10) (?s #$rdf:type ?t)))

;;; only returns a few things
(do-sparql *default-frame-source* '(:select (?s) () (?s  #$rdf:type #$rdfs:Class)))

(defun term-search (term)
  (do-sparql-one-var *default-frame-source* `(:select (?s) () (?s ?p ,term))))

(defun term-search-grid (term)
  (nl::frame-grid (do-sparql-one-var *default-frame-source* `(:select (?s) (:distinct t) (?s ?p ,term)))))

;;; Science Commons specific
(defun gene-lookup (name)
  (do-sparql-one-var *default-frame-source*
    `(:select (?gene) (:distinct t) 
	      (?gene #$http://purl.org/science/owl/sciencecommons/ggp_has_symbol ,name)
	      (?gene #$rdf:type #$http://purl.org/science/owl/sciencecommons/gene_record))))


;;; PubMed articles are like this:
; #$http://purl.org/science/article/pmid/17108814

(defun go-term-lookup (name)
  (do-sparql *default-frame-source*
    '(:select (?g) (:distinct t)
      (?b #$http://www.geneontology.org/formats/oboInOwl#hasURI ?g)
      (?b 
      
