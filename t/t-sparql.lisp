(in-package :sw)

(define-test sparql-sanity
    (assert-true (sanity-check *default-sparql-endpoint*)))

(defvar *dbpedia* (make-instance 'sparql-endpoint :url "http://dbpedia.org/sparql"))

(define-test sparql-syntax
    (assert-true 
     (do-sparql *dbpedia* '(:select :all (:distinct t :limit 10) (?s #$rdf:type ?t))))
  (assert-true 
   (member #$http://dbpedia.org/resource/Illinois
     (do-sparql-one-var *dbpedia* '(:select (?t) (:distinct t :limit 10) (#$http://dbpedia.org/resource/Chicago #$dbpprop:subdivisionName ?t)))))
  (assert-true 
   (do-sparql *dbpedia* '(:select (?t ?f)
				  (:distinct t :limit 10)
				  (#$http://dbpedia.org/resource/Chicago #$dbpprop:subdivisionName ?t)
				  (:optional (?t #$dbpprop:fossil ?f))
			  ))))


(setq linkedct-query 
  '(:select
    (?trial ?title ?inttype ?intname)
    (:offset 0)
    (?trial #$linkedct:condition ?condition)
    (?condition #$linkedct:condition_name "Leprosy")
    (?trial #$linkedct:brief_title ?title)
    (?trial #$linkedct:intervention ?intervention)
    (?intervention #$linkedct:intervention_type ?inttype)
    (?intervention #$linkedct:intervention_name ?intname)))

(define-test linkedct-test
    ;; normal
    (assert-true (> (length (do-sparql nil linkedct-query)) 3))
  ;; case insensitized

  (assert-true (> (length (do-sparql nil (case-insensitize linkedct-query))) 3))
  (assert-true (> (length  (bulk-load-query nil linkedct-query)) 1))
  )

