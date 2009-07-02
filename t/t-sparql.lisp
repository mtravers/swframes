(in-package :sw)

(define-test sparql-sanity
    (assert-true (sanity-check *default-sparql-endpoint*)))

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

