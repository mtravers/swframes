(in-package :sw)

(use-package :lisp-unit)

(define-test deref-test
    (delete-frame #$http://dbpedia.org/resource/Panitumumab)
  (let ((f (intern-uri "http://dbpedia.org/resource/Panitumumab")))
    (dereference f)
    (assert-true (slotv f #$rdfs:comment))
    ;; test transitive dereference (+++ not presently working)
    (let ((subject (car (slotv f #$skos:subject))))
      (assert-true subject)
      (assert-true (slotv subject #$skos:broader))
      (assert-true (> (length (slotv-inverse subject #$skos:subject)) 100)))))
