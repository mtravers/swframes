(in-package :sw)

(define-test basic-clos
    (rdfs-def-class #$crx:BigClass ())
  (rdfs-def-class #$crx:SmallClass  (#$crx:BigClass))
  (let* ((ib (rdfs-make-instance #$crx:BigClass))
	 (is (rdfs-make-instance #$crx:SmallClass))
	 (random-frame (gen-test-frame)))
    ;; test that frames have appropriate clos classes and the classes have the right relationship
    (assert-true (not (eq (type-of ib) (type-of random-frame))))
    (assert-true (not (eq (type-of ib) (type-of is))))
    (assert-true (typep is (type-of ib)))
    (assert-true (typep is (type-of random-frame)))))
