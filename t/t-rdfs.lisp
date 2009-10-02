(in-package :sw)

(define-test rdfs-basic
    (assert-error t (rdfs-make-instance #$crx:NotAClass))
  (rdfs-def-class #$crx:TestClass ())
  (let ((i (rdfs-make-instance #$crx:TestClass #$crx:slot "foo")))
    (write-frame i)
    (assert-equal (list i)
		  (rdfs-find "foo" :class #$crx:TestClass :slot  #$crx:slot))
    (destroy-frame i)
    ))


(define-test rdfs-classp
    (rdfs-def-class #$crx:TestClass1 ())
  (rdfs-def-class #$crx:TestClass2 (#$crx:TestClass1))
  (let ((i1 (rdfs-make-instance #$crx:TestClass1 #$crx:slot "foo"))
	(i2 (rdfs-make-instance #$crx:TestClass2 #$crx:slot "bar")))
    (assert-true (rdfs-classp i1 #$crx:TestClass1))
    (assert-true (rdfs-classp i2 #$crx:TestClass2))
    (assert-true (rdfs-classp i2 #$crx:TestClass1))
    (assert-false (rdfs-classp i1 #$crx:TestClass2))
    ))
