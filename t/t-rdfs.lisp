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
