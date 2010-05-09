(in-package :sw)

(define-test rdfs-basic
    (assert-error t (rdfs-make-instance #$sw:NotAClass))
  (rdfs-def-class #$sw:TestClass ())
  (let ((i (rdfs-make-instance #$sw:TestClass #$sw:slot "foo")))
    (write-frame i)
    (assert-equal (list i)
		  (rdfs-find "foo" :class #$sw:TestClass :slot  #$sw:slot))
    (destroy-frame i)
    ))


(define-test rdfs-classp
    (rdfs-def-class #$sw:TestClass1 ())
  (rdfs-def-class #$sw:TestClass2 (#$sw:TestClass1))
  (let ((i1 (rdfs-make-instance #$sw:TestClass1 #$sw:slot "foo"))
	(i2 (rdfs-make-instance #$sw:TestClass2 #$sw:slot "bar")))
    (assert-true (rdfs-classp i1 #$sw:TestClass1))
    (assert-true (rdfs-classp i2 #$sw:TestClass2))
    (assert-true (rdfs-classp i2 #$sw:TestClass1))
    (assert-false (rdfs-classp i1 #$sw:TestClass2))
    ))

(def-namespace "ftc" "http://collabrx.com/frametestcase/")

;;; Tests that lists can be passed as args to rdfs-make-instance and range checking happens properly
(def-namespace "ftc" "http://collabrx.com/frametestcase/")

(define-test randy
  (rdfs-def-class #$ftc:Thing ())
  (rdfs-def-class #$ftc:ThingSet ()
		  (#$ftc:slots/things :range #$ftc:Thing))
  (let ((things (loop for i from 1 to 3 collect (rdfs-make-instance #$ftc:Thing))))
    (rdfs-make-instance #$ftc:ThingSet #$ftc:slots/things things)))
