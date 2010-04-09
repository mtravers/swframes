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

;;; Tests that lists can be passed as args to rdfs-make-instance and range checking happens properly
(def-namespace "ftc" "http://collabrx.com/frametestcase/")

(define-test randy
  (rdfs-def-class #$ftc:Thing ())
  (rdfs-def-class #$ftc:ThingSet ()
		  (#$ftc:slots/things :range #$ftc:Thing))
  (let ((things (loop for i from 1 to 3 collect (rdfs-make-instance #$ftc:Thing))))
    (rdfs-make-instance #$ftc:ThingSet #$ftc:slots/things things)))
