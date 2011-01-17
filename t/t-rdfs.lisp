(in-package :sw)

(define-test rdfs-basic
    (assert-error t (make-instance$ #$sw:NotAClass))
  (defclass$ #$sw:TestClass ())
  (let ((i (make-instance$ #$sw:TestClass #$sw:slot "foo")))
    (write-frame i)
    (assert-equal (list i)
		  (rdfs-find "foo" :class #$sw:TestClass :slot  #$sw:slot))
    (destroy-frame i)
    ))


(define-test rdfs-classp
    (defclass$ #$sw:TestClass1 ())
  (defclass$ #$sw:TestClass2 (#$sw:TestClass1))
  (let ((i1 (make-instance$ #$sw:TestClass1 #$sw:slot "foo"))
	(i2 (make-instance$ #$sw:TestClass2 #$sw:slot "bar")))
    (assert-true (rdfs-classp i1 #$sw:TestClass1))
    (assert-true (rdfs-classp i2 #$sw:TestClass2))
    (assert-true (rdfs-classp i2 #$sw:TestClass1))
    (assert-false (rdfs-classp i1 #$sw:TestClass2))
    ))

(def-namespace "ftc" "http://collabrx.com/frametestcase/")

;;; Tests that lists can be passed as args to make-instance$ and range checking happens properly
(def-namespace "ftc" "http://collabrx.com/frametestcase/")

(define-test randy
  (defclass$ #$ftc:Thing ())
  (defclass$ #$ftc:ThingSet ()
		  (#$ftc:slots/things :range #$ftc:Thing))
  (let ((things (loop for i from 1 to 3 collect (make-instance$ #$ftc:Thing))))
    (make-instance$ #$ftc:ThingSet #$ftc:slots/things things)))
