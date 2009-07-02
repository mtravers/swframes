(in-package :sw)

(define-test rdfs-basic
    (assert-error t (rdfs-make-instance #$crx:NotAClass))
  (rdfs-def-class #$crx:TestClass ())
  (let ((i (rdfs-make-instance #$crx:TestClass)))
    (write-frame i)))
