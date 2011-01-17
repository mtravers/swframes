(in-package :sw)

;;; test basic mechanics of short names
(define-test short-names
  (defclass$ #$sw:account ()
    :name
    :amount)
  (let ((instance (make-instance$ #$sw:account :name "Piggy Bank" :amount 102.34)))
    (assert-equal (ssv instance :amount) 102.34)))
