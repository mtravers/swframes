(in-package :sw)

(use-package :lisp-unit)

(define-test intern
    (assert-eq (intern-uri "blither")
	       (intern-uri "blither")))

(define-test rename
  (let ((x (uri "test27"))
	(y (uri "test28")))
    (assert-true (frame-fresh? x nil))
    (awhen (frame-named "test27renamed")
	   (delete-frame it))
    (rename-frame x "test27renamed")
    (assert-true (frame-fresh? x nil))
    (assert-equal (frame-uri x) "test27renamed")
    (assert-error t (rename-frame y "test27renamed"))
    ))

(define-test inverses 
    (let ((x (uri "test88"))
	  (y (uri "test89"))
	  (p (uri "hasProp")))
      (setf (slotv x p) y)
      (assert-true (member x (slotv-inverse y p)))
      (delete-frame x)
      (assert-false (member x (slotv-inverse y p)))
      ))

(define-test namespaces
    (sw-register-namespace "test" "http://collabrx.com/test")
  (assert-equal "http://collabrx.com/testfoo"
		(expand-uri "test:foo"))
  (assert-eq (intern-uri "test:foo")
	     (intern-uri "http://collabrx.com/testfoo"))
  (unregister-namespace "test")
  (assert-false (eq (intern-uri "test:foo")
		    (intern-uri "http://collabrx.com/testfoo")))
  )


;;; +++ test dependency delete
