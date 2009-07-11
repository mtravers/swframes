(in-package :sw)

(use-package :lisp-unit)

(define-test intern
    (assert-eq (intern-uri "blither")
	       (intern-uri "blither")))

(define-test frame-reader
    (let ((f1 (read-from-string "#$frame0"))
	  (f2 (read-from-string "#$frame0"))
	  (fun1 (read-from-string "#^frame0"))
	  (inv1 (read-from-string "#vframe0")))
      (assert-true (frame-p f1))
      (assert-eq f1 f2)
      (assert-false (eq f1 fun1))
      (assert-false (eq fun1 inv1))
      ))


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
  (assert-error 'error (intern-uri "test:foo")))


;;; +++ test dependency delete
