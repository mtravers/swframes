
(in-package :sw)

(use-package :lisp-unit)

(defun gen-test-frame (&optional (root "test"))
  (make-frame (formatn "~A~A" root (gensym)) :source *code-source*))

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
  (let ((x (gen-test-frame))
	(y  (gen-test-frame)))
    (assert-true (frame-fresh? x nil))
    (awhen (frame-named "test27renamed")
	   (delete-frame it))
    (rename-frame x "test27renamed")
    (assert-true (frame-fresh? x nil))
    (assert-equal (frame-uri x) "test27renamed")
    (assert-error t (rename-frame y "test27renamed"))
    ))

(define-test inverses 
    (let ((x (gen-test-frame))
	  (y (gen-test-frame))
	  (p (gen-test-frame "hasProp")))
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


;;; Slots -- note that this is generally fucked.

(define-test basic-slot
    (let ((f (gen-test-frame))
	  (s (gen-test-frame "slot")))
      (flet ((test-slot (v)
	       (setf (slotv f s) v)
;	       (assert-equal (slotv f s) v )
	       (setf (msv f s) v)
	       (assert-equal (msv f s) v )
	       (setf (ssv f s) v)
	       (assert-equal (ssv f s) v )
	       ))
	(test-slot 23)
	(test-slot "foo")
	(test-slot '(a b c))
	(test-slot nil)
	(test-slot #$foobar))))
	
			     
