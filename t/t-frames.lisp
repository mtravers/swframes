
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


;;; slot equality is set equality
(defun slotv-equal (a b)
  (if (and (listp a) (listp b))
      (set-equal a b)
      (equal a b)))

;;; Works fine for in-memory
;;; write to db tests work IFF you declare slot special.  Maybe that should be the default.
(define-test basic-slot
    (let ((f (gen-test-frame))
	  (s (gen-test-frame "slot")))
      (declare-special-slot s #$crx:slots/LispValueSlot)
      (labels ((test-slot (v)
		 (test-slot-1 v nil)
		 (test-slot-1 v t))
	       (test-slot-1 (v db)
		 (if (listp v)
		     (progn
		       (setf (slotv f s) v)
		       (when db (forget))
		       (assert-true (slotv-equal v (slotv f s) )))
		     (assert-error 'error (setf (slotv f s) v)))
		 (setf (msv f s) v)
		 (when db (forget))
		 (assert-true (slotv-equal v (msv f s) ))
		 (setf (ssv f s) v)
		 (when db (forget))
		 (assert-true (slotv-equal v (ssv f s) ))
		 )
	       (forget ()
		 (write-frame f :source *default-frame-source*)
		 (reset-frame f)
		 (fill-frame f)))
	(test-slot 23)
	(test-slot "foo")
	(test-slot '(a b c))
	(test-slot t)
	(test-slot nil)
	(test-slot #$foobar)
	(test-slot (list #$foobar #$barfoo))
	;; clean up after ourselves
	(destroy-frame f)
	)))

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
      (setf (ssv x p) y)
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


;;; Slot accessors


	
			     
