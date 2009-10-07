
(in-package :sw)

(use-package :lisp-unit)

(defun gen-test-frame (&optional (root "test"))
  (make-frame (format nil "~A~A" root (gensym)) :source *code-source*))

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
      (set-equal a b #'equal)
      (equal a b)))

#|
Regular slots handle strings, fixnums, and frames
Lisp slots handle any printable Lisp object, 
   storing fixnums natively (+++ test for this)
|#

(defun normal-slot-element? (x)
  (typecase x
    (number t)
    (string t)
    (frame t)
    (otherwise nil)))

(defmacro assert-slotv-equal (expected form)
  `(assert-true (slotv-equal ,expected ,form)))

(define-test basic-slot
    (let ((f (gen-test-frame))
	  (s (gen-test-frame "slot"))
	  (ls (gen-test-frame "lslot")))
      (declare-special-slot ls #$crx:slots/LispValueSlot)
      (labels ((test-slot (v normal lisp)
		 (when normal
		   (test-slot-1 s v nil)
		   (test-slot-1 s v t))
		 (when lisp
		   (test-slot-1 ls v nil)
		   (test-slot-1 ls v t)))
	       (test-slot-1 (s v db)
;		 (print `(test ,v ,s ,db))
		 (when db (forget))
		 (setf (msv f s) v)
		 (assert-slotv-equal v (msv f s))
		 (when (normal-slot-element? v)
		   (setf (ssv f s) v)
		   (when db (forget))
		   (assert-slotv-equal v (ssv f s)))
		 (when (listp v)
		   (setf (slotv f s) v)
		   (when db (forget))
		   (assert-slotv-equal v (slotv f s)))
		 (forget)
		 )
	       (forget ()
		 (write-frame f :source *default-frame-source*)
		 (reset-frame f)
		 (fill-frame f)))
	(test-slot 23 t t)
	(test-slot "foo" t t)
	(test-slot #$foobar t t)
	(test-slot 'a nil t)
	(test-slot t nil t)
	(test-slot nil nil t)
 	(test-slot (list #$foobar #$barfoo) t t)
 	(test-slot '("what" "nonsense") t t)
 	(test-slot '("what" #$blither 23) t t)
 	(test-slot '(a b c) nil t)
	;; clean up after ourselves
	(destroy-frame f)
	)))

(define-test unwriteable 
    (let ((f (gen-test-frame))
	  (s (gen-test-frame "slot")))
      (setf (slotv f s) (list #'(lambda () (not 'serializable))))
      (assert-error 'error
		    (write-frame f))
      (delete-frame f)
      (delete-frame s)))

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
    (register-namespace "test" "http://collabrx.com/test")
  (assert-equal "http://collabrx.com/testfoo"
		(expand-uri "test:foo"))
  (assert-eq (intern-uri "test:foo")
	     (intern-uri "http://collabrx.com/testfoo"))
  (unregister-namespace "test")
  (assert-error 'error (intern-uri "test:foo")))


;;; +++ test dependency delete


			     
#|
Old stuff

Tests:
(setq f1 (make-frame 
	   :source "http://data.linkedct.org/sparql" 
	   :uri "http://data.linkedct.org/resource/trials/NCT00696657"))

(fill-sframe f1)
(setq f2 (car (slotv f1 (intern-uri "http://data.linkedct.org/resource/linkedct/location")))
(fill-sframe f2)


(defvar *bio2df-server* (make-sparql-source "http://lod.openlinksw.com/sparql"))

(describe-sframe (intern-uri "http://data.linkedct.org/resource/trials/NCT00123435"))

;;; Test inverse
(add-triple #$a #$has #$b)
(assert (member #$b (slotv-inverse #$b #$has)))

|#

