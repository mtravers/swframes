(in-package :sw)

;;; test
(define-test basic-versioning
  (let ((f (gen-test-frame))
	(s (gen-test-frame "slot")))
    (setf (ssv f s) "foo")
    (write-frame-versioned f)
    (setf (ssv f s) "bar")
    (write-frame-versioned f)
    (assert-equal '("bar" "foo")
		  (mapcar (ssv-accessor s) (frame-version-history f)))
    (destroy-frame f)
    (destroy-frame s)))
