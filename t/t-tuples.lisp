(in-package :sw)

;;; when there are other tuple implementations, repeat these tests for them
(define-test basic-tuple
    (let ((tuple (make-tuple)))
      (setf (tuple-field tuple :foo) 23)
      (assert-eq 23 (tuple-field tuple :foo))
      (assert-true (member :foo (tuple-fields tuple)))
      (tuple-remove-field tuple :foo)
      (assert-true (null (tuple-field tuple :foo)))))

;;; when there are other tuple-set implementations, repeat these tests for them
(define-test basic-tset
    (let* ((tset (make-instance 'list-tuple-set))
	   (tup (tset-make-tuple tset)))
      (setf (tuple-field tup :foo) 23)
      (assert-equal 1 (tset-count tset))
      (let ((list (tset-tuple-list tset)))
	(assert-equal 1 (length list)))))

(define-test file-tset
    (let ((ts (make-instance 'file-tuple-set :file "/misc/downloads/relationships.tsv"))
	  ti tup subseq)
      (assert-equal 5 (length (tset-fields ts)))
      (assert-true (member :|RelatedGenesDrugsDiseases| (tset-fields ts)))
      (setq ti (tset-iterator ts))
      (setq tup (tset-iterator-next ti))
      (assert-equal "PA646313" (tuple-field tup :|PharmGKBAccessionId|))
      (setq subseq (tset-subseq ts 20 10))
      (assert-equal 10 (length subseq))
      (assert-true (tuples-equal? (car (tset-subseq ts 20 10))
				  (car (tset-subseq ts 20 10))))      
      ))

(define-test tset-writer
    (setq writer (make-instance 'tset-file-writer :tset ts :file "/tmp/tset.tsv"))
  (write-tset writer))

(define-test tset-file-copy-and-index
    (setq ts (make-instance 'file-tuple-set :file "/misc/downloads/relationships.tsv"))
    (setq ts1 (make-instance 'in-memory-tuple-set))
  (tset-copy ts ts1)
  (add-index ts1 :|PharmGKBAccessionId|)
  (assert-true (lookup-tuples ts1 :|PharmGKBAccessionId| "PA152408100")))

(define-test tset-lookup-slow
    (setq ts (file-to-memory-tset "/misc/working/rnb/crx1/patients/pn1/data/EA08034_2009_03_27_Summary.txt"))
  (assert-true (> (length (lookup-tuples-slow ts #'(lambda (tup) (let ((v (tuple-field tup :|PValue_85993|))) (and v (< v .05))))))
		  2)))

