(in-package :sw)

(use-package :lisp-unit)

(define-test deref-test
    (delete-frame #$http://dbpedia.org/resource/Panitumumab)
  (let ((f (intern-uri "http://dbpedia.org/resource/Panitumumab")))
    (dereference f)
    (assert-true (slotv f #$rdfs:comment))
    ;; test transitive dereference (+++ not presently working)
    (let ((subject (car (slotv f #$skos:subject))))
      (assert-true subject)
      (assert-true (slotv subject #$skos:broader))
      (assert-true (> (length (slotv-inverse subject #$skos:subject)) 100)))))

(defun frames-equal (f1 f2)
  (let ((s1 (ht-contents (frame-slots f1)))
	(s2 (ht-contents (frame-slots f2))))
    (and (null (set-difference s1 s2 :test #'equal))
	 (null (set-difference s2 s1 :test #'equal)))
    ))

(defun frame-diff (f1 f2)
  (let ((s1 (ht-contents (frame-slots f1)))
	(s2 (ht-contents (frame-slots f2))))
    (print "in first, not second:")
    (print (set-difference s1 s2 :test #'(lambda (a b) (tree-equal a b :test #'equal))))
    (print "in second not first:")
    (print (set-difference s2 s1 :test #'(lambda (a b) (tree-equal a b :test #'equal)) ))))

;;; +++ fails because of integer/string issues
(define-test deref-round-trip
  (let ((f (gen-random-frame))
	ff fff xml sxml pxml)
    (setf ff (frame-copy f))
    ;; sanity check equality tester
    (assert-true (frames-equal f ff))

    (setf xml (frame-description-xml f))
    (setf sxml (s-xml:print-xml-string xml :pretty t))
    (delete-frame f)
    (setf pxml (parse-xml sxml))
    (setf fff (car (process-rdf-xml pxml)))
    (assert-true (frames-equal ff fff))))
