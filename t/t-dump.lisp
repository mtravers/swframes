(in-package :sw)

;;; Loses because of finalizations, really should fix that -- with MOP (MMM)
; (rdfs-find :all :class #$http://dbpedia.org/ontology/City :source *dbpedia* :limit 10)

(define-test nt-dump-test
    (let* ((file "/tmp/villages.nt")
	   (villages (rdfs-find :all :class #$http://dbpedia.org/ontology/Village :source *dbpedia* :limit 100 :fill? t))
	   (writer (make-instance 'nt-writer :file file)))
      (assert-true (> (length villages) 0))
      (dump-frames writer villages)
      ;; test that file was written and is non-zero length
      (assert-true (probe-file file))
      (assert-true (> (with-open-file (i file) (file-length i)) 0))
      ))

      
