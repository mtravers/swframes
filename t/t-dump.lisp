(in-package :sw)

(define-test nt-dump-test
    (let* ((file "/tmp/users.nt")
	   (users (rdfs-find :all :class #$crx:bioblog/User :fill? t)) ;MMM
	   (writer (make-instance 'nt-writer :file file)))
      (dump-frames writer users)
      ;+++ test that file was written and is non-zero lenght
      ))

      
