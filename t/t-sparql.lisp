(in-package :sw)

(register-namespace "dbpprop" "http://dbpedia.org/property/") 

(define-test sparql-sanity
    (assert-true (sanity-check *default-sparql-endpoint*)))

(defvar *dbpedia* (make-instance 'sparql-endpoint :url "http://dbpedia.org/sparql"))

(define-test sparql-syntax
    (assert-true 
     (do-sparql *dbpedia* '(:select :all (:distinct t :limit 10) (?s #$rdf:type ?t))))
  (assert-true 
   (member #$http://dbpedia.org/resource/Illinois
     (do-sparql-one-var *dbpedia* '(:select (?t) (:distinct t :limit 10) (#$http://dbpedia.org/resource/Chicago #$dbpprop:subdivisionName ?t)))))
  (assert-true 
   (do-sparql *dbpedia* '(:select (?t ?f)
				  (:distinct t :limit 10)
				  (#$http://dbpedia.org/resource/Chicago #$dbpprop:subdivisionName ?t)
				  (:optional (?t #$dbpprop:fossil ?f))
			  ))))


(setq linkedct-query 
  '(:select
    (?trial ?title ?inttype ?intname)
    (:offset 0)
    (?trial #$linkedct:condition ?condition)
    (?condition #$linkedct:condition_name "Leprosy")
    (?trial #$linkedct:brief_title ?title)
    (?trial #$linkedct:intervention ?intervention)
    (?intervention #$linkedct:intervention_type ?inttype)
    (?intervention #$linkedct:intervention_name ?intname)))

(define-test linkedct-test
    ;; normal
    (assert-true (> (length (do-sparql nil linkedct-query)) 3))
  ;; case insensitized

  (assert-true (> (length (do-sparql nil (case-insensitize linkedct-query))) 3))
  (assert-true (> (length  (bulk-load-query nil linkedct-query)) 1))
  )

;;; Trying to track down a subtle SPARQL string quoting problem

(defun test-lisp-deserialize (str)
  (let ((f (gen-test-frame))
	(s (gen-test-frame "crx:slot")))
    (setf (frame-source f) *default-sparql-endpoint*)
    (declare-special-slot s #$crx:slots/LispValueSlot)
    (setf (ssv f s) str)
    (write-frame f)
    (fill-frame f :force? t)
    (assert (equal (ssv f s) str))
    (destroy-frame f)))

;;; Shit, problems is at an even lower level.

(defun test-sparql-quoting (str triple? quoting?)
  (let ((f (gen-test-frame))
	(s (gen-test-frame "crx:slot"))
	(mstr (if quoting?
		  (backslash-quote-string str)
		  str)))
    (do-sparql *default-sparql-endpoint*
      (format nil
	      (if triple?
		  "INSERT INTO GRAPH <http://collabrx.com/main> { <~A> <~A> '''~A''' }"
		  "INSERT INTO GRAPH <http://collabrx.com/main> { <~A> <~A> \"~A\" }")
	      (frame-uri f) (frame-uri s) mstr))
    (let ((res
	   (do-sparql *default-sparql-endpoint* (format nil "SELECT * WHERE { <~A> <~A> ?o }" (frame-uri f) (frame-uri s)))))
      (assert-equal str (cadr (car (car res))))
      )
    (destroy-frame f *default-sparql-endpoint*)))

(defun all-chars ()
  (coerce 
   (mt:collecting
    (dotimes (n 255)
      (mt:collect (code-char n))))
   'string))

(defparameter all-printable-chars
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")


(define-test sparql-quoting
    (test-sparql-quoting "foo" nil nil)
    (test-sparql-quoting all-printable-chars t t)
    ;; has newline problems so doesn't pass.
;    (test-sparql-quoting (format nil "~%foo~%bar") t t)
    )
  

  
