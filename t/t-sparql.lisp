(in-package :sw)

(register-namespace "dbpprop" "http://dbpedia.org/property/") 

(define-test sparql-sanity
    (assert-true (sanity-check *default-frame-source*)))

(defvar *dbpedia* (make-instance 'sparql-endpoint :url "http://dbpedia.org/sparql"))

(define-test sparql-syntax
    (assert-true 
     (do-sparql *dbpedia* '(:select :all (:distinct t :limit 10) (?s #$rdf:type ?t))))
  (assert-true 
   (member #$http://dbpedia.org/resource/Area_code_312
     (do-sparql-one-var *dbpedia* '(:select (?t) (:distinct t :limit 10) (#$http://dbpedia.org/resource/Chicago #$dbpprop:areaCode ?t)))))
  (assert-true 
   (do-sparql *dbpedia* '(:select (?t ?f)
				  (:distinct t :limit 10)
				  (#$http://dbpedia.org/resource/Chicago #$dbpprop:subdivisionName ?t)
				  (:optional (?t #$dbpprop:fossil ?f))
			  ))))


;;; +++ or is there another "linkedct/" on the end?
(register-namespace "linkedct" "http://data.linkedct.org/resource/linkedct/")

(defvar linkedct-query 
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
  (let ((linkedct-source (make-instance 'sparql-endpoint :url "http://data.linkedct.org/sparql")))
    ;; normal
    (assert-true (> (length (do-sparql linkedct-source linkedct-query)) 3))
    ;; case insensitized
    ;; ++ failing, query is returning nil
    (assert-true (> (length (do-sparql linkedct-source (case-insensitize linkedct-query))) 3))
    (assert-true (> (length  (bulk-load-query nil linkedct-query)) 1))
    ))

;;; +++ clean this mess up
;;; Trying to track down a subtle SPARQL string quoting problem

(defun test-lisp-deserialize (str)
  (let ((f (gen-test-frame))
	(s (gen-test-frame "sw:slot")))
    (setf (frame-source f) *default-frame-source*)
    (declare-special-slot s #$sw:slots/LispValueSlot)
    (setf (ssv f s) str)
    (write-frame f)
    (reset-frame f)
    (fill-frame f :force? t)
    (assert-equal (ssv f s) str)
    (destroy-frame f)))

;;; Shit, problem is at an even lower level.

(defun test-sparql-quoting (str triple? quoting?)
;  (test-lisp-deserialize str)
  (let ((f (gen-test-frame))
	(s (gen-test-frame "sw:slot"))
	(mstr (if quoting?
		  (backslash-quote-string str)
		  str)))
    (do-sparql *default-frame-source*
      (format nil
	      (if triple?
		  "INSERT INTO GRAPH <http://collabrx.com/main> { <~A> <~A> '''~A''' }"
		  "INSERT INTO GRAPH <http://collabrx.com/main> { <~A> <~A> \"~A\" }")
	      (frame-uri f) (frame-uri s) mstr))
    (let ((res
	   (do-sparql *default-frame-source* (format nil "SELECT * WHERE { <~A> <~A> ?o }" (frame-uri f) (frame-uri s)))))
      (assert-equal str (cadr (car (car res))))
      )
    (destroy-frame f *default-frame-source*)))

(defparameter all-printable-chars
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(define-test sparql-quoting
    (test-sparql-quoting "foo" nil nil)
    (test-sparql-quoting all-printable-chars t t)
    ;; has newline problems so doesn't pass.
    (test-sparql-quoting (format nil "~%foo~%bar") t t)
    )
  

  

#|
Here's a list of endpoints:
http://esw.w3.org/SparqlEndpoints

(discover-classes (make-instance 'sparql-endpoint :url "http://api.talis.com/stores/periodicals/services/sparql") )

;;; Returns JSON!
(discover-classes (make-instance 'sparql-endpoint :url "http://zbw.eu/beta/sparql"))

;;; times out
(discover-classes (make-instance 'sparql-endpoint :url "http://lod.openlinksw.com/sparql/") :limit 20)

;;; works,
(discover-classes (make-instance 'sparql-endpoint :url "http://services.data.gov.uk/transport/sparql") :limit 20)



|#
