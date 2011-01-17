(in-package :sw)

(defvar *sw-test-data-dir*
   (make-pathname :directory (append (pathname-directory *load-pathname*) '("data"))))

(define-test parse-owl-file
    (let ((frames (parse-owl-file (merge-pathnames "swan.owl" *sw-test-data-dir*))))
      (assert-true (frame-p (car frames)))
      (assert-true (typep (frame-source (car frames)) 'file-frame-source)) 
      ))

#|
Tests (+++ turn these into actual unit tests)


Fails because of !!!entitites  Need to extend parser, blah
(parse-owl-file "/misc/downloads/so.owl")

Patch this into xml.lisp, long term fix is to smarten up the parser
  ~/.sbcl/site/s-xml/src/xml.lisp (+++)

(defun make-standard-entities ()
  "A hashtable mapping XML entity names to their replacement strings,
  filled with the standard set"
  (let ((entities (make-hash-table :test #'equal)))
    (setf (gethash "amp" entities) (string #\&)
	  (gethash "quot" entities) (string #\")
	  (gethash "apos" entities) (string #\')
	  (gethash "lt" entities) (string #\<)
	  (gethash "gt" entities) (string #\>)
	  (gethash "nbsp" entities) (string #\space))
    (add-special-entities entities 
			  '(("so" "http://purl.org/sswap/SO/")
			    ("ro" "http://purl.org/sswap/RO/")
			    ("sofa" "http://purl.org/sswap/SOFA/")
			    ("owl" "http://www.w3.org/2002/07/owl#")
			    ("oboInOwl" "http://purl.org/sswap/oboInOwl/")
			    ("p2" "http://purl.org/sswap/RO/ro.owl#")
			    ("xsd" "http://www.w3.org/2001/XMLSchema#")
			    ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
			    ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
			    ("p1" "http://purl.org/sswap/oboInOwl/oboInOwl.owl#")
			    ))
    entities))

(defun add-special-entities (ht ents)
  (dolist (e ents)
    (setf (gethash (car e) ht) (cadr e))))

;;; try this (works with new sparql limit hack). But easier to load through virtuoso.
(with-write-group (*default-frame-source*)
  (dolist (s (cdr sos))
    (write-frame s :source *default-frame-source* :no-delete? t)))

(owl-file-to-virtuoso "/misc/kbs/swan.owl" "http://swframes.org/graphs/swan")
(local-term-query "Journal Article")	;see if it wrote it...


(parse-rdf-xml-file "/misc/kbs/go/go_200904-termdb.rdf-xml")

|#
