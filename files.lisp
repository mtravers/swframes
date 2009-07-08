(in-package :sw)

;;; You can load frames from files (RDF/OWL)

(defclass* file-frame-source (frame-source) 
  (file)
  :initable-instance-variables)

;;; no-op 
(defmethod fill-frame-from ((frame frame) (source file-frame-source) &key inverse?)
  (declare (ignore frame source inverse?))
  )

(defun parse-rdf-xml-file (file)
  (let ((*default-frame-source* (make-instance 'file-frame-source :file file)))
    (process-rdf-xml (s-xml:parse-xml-file file))))

(defun parse-owl-file (file)
  (parse-rdf-xml-file file))		;for now

(defun owl-file-to-virtuoso (file graph)
  (let ((frames (parse-owl-file file))
	(writer (make-instance 'sparql-endpoint
			       :uri (sparql-endpoint-uri *collabrx-sparql*)
			       :writeable? t
			       :write-graph graph)))
    (dolist (f frames)
      (write-frame f writer))))


#|
Tests

Small
(parse-owl-file "/misc/kbs/swan.owl")

Big
Fails because of !!!entitites
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

(owl-file-to-virtuoso "/misc/kbs/swan.owl" "http://collabrx.com/graphs/swan")
(local-term-query "Journal Article")	;see if it wrote it...


|#
