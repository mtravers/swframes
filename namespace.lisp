(in-package :swframes)

;;; was colliding with s-xml.  Actually, why don't I just reuse it, it's about the same code.  
(defparameter *sw-namespaces* nil)

(defun sw-register-namespace (abbrev full &optional force?)
  (aif (member abbrev *sw-namespaces* :key #'car :test #'string-equal)
       (unless (equal (cadr (car it)) full)
	 (if force?
	     (progn (deletef abbrev *sw-namespaces* :key #'car :test #'string-equal)
		    (push (list abbrev full) *sw-namespaces*))
	     (warn "Attempt to redefine namespace ~A from ~A to ~A" abbrev (cadr (car it)) full)
	     ))
       (push (list abbrev full) *sw-namespaces*)))

;;; Use this in code
(defmacro def-namespace (abbrev full)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sw-register-namespace ,abbrev ,full)))

(defun unregister-namespace (abbrev)
  (deletef abbrev *sw-namespaces* :test #'equal :key #'car))

(defun abbreviate-uri (uri)
  (dolist (namespace *sw-namespaces*)
    (let ((full (cadr namespace)))
      (when (and (not (eq full :uri-scheme))
		 (>= (length uri) (length full))
		 (string= uri full :end1 (length full)))
	(return-from abbreviate-uri (values (format nil "~A:~A" (car namespace) (subseq uri (length full)))
					    (car namespace)
					    )))))
  (values uri nil))

;;; Define known real (non-abbreviated) schemas
(sw-register-namespace "http" :uri-scheme)
(sw-register-namespace "urn" :uri-scheme)
(sw-register-namespace "bnode" :uri-scheme) ;for rdf-file parsing
(sw-register-namespace "nodeID" :uri-scheme) ;+++ temp: for Virtuoso blank nodes

(defun namespace-lookup (namespace)
  (find namespace *sw-namespaces* :key #'car :test #'string-equal))

(defun namespace-expand (namespace)
  (cadr (namespace-lookup namespace)))

(defvar *default-namespace* nil)	;+++ was crx, but we need to have prefixless namespace for GO

(defun expand-uri (uri &optional no-error)
  (let* ((colonpos (position #\: uri))
	 (prefix (and colonpos
		     (subseq uri 0 colonpos)))
	 (namespace (and prefix (namespace-expand prefix))))
    (cond 
      ((zerop (length uri))
       "")
      ;; special case for parsing BioPax
      ((char= (char uri 0) #\#)
       (string+ (namespace-expand "NS-0") (subseq uri 1)))
      ((null prefix)
       (if *default-namespace*
	   (string+ (namespace-expand *default-namespace*) uri)
	   uri))
      ((null namespace)
       (if no-error
	   uri
	   (error "Unknown namespace ~A" prefix)))
      ((eq namespace :uri-scheme)
       uri)
      (t (string+ namespace (subseq uri (1+ colonpos)))))))


(defun expand-uri-0 (ns string)
  (let ((namespace (namespace-lookup ns)))
    (if namespace
	(string+ (cadr namespace) string)
	(error "No namespace ~A" ns))))

;;; Some standard namespaces
(defparameter *standard-namespaces*
  '(;; SemWeb infrastructure
    ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
    ("xsd" "http://www.w3.org/2001/XMLSchema#")
    ("owl" "http://www.w3.org/2002/07/owl#")

    ;; Common schemas
    ("dc" "http://purl.org/dc/terms/")
    ("foaf" "http://xmlns.com/foaf/0.1/")
    ("skos" "http://www.w3.org/2004/02/skos/core#")

    ;; Linked Data related
    ("linkedct" "http://data.linkedct.org/resource/")
    ("linkedct" "http://data.linkedct.org/resource/linkedct/")
    ("d2r" "http://sites.wiwiss.fu-berlin.de/suhl/bizer/d2r-server/config.rdf#")
    ("dbpedia" "http://dbpedia.org/property/")
    ("drugbank" "http://www4.wiwiss.fu-berlin.de/drugbank/resource/")
    ("dailymed" "http://www4.wiwiss.fu-berlin.de/dailymed/resource/")
    ("diseasome" "http://www4.wiwiss.fu-berlin.de/diseasome/resource/")

    ;; Other
    ("nci" "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#")
    ("go" "http://www.geneontology.org/go#")
    ("go1" "http://www.geneontology.org/dtds/go.dtd#") ;slots use this
    ("bp" "http://www.biopax.org/release/biopax-level2.owl#")

    ;; Local
    ("crx" "http://collabrx.com/rdf/")

    ))

(dolist (n *standard-namespaces*)
  (sw-register-namespace (car n) (cadr n) t))

;;; Generate headers for SPARQL (not currently used)
(defun sparql-namespace-prefix (&optional abbrevs)
  (format nil "~:{~%PREFIX ~A: <~A>~}" *sw-namespaces*))
