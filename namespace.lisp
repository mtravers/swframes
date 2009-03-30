(in-package :swframes)

;;; was colliding with s-xml.  Actually, why don't I just reuse it, it's about the same code.  
(defparameter *sw-namespaces* nil)

(defun sw-register-namespace (abbrev full)
  (aif (member abbrev *sw-namespaces* :key #'car :test #'string-equal)
       (unless (equal (cadr (car it)) full)
	 (warn "Attempt to redefine namespace ~A from ~A to ~A" abbrev (cadr (car it)) full))
       (push (list abbrev full) *sw-namespaces*)))

(defun abbreviate-uri (uri)
  (dolist (namespace *sw-namespaces*)
    (let ((full (cadr namespace)))
      (when (and (>= (length uri) (length full))
		 (string= uri full :end1 (length full)))
	(return-from abbreviate-uri (values (format nil "~A:~A" (car namespace) (subseq uri (length full)))
					    (car namespace)
					    )))))
  (values uri nil))

(defun namespace-lookup (namespace)
  (find namespace *sw-namespaces* :key #'car :test #'string=))

(defun namespace-expand (namespace)
  (cadr (namespace-lookup namespace)))

(defun expand-uri (uri)
  (let ((colonpos (position #\: uri)))
    (if colonpos
	(let ((namespace (namespace-lookup (subseq uri 0 colonpos))))
	  (if namespace
	      (format nil "~A~A" (cadr namespace) (subseq uri (1+ colonpos)))
	      uri))
	uri)))

(defun expand-uri-0 (ns string)
  (let ((namespace (namespace-lookup ns)))
    (if namespace
	(format nil "~A~A" (cadr namespace) string)
	(error "No namespace ~A" ns))))

;;; Some standards
(defparameter *standard-namespaces*
  '(("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
    ("xsd" "http://www.w3.org/2001/XMLSchema#")
    ("owl" "http://www.w3.org/2002/07/owl#")

    ("dc" "http://purl.org/dc/terms/")
    ("foaf" "http://xmlns.com/foaf/0.1/")
    ("skos" "http://www.w3.org/2004/02/skos/core#")

    ("db" "http://data.linkedct.org/resource/")
    ("linkedct" "http://data.linkedct.org/resource/linkedct/")
    ("d2r" "http://sites.wiwiss.fu-berlin.de/suhl/bizer/d2r-server/config.rdf#")
    ("dbpedia" "http://dbpedia.org/property/")
    ("drugbank" "http://www4.wiwiss.fu-berlin.de/drugbank/")))

(dolist (n *standard-namespaces*)
  (sw-register-namespace (car n) (cadr n)))
 
