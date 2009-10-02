(in-package :swframes)

;;; was colliding with s-xml.  Actually, why don't I just reuse it, it's about the same code.  
(defparameter *sw-namespaces* nil)

(defun register-namespace (abbrev full &optional force?)
  (aif (member abbrev *sw-namespaces* :key #'car :test #'string-equal)
       (unless (equal (cadr (car it)) full)
	 (if force?
	     (progn (deletef abbrev *sw-namespaces* :key #'car :test #'string-equal)
		    (push (list abbrev full) *sw-namespaces*))
	     (warn "Attempt to redefine namespace ~A from ~A to ~A" abbrev (cadr (car it)) full)
	     ))
       (push (list abbrev full) *sw-namespaces*)))

(defun unregister-namespace (abbrev)
  (deletef abbrev *sw-namespaces* :test #'equal :key #'car))

;;; Use this in code
(defmacro def-namespace (abbrev full)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-namespace ,abbrev ,full)))

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
       uri)
      ((null namespace)
       (if no-error
	   uri
	   (error "Unknown namespace ~A" prefix)))
      ((eq namespace :uri-scheme)
       uri)
      (t (string+ namespace (subseq uri (1+ colonpos)))))))


(defun namespace-lookup (namespace)
  (find namespace *sw-namespaces* :key #'car :test #'string-equal))

(defun namespace-expand (namespace)
  (cadr (namespace-lookup namespace)))

(defun expand-uri-0 (ns string)
  (let ((namespace (namespace-lookup ns)))
    (if namespace
	(string+ (cadr namespace) string)
	(error "No namespace ~A" ns))))

;;; Define known real (non-abbreviated) schemas
(register-namespace "http" :uri-scheme)
(register-namespace "urn" :uri-scheme)
(register-namespace "bnode" :uri-scheme) ;for rdf-file parsing
(register-namespace "nodeID" :uri-scheme) ;+++ temp: for Virtuoso blank nodes

;;; Generate headers for SPARQL (not currently used)
(defun sparql-namespace-prefix (&optional abbrevs)
  (format nil "~:{~%PREFIX ~A: <~A>~}" *sw-namespaces*))
