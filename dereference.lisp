(in-package :swframes)

(export '(dereference))

;;; Singleton class to represent frames obtained through dereferencing.

(defclass dereference-source (frame-source) 
  ())

(defvar *dereference-source* (make-instance 'dereference-source))

;;; Done in memory, so nothing more to do
;;; +++ alternative: make this an error.
(defmethod delete-triple ((source dereference-source) s p o &key write-graph)
  )

(defmethod write-triple ((source dereference-source) s p o &key write-graph)
  )

(defmethod fill-frame-from (frame (source dereference-source) &key inverse?)
  (dereference frame t)
  )

;;; sort-of back compatability
(defun frame-dereferenced? (frame)
  (eq *dereference-source*
      (frame-source frame)))

;;;

;;; try to standardize the call to this, since there are a few random things we need to do often.
(defun parse-xml (source)
  (let* (; (s-xml::*ignore-namespaces* t)
	 #+:CCL
	 ;; fixes a nasty bug where tags lose their namespaces if their symbol is defined in CL!
	 ;; not sure how to deal with this in other Lisps.
	 (ccl:*make-package-use-defaults* nil)
	 ) 
    (s-xml:parse-xml-string (adjust-sparql-string source))))

#|
Dereferencing is Semweb jargon for looking at a URI, inferring from it a server, and going to that server
to get some useful information (such as triples it participates in).  This is a highly under-specified and
under-implemented technique, sadly.

Some servers respond with HTML containing RDFa encoded information. Unfortunately the XML parser
can't usually deal with typical syntactically sloppy HTML.

Dereferencing piecemeal is going to be too slow.  Probably this should be part of a crawler that
dereferences things en masse and brings them into a local store.
|#

(defpackage :|rdf|)

(defmethod dereference ((frame frame) &optional force?)
  #.(doc
     "Dereference FRAME if it hasn't been done already or if FORCE? is set"
     "Dereferencing (also known as \"linked data\") is Semweb jargon for taking a URI, treating at as a URL, and going to the server"
     "to get some useful information (such as the triples it participates in)."
     "See http://www4.wiwiss.fu-berlin.de/bizer/pub/LinkedDataTutorial/ for more information")
  (when (or force? (not (frame-dereferenced? frame)))
    (when (string-prefix-equals (frame-uri frame) "http")
      (dereference-1 frame)))
  frame)

(defmethod dereference-1 ((frame frame) &optional (url (frame-uri frame)))
  (handler-case 
      (multiple-value-bind (body response-code response-headers uri)
	  ;; turns out this processes the 303 redirect without any further intervention
	  (net.aserve::with-timeout-local (15 (error "timeout dereferencing ~A" frame))
	    (get-url url :accept "application/rdf+xml"))
	#+:CCL (declare (ccl::ignore-if-unused response-headers uri))
;;;	(print `(response-code ,response-code response-headers ,response-headers ,uri))
	(unless (= response-code 200)
	  (error "Failed to dereference ~A, response code ~A" frame response-code))
	(let ((xml (parse-xml body)))
	  (process-rdf-xml xml :source *dereference-source*)))
    ;; +++ actually could parse rdf out of html if we were ambitious
    (s-xml:xml-parser-error (e)
      (declare (ignore e))
      (warn "Attempt to dereference ~A got non-XML response" frame)
      nil)
    ;; +++ deal with 404
    (error (e)
      (warn "Unexpected error ~A while dereferencing ~A" e frame)
      nil)
    ))

;;; An incomplete parser of RDF/XML
;;; can get RSS feeds, ie
(defun process-rdf-url (url)
  (multiple-value-bind (body response-code response-headers uri)
      ;; turns out this processes the 303 redirect without any further intervention
      (net.aserve::with-timeout-local (15 (error "timeout dereferencing ~A" url))
        (get-url url))
    #+:CCL (declare (ccl::ignore-if-unused response-headers uri))
;;;    (print `(response-code ,response-code response-headers ,response-headers ,uri))
    (unless (= response-code 200)
      (error "Failed to dereference ~A, response code ~A" url response-code))
    (let ((xml (parse-xml body)))
      (process-rdf-xml xml))))

;;; undo some s-xml damage (translates its NS-2 style names back into uris)
(defun translate-symbol (identifier)
  ;; weird bug in xml parser results in SEQUENCE instead of bp:sequence, ie:
  ;; (get-pathways #$http://cbio.mskcc.org/cpath#CPATH-71202)
  ;; +++ kludge around it because I don't have time to fix it.
  (when (eq identifier 'cl:sequence)
    (return-from translate-symbol (expand-uri "bp:sequence")))
  (let* ((package (symbol-package identifier))
	 (name (symbol-name identifier))
	 (namespace (find package s-xml::*known-namespaces* :key #'s-xml:get-package))
	 (namespace-uri (and namespace (s-xml::get-uri namespace))))
    (unless namespace
      (error "Unknown XML namespspace for ~A" identifier))
    (string+ namespace-uri name)))

(defun process-rdf-xml (xml &key base (source *default-frame-source*))
  (assert xml)
  ;; base can be set as an argument or from the header attributes
  (let ((top-frames nil))
    (labels ((->frame (thing)
	       (when (symbolp thing)
		 (setf thing (translate-symbol thing)))
	       (intern-uri thing :source source))
             (add-value (v frame slot)
;;;	       (print `(add-triple ,frame ,slot ,v))
               (add-triple frame slot v)
               )
             ;; +++ probably wants to be pulled out, this is a fundamental piece of RDF unfortunately
             (make-blank-node (type)
               (->frame (format nil "bnode:~A" (gensym (symbol-name type)))))
	     ;; no idea if this is to spec...
	     (full-uri (thing)
	       (cond ((position #\: thing)
		      thing)
		     ((and base (char= (aref thing 0) #\#))
		      (string+ base thing))
		     (base (string+ base "#" thing))
		     (t (error "Can't turn ~A into full uri" thing))))
             (process-description (desc &optional top)
;;;	       (print `(process-description ,desc))
               (let* ((about0 (or (lxml-attribute desc '|rdf|::|about|)
                                  (and (lxml-attribute desc '|rdf|::|ID|)
                                       base
                                       (full-uri (lxml-attribute desc '|rdf|::|ID|)))))
                      (about (if (and about0 (not (equal "" about0)))
                                 (->frame about0)
                                 (make-blank-node (lxml-tag desc))
                                 )))
                 (when top
                   (push about top-frames)
;;;		   (print `(about ,about))
                   )
                 (unless (eq (lxml-tag desc) '|rdf|::|Description|)
                   (add-value (->frame (lxml-tag desc)) about (->frame '|rdf|::|type|)))
                 (dolist (elt (lxml-all-subelements desc))
                   ;; stupid dbpedia defines namespaces in the element they it is used in!
                   (do ((rest (lxml-attributes elt) (cddr rest)))
                       ((null rest))
                     (when (string-prefix-equals (symbol-name (car rest)) "xmlns")
                       (register-namespace (cadr (string-split (symbol-name (car rest)) #\:  ))
					   (cadr rest))))
                   (let ((property (->frame (lxml-tag elt)))
                         )
                     (acond ((lxml-attribute elt '|rdf|::|resource|)
                             (add-value (->frame (full-uri it)) about property))
                            ((symbolp elt)
                             (warn "Empty elt ~A" elt))
                            ((stringp (cadr elt))
                             ;; no resource, so a literal? or another description?
			     (let ((datatype (lxml-attribute elt '|rdf|::|datatype|))
				   (value (cadr elt)))
			       (when datatype (setq datatype (intern-uri datatype))) ;need to do this because namespaces apply
;;;			       (print `(value ,value datataype ,datatype))
			       ;; +++ highly incomplete list of datatypes, but enough to get unit test working.
			       ;; see: http://www.w3.org/TR/2001/REC-xmlschema-2-20010502/#built-in-datatypes
			       (cond ((null datatype))
				     ((member datatype '(#$xsd:integer #$xsd:int #$xsd:long #$xsd:double #$xsd:float))
				      (setq value (read-from-string value)))
				     ((member datatype '(#$xsd:string)))
				     (datatype ;good to know about
				      (warn "Dereference found unknown datatype ~A" datatype)
				      ))
			       (add-value value about property)))
                            (t (dolist (sub (lxml-all-subelements elt))
                                 (add-value (process-description sub) about property)))
                                        ;                            (t (error "Cant figure out what to do with ~A" elt))
                            )
                     ))
                 about))
	     ;; may be entirely unnecessary now that we've figured out what s-xml does
	     (process-namespaces (xml)
	       (do ((namespaces (and (listp (car xml))
				     (cdr (car xml)))
				(cddr namespaces)))
		   ((null namespaces))
		 (let* ((splits (string-split (string (car namespaces)) #\:  ))
			(com (car splits))
			(ns (cadr splits))
			(full (cadr namespaces)))
;		   (print `(ns2 ,com ,ns ,full))
		   (cond ((equal com "xmlns")
			  (if (null ns)
			      (setq base full)
			      (register-namespace ns full)))
			 ((equal com "base")
			  (setq base full))
			 (t (error "Don't know what to do with ~A" (car namespaces))))))))
      (process-namespaces xml)
      (if (name-eq '|rdf|::RDF (lxml-tag xml))
	  (progn
	    (dolist (desc (lxml-all-subelements xml))
	      (process-description desc t))
	    (unregister-namespace "NS-0")
	    (dolist (f top-frames)
	      (setf (frame-loaded? f) t))
	    (nreverse top-frames))
	  ;; RDF not at top level, try walking in
	  (process-rdf-xml 
	   (lxml-find-element xml '|rdf|::RDF)
	   :base base))
	  )))


