(in-package :swframes)

#|
Dereferencing is Semweb jargon for looking at a URI, inferring from it a server, and going to that server
to get some useful information (such as triples it participates in).  This is a highly under-specified and
under-implemented technique, sadly.

Some servers respond with HTML containing RDFa encoded information. Unfortunately the XML parser
can't usually deal with typical syntactically sloppy HTML.  

Dereferencing piecemeal is going to be too slow.  Probably this should be part of a crawler that
dereferences things en masse and brings them into a local store...


Here we'll keep track of 

; This one works at least some of the time.
; #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugs/DB00022
; this stuff is weird, it contains mostly back links rather than forward links.  Sigh.

;#$http://dbpedia.org/page/Aminophylline
;  returns HTML with embedded RDFa(?) but it can't be XML parsed.
; But you can substitute in 
; (dereference #$http://dbpedia.org/resource/Panitumumab)
; and apparently: 
;  http://dbpedia.org/data/Panitumumab.rdf
;  http://dbpedia.org/data/Panitumumab.n3


;;;
;(dereference #$http://bio2rdf.org/proteinlinks/cas:317-34-0)


http://www.rdfabout.com/rdf/usgov/sec/id/cik0001308161

Returns XML prefaced by two garbage chars.



|#


(defpackage :|rdf|)

;;; An incomplete parser of RDF/XML

(defmethod dereference ((frame frame))
  (unless (frame-dereferenced? frame)
    (dereference-1 frame)
    (setf (frame-dereferenced? frame) t)))

(defmethod dereference-1 ((frame frame))
  (multiple-value-bind (body response-code response-headers uri)
      ;; turns out this processes the 303 redirect without any further intervention
      (utils:get-url (frame-uri frame) :accept "application/rdf+xml")
    (let* (; (s-xml::*ignore-namespaces* t)
	   (xml (s-xml:parse-xml-string (knewos::adjust-sparql-string body))))
      (labels ((symbol->frame (symbol)
	       (let ((ns (package-name (symbol-package symbol)))
		     (text (symbol-name symbol)))
		 (intern-uri (expand-uri-0 ns text) )))
	       (add-value (v frame slot)
		 (add-triple frame slot v)
		 )
	       (process-description (desc)
		 (let* ((about (intern-uri (lxml-attribute desc '|rdf|::|about|))))
		   (assert about)	;+++ could be blank node...in which case we are somewhat fucked.
		   (unless (eq (lxml-tag desc) '|rdf|::|Description|)
		     (add-value (symbol->frame (lxml-tag desc)) about (symbol->frame '|rdf|::|type|)))
		   (dolist (elt (lxml-all-subelements desc))
		     (let ((property (symbol->frame (lxml-tag elt)))
			   )
		       (acond ((lxml-attribute elt '|rdf|::|resource|)
			       (add-value (intern-uri it) about property))
			      ((symbolp elt)
			       (warn "Empty elt ~A" elt))
			      ((stringp (cadr elt))
			      ;; no resource, so a literal? or another description?
			       (add-value (cadr elt) about property))
			      (t (dolist (sub (lxml-all-subelements elt))
				   (add-value (process-description sub) about property)))
;			     (t (error "Cant figure out what to do with ~A" elt))
			      )
		       ))
		   about)))
	(assert (name-eq '|rdf|::RDF (car (car xml))))
	(do ((namespaces (cdr (car xml)) (cddr namespaces)))
	    ((null namespaces))
	  (let ((ns (cadr (utils:string-split (string (car namespaces)) #\:  )))
		(full (cadr namespaces)))
	    (sw-register-namespace ns full)))
      (dolist (desc (lxml-all-subelements xml))
	(process-description desc))
      xml))))
  

