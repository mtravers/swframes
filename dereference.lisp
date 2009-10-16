(in-package :swframes)

(export '(dereference))

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
dereferences things en masse and brings them into a local store...

Here we'll keep track of some of the available data sources:

; This one works at least some of the time.
; #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugs/DB00022

;#$http://dbpedia.org/page/Aminophyllinen
;  returns HTML with embedded RDFa(?) but it can't be XML parsed.
; But you can substitute in
; (dereference #$http://dbpedia.org/resource/Panitumumab)
; and apparently:
;  http://dbpedia.org/data/Panitumumab.rdf
;  http://dbpedia.org/data/Panitumumab.n3


;;; Times out
;(dereference #$http://bio2rdf.org/proteinlinks/cas:317-34-0)

Many linked data sets here:
http://esw.w3.org/topic/TaskForces/CommunityProjects/LinkingOpenData/DataSets

http://www.rdfabout.com/rdf/usgov/sec/id/cik0001308161
- Returns XML prefaced by two garbage chars (fixed by improving adjust-sparql-string)
- Has some blank nodes, so deal with them now.

http://www.rdfabout.com/rdf/usgov/geo/us

- Returns headers like this:
<?xml version="1.0" encoding="UTF-8"?>

<?xml-stylesheet type="text/xsl" href="http://sw.opencyc.org/xsl/OpenCycOWLCollectionDisplayLatest.xsl"?>

<!DOCTYPE rdf:RDF [
     <!ENTITY ocyc "http://sw.opencyc.org/concept/" >
     <!ENTITY cyc  "http://sw.cyc.com/concept/" >
     <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#" >

Which our XML parser can't handle.  Forget it.

http://www4.wiwiss.fu-berlin.de/bookmashup/books/006251587X
- Works!

#$http://rdf.freebase.com/ns/en.blade_runner
- Works!
- unfortunately freebase doesn't seem to provide a SPARQL endpoint, sigh.

#$diseasome:diseasome/diseases
- Works (not any more)


NOT WORKING
#$http://www.bbc.co.uk/music/artists/5f6ab597-f57a-40da-be9e-adad48708203#artist

returns some RDF but it gets applied to the wrong frame. Namespace problem?


#$http://wiki.rkbexplorer.com/id/resist
-- gets entity error because of that annoying syntax.

 #$http://www.geonames.org/2950159/about.rdf
-- NS-2 error

http://data.linkedmdb.org/all/director
-- gets data but ins't handled proplery, mot suure why...
  ah, its data is not about itself!  Odd.

|#


(defpackage :|rdf|)

;;; An incomplete parser of RDF/XML

(defmethod dereference ((frame frame) &optional force?)
  #.(doc
     "Dereference FRAME if it hasn't been done already or if FORCE? is set"
     "Dereferencing (also known as \"linked data\") is Semweb jargon for taking a URI, treating at as a URL, and going to the server"
     "to get some useful information (such as the triples it participates in)."
     "See http://www4.wiwiss.fu-berlin.de/bizer/pub/LinkedDataTutorial/ for more information")
  (when (or force? (not (frame-dereferenced? frame)))
    (when (string-prefix-equals (frame-uri frame) "http")
      (dereference-1 frame)
      (setf (frame-dereferenced? frame) t)))
  frame)

(defmethod dereference-1 ((frame frame))
  (handler-case 
      (multiple-value-bind (body response-code response-headers uri)
	  ;; turns out this processes the 303 redirect without any further intervention
	  (net.aserve::with-timeout-local (15 (error "timeout dereferencing ~A" frame))
	    (get-url (frame-uri frame) :accept "application/rdf+xml"))
	#+:CCL (declare (ccl::ignore-if-unused response-headers uri))
;;;	(print `(response-code ,response-code response-headers ,response-headers ,uri))
	(unless (= response-code 200)
	  (error "Failed to dereference ~A, response code ~A" frame response-code))
	(let ((xml (parse-xml body)))
	  (process-rdf-xml xml)))
    ;; +++ actually could parse rdf out of html if we were ambitious
    (s-xml:xml-parser-error (e)
      (declare (ignore e))
      (warn "Attempt to dereference ~A got non-XML response" frame)
      nil)
    ;; +++ deal with 404
    (error (e)
      (warn "Unexpected error ~A while dereferencing ~A" e frame)
      nil)))

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

(defun process-rdf-xml (xml &key base)
  (assert xml)
  ;; base can be set as an argument or from the header attributes
  (let ((top-frames nil))
    (labels ((symbol->frame (symbol)
	       (intern-uri (translate-symbol symbol)))
             (add-value (v frame slot)
               (add-triple frame slot v)
               )
             ;; +++ probably wants to be pulled out, this is a fundamental piece of RDF unfortunately
             (make-blank-node (type)
               (intern-uri (format nil "bnode:~A" (gensym (symbol-name type)))))
             (process-description (desc &optional top)
;;;	       (print `(process-description ,desc))
               (let* ((about0 (or (lxml-attribute desc '|rdf|::|about|)
                                  (and (lxml-attribute desc '|rdf|::|ID|)
                                       base
                                       (string+ base "#" (lxml-attribute desc '|rdf|::|ID|)))))
                      (about (if about0
                                 (intern-uri about0)
                                 (make-blank-node (lxml-tag desc))
                                 )))
                 (when top
                   (push about top-frames)
;;;		   (print `(about ,about))
                   )
                 (unless (eq (lxml-tag desc) '|rdf|::|Description|)
                   (add-value (symbol->frame (lxml-tag desc)) about (symbol->frame '|rdf|::|type|)))
                 (dolist (elt (lxml-all-subelements desc))
                   ;; stupid dbpedia defines namespaces in the element they it is used in!
                   (do ((rest (lxml-attributes elt) (cddr rest)))
                       ((null rest))
                     (when (string-prefix-equals (symbol-name (car rest)) "xmlns")
                       (register-namespace (cadr (string-split (symbol-name (car rest)) #\:  ))
                                              (cadr rest))))
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
;;;      (print `(ns ,(car namespaces) ,(cadr namespaces)))
		 (let* ((splits (string-split (string (car namespaces)) #\:  ))
			(com (car splits))
			(ns (cadr splits))
			(full (cadr namespaces)))
		   (cond ((equal com "xmlns")
			  (register-namespace ns full))
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


