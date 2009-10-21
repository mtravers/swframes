(in-package :sw)

#|
Idea: generate RDF-XML from Virtuoso and serve as linked data.
We would need to redo our URI schemas to use rdf.collabrx.com rather than collabrx.com, and arrange some kind of remap in Apache.

In the interim, set this up as a normal http service

Of course what exactly gets included with a linked-data query is undefined, as far as I can tell.

Security?

|#

(defparameter *uri-prefix* "http://rdf.collabrx.com/")
(defparameter *dereference-path* "/dereference")

;;; State vars
(defvar *dereference-namespaces*)
(defvar *dereference-frames*)
(defvar *dereference-depth* 3)

(net.aserve:publish :path *dereference-path*
		    :function 'dereference-server)

(defun dereference-server (req ent)
  (wb::with-http-response-and-body (req ent) 
    (let* ((uri (net.aserve:request-query-value "uri" req))
	   (frame (make-frame uri)))
      (s-xml:print-xml (frame-description-xml frame) 
		       :stream net.aserve::*html-stream*
		       :pretty t))))

(defun frame-description-xml (frame)
  (let ((*dereference-depth* 3)
	(*dereference-namespaces* nil)
	(*dereference-frames* nil))
    (frame-description-xml-1 frame)
    ))


(defun frame-description-xml-1 (frame)
  (let ((*dereference-depth* (1- *dereference-depth*)))
    `((:|rdf:RDF|
	:|xmlns:rdf| "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      ((:|rdf:Description| :|rdf:about| ,(frame-uri frame))
       ,@(when (expand-frame? frame)
	       (push frame *dereference-frames*)
	 (mt:collecting
	  (for-frame-slots (frame slot value)
			   (dolist (elt value)
			     (mt:collect (slot-description-xml slot elt))
			     )))
	       )
       ))))

(defun expand-frame? (f)
  (and (> *dereference-depth* 0)
       (not (member f *dereference-frames*))))


(defun slot-description-xml (slot value)
  (let ((datatype 
	 (typecase value
	   (fixnum "http://www.w3.org/2001/XMLSchema#long")
	   (float "http://www.w3.org/2001/XMLSchema#double")
	   (t nil)
	   )))
    `((,(frame-xml-tag slot) ,@(when datatype `((|rdf|:|datatype| ,datatype))))
      ,(cond ((frame-p value)
	      (frame-description-xml-1 value)) ;+++ need to do some depth calc
	     (t
	      (princ-to-string value))))
    ))


(defun frame-xml-tag (frame)
  (multiple-value-bind (uri namespace) (abbreviate-uri (frame-uri frame))
    (unless namespace
      (setf namespace (generate-namespace frame)
	    uri (abbreviate-uri (frame-uri frame))) ;redo
      )
    (pushnew namespace *dereference-namespaces*)
    uri
    ))	

(defun generate-namespace (frame)
  (let* ((uri (frame-uri frame))
	 (slashpos (position #\/ uri :from-end t))
	 (ns (string (gensym "NS"))))
    (register-namespace ns (subseq uri 0 (1+ slashpos)))
    )
  )


#|
	   `((|rdf|:RDF :|xmlns:rdf| "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	     ((|corp|:|Company| |rdf|:|about|
		     ,(frame-uri frame)
|#
