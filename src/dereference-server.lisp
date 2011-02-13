(in-package :sw)

;;; +=========================================================================+
;;; | Copyright (c) 2009, 2010  Mike Travers and CollabRx, Inc                |
;;; |                                                                         |
;;; | Released under the MIT Open Source License                              |
;;; |   http://www.opensource.org/licenses/mit-license.php                    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  Mike Travers

#|
Generate RDF-XML from Frames and serve as linked data.

Can be consumed by R, for instance (see below):

Of course what exactly gets included with a linked-data query is undefined, as far as I can tell.

Todo:
Should provide n3 also?
Get rid of temporary namespaces.  Seems wrong that we need to make them

Usage:
(start-server :port 8002)		;or wherete

HTTP GET http://localhost:8002/dereference?uri=<frame-uri>
 returns RDF XML that describes <frame-uri>

This is a standin for proper dereferencable URLs, but ought to be fine for now

From R:
library(Rredland)      # http://www.bioconductor.org/packages/bioc/html/Rredland.html
rdf = readRDF('http://10.1.1.113:8002/dereference?uri=http://collabrx.com/rdf/bioblog/Entry/99/4318')
as(rdf, "data.frame")

|#

(defparameter *dereference-path* "/dereference")

;;; State vars
(defvar *dereference-namespaces*)
(defvar *dereference-frames*)
(defvar *dereference-depth* 3)

(net.aserve:publish :path *dereference-path*
		    :function 'dereference-server)

(defun dereference-server (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent) 
      (let* ((uri (net.aserve:request-query-value "uri" req))
	     (frame (make-frame uri)))
	(s-xml:print-xml (frame-description-xml frame) 
			 :stream net.aserve::*html-stream*
			 :pretty t)))))

(defun frame-description-xml (frame)
  (let ((*dereference-depth* 3)
	(*dereference-namespaces* (list "rdf"))
	(*dereference-frames* nil)
	(*sw-namespaces* *sw-namespaces*) ;intent of this is to revert away all the temp namespaces
	)
    (let ((xml (frame-description-xml-1 frame))
	  (namespace-terms 
	   (mt:collecting
	    (dolist (ns *dereference-namespaces*)
	      (mt:collect (string+ "xmlns:" ns))
	      (mt:collect (namespace-expand ns))))))
      `((:|rdf:RDF| ,@namespace-terms)
	,xml)
      )
    ))

(defun frame-description-xml-1 (frame)
  (let ((*dereference-depth* (1- *dereference-depth*)))
    `((:|rdf:Description| :|rdf:about| ,(frame-uri frame))
       ,@(when (expand-frame? frame)
	       (push frame *dereference-frames*)
	       (fill-frame frame)
	       (mt:collecting
		(for-frame-slots (frame slot value)
				 (dolist (elt value)
				   (mt:collect (slot-description-xml slot elt))
				   )))
	       ))))

(defun expand-frame? (f)
  (and (> *dereference-depth* 0)
       (not (member f *dereference-frames*))))

;;; Re: types, see: http://www.w3.org/TR/2001/REC-xmlschema-2-20010502/#built-in-datatypes
(defun slot-description-xml (slot value)
  (let ((datatype 
	 (typecase value
	   (fixnum "http://www.w3.org/2001/XMLSchema#int")
	   (float "http://www.w3.org/2001/XMLSchema#double")
	   (t nil)
	   )))
    `((,(frame-xml-tag slot) ,@(when datatype `(:|rdf:datatype| ,datatype)))
      ,(cond ((frame-p value)
	      (frame-description-xml-1 value)) 
	     (t
	      (princ-to-string value))))))

(defun frame-xml-tag (frame)
  (multiple-value-bind (uri namespace) (abbreviate-uri (frame-uri frame))
    (unless (and namespace
		 (not (position #\/ uri))
		 )
      (setf namespace (generate-namespace frame)
	    uri (abbreviate-uri (frame-uri frame))) ;redo
      )
    (pushnew namespace *dereference-namespaces* :test #'equal)
    uri))	

(defun generate-namespace (frame)
  (let* ((uri (frame-uri frame))
	 (slashpos (position #\/ uri :from-end t))
	 (ns (string (gensym "NS"))))
    (register-namespace ns (subseq uri 0 (1+ slashpos)))
    ns
    )
  )


;;; testing only
(defun dereference-local (frame)
  (dereference-1 frame (string+ "http://localhost:8002" *dereference-path* "?uri=" (frame-uri frame) )))
