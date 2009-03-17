(in-package :swframes)

#|
An RDF-backed frame system

Idle thoughts:
- could wire this into CLOS in the manner of ActiveRDF.


|#

(defstruct (frame (:print-function frame-printer))
  uri
  source
  loaded?				;T if slots have been loaded
  dirty?				;T if needs to be written back out, or list of preds to write out.
  (slots (make-hash-table :test #'eq)) ;ht mapping preds to values (poss. multiple)
  (inverse-slots nil)
  )

(defun frame-printer (frame stream ignore)
  (mt:report-and-ignore-errors
    (format stream "#$~A" (frame-name frame))))

(defun frame-name (frame)
  (abbreviate-uri (frame-uri frame)))  

;;; reader 
(defun uri (thing)
  (typecase thing
    (frame thing)
    (string (intern-uri (expand-uri thing)))
    (t (error "Can't turn ~A into a URI" thing))))
    
(set-dispatch-macro-character #\# #\$ 'pound-dollar-frame-reader)

;;; +++ would be good to allow #$"sdasdad" for hard to parse names
(defun pound-dollar-frame-reader (stream char arg)
  (declare (ignore char arg))
  (uri (frames::read-fname stream)))

(defvar *default-frame-source* nil)

(defvar *uri->frame-ht* (make-hash-table :test 'equal))

(defun reset-frames ()
  (clrhash *uri->frame-ht*))

;;; +++
(defmethod reset-frame ((frame frame))
  (clrhash (frame-slots frame))
  (setf (frame-loaded? frame) nil)
  (setf (frame-inverse-slots frame) nil))

;;; +++ namespace hackery (steal LSW)
(defun frame-uri-namespaced (frame)
  (frame-uri frame))			;not yet

(defun intern-uri (uri)
  (or (gethash uri *uri->frame-ht*)
      (setf (gethash uri *uri->frame-ht*)
	    (make-frame :uri uri :source *default-frame-source*))))

(defun sparql-binding-elt (binding v)
  (cadr (find v binding :key #'car :test #'equal)))

;;; +++ use new sparql syntax
(defmethod fill-sframe ((frame frame) &key force?)
  (when (or force? (not (frame-loaded? frame)))
    (let ((*default-frame-source* (or (frame-source frame)
				      *default-frame-source*)))
      (dolist (binding (knewos::run-sparql 
			*default-frame-source*
			(format nil "select ?p ?o where { <~A> ?p ?o . }" (frame-uri frame))
			:make-uri #'intern-uri))
	(push (sparql-binding-elt binding "o")
	      (gethash (sparql-binding-elt binding "p") (frame-slots frame)))
	)
      (setf (frame-loaded? frame) t)
      ;; +++ inverses))
      ))
  )
		   
;;; Not sure how we really will handle inverses, so broken out and not very symmetical with forward links for now...might
;;;  what to be done standardly as part of fill
(defmethod fill-sframe-inverse ((frame frame))
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-hash-table :test #'eq))
  (let ((*default-frame-source* (or (frame-source frame)
				    *default-frame-source*)))
    (dolist (binding (knewos::run-sparql 
		      *default-frame-source*
		      (generate-sparql `(:select (?s ?p) () (?s ?p ,frame)))
		      :make-uri #'intern-uri))
      (push (sparql-binding-elt binding "s")
	    (gethash (sparql-binding-elt binding "p") (frame-inverse-slots frame)))
      ))))
  
(defmethod slotv ((frame frame) (slot frame))
  (fill-sframe frame)
  (gethash slot (frame-slots frame)))

(defmethod slotv-inverse ((frame frame) (slot frame))
  (fill-sframe-inverse frame)
  (gethash slot (frame-inverse-slots frame)))


;;; temp -- these probably want to be objects
(defun make-sparql-source (endpoint)
  endpoint)

;;; bulk fill +++
;;; query (sexpy sparql syntax from lsw) ___

(defun describe-sframe (frame)
  (fill-sframe frame)
  (princ "Forward:")
  (pprint (mt:ht-contents (frame-slots frame)))
  (princ "Inverse:")
  (fill-sframe-inverse frame)
  (pprint (mt:ht-contents (frame-inverse-slots frame)))  )

#|
Tests:
(setq f1 (make-frame 
	   :source "http://data.linkedct.org/sparql" 
	   :uri "http://data.linkedct.org/resource/trials/NCT00696657"))

(fill-sframe f1)
(setq f2 (car (slotv f1 (intern-uri "http://data.linkedct.org/resource/linkedct/location")))
(fill-sframe f2)


(defvar *bio2df-server* (make-sparql-source "http://lod.openlinksw.com/sparql"))

(describe-sframe (intern-uri "http://data.linkedct.org/resource/trials/NCT00123435"))

|#


(defun name-eq (s1 s2)
  (equal (symbol-name s1) (symbol-name s2)))

; This one works at least some of the time.
; #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugs/DB00022
; this stuff is weird, it contains mostly back links rather than forward links.  Sigh.
(defmethod dereference ((frame frame))
  (multiple-value-bind (body response-code response-headers uri)
      ;; turns out this processes the 303 redirect
      (utils:get-url (frame-uri frame) :accept "application/rdf+xml")
;    (print (list response-code response-headers uri))
    (let* ((xml (s-xml:parse-xml-string (knewos::adjust-sparql-string body))))
      (assert (name-eq :rdf (car (car xml))))
      (dolist (desc (lxml-subelements xml '|rdf|:|Description|))
	(let ((about (lxml-attribute desc '|rdf|:|about|)))
	  (cond ((not (equal about (frame-uri frame)))
		 (format t "~%Entry about ~A" about)))))
      xml)))
	   

