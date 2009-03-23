(in-package :swframes)

#|
An RDF-backed frame system

Idle thoughts:
- could wire this into CLOS in the manner of ActiveRDF.


|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-frame-source* nil))

(defun frame-name (frame)
  (abbreviate-uri (frame-uri frame)))  

(defun frame-named (name)
  (intern-uri (expand-uri name)))

;; hook into old code, including listener
(defun frames::frame-fnamed (name &optional force?)
  (declare (ignore force?))
  (frame-named name))

(defun frame-label (frame)
  (or (car (slotv frame #$http://www.w3.org/2000/01/rdf-schema#label nil))
      (frame-name frame)))

;;; names should be reversed
(defun %frame-slots (frame)
  (wlisp::hash-keys (frame-slots frame)))

(defun %frame-inverse-slots (frame)
  (wlisp::hash-keys (frame-inverse-slots frame)))

(defun reset-frames ()
  (clrhash *uri->frame-ht*))

(defmacro for-all-frames ((var) &body body)
  `(maphash #'(lambda (uri ,var)
		(declare (ignore uri))
		,@body)
	    *uri->frame-ht*))

(defun all-frames ()
  (collecting (for-all-frames (f) (utils::collect f))))

;;; +++
(defmethod reset-frame ((frame frame))
  (clrhash (frame-slots frame))
  (setf (frame-loaded? frame) nil)
  (setf (frame-inverse-slots frame) nil))

;;; debugging
(defun frame-fresh? (frame)
  (unless (eq frame (intern-uri (frame-uri frame)))
    (error "~A is stale" frame))
  t)

;;; +++ namespace hackery (steal LSW)
(defun frame-uri-namespaced (frame)
  (frame-uri frame))			;not yet



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


;;; Try via dereferencing
(defmethod fill-sframe ((frame frame) &key force?)
  (when (or force? (not (frame-loaded? frame)))
    (if (ignore-errors
	  (dereference frame))
	(setf (frame-loaded? frame) t)
	(warn "Attempt to dereference ~A failed" frame))
      ;; +++ inverses))
    ))
		   
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
  
(defvar *fill-by-default?* nil)



(defmethod slotv ((frame frame) (slot frame) &optional (fill? *fill-by-default?*))
  (frame-fresh? frame)
  (frame-fresh? slot)
  ;;+++ need a theory of this.
  (if fill? (fill-sframe frame))
  (gethash slot (frame-slots frame)))

(defsetf slotv set-slotv)

(defmethod set-slotv ((frame frame) (slot frame) value)
  (setf (gethash slot (frame-slots frame)) value))

(defmethod slotv-inverse ((frame frame) (slot frame))
;  (fill-sframe-inverse frame)
  (and (frame-inverse-slots frame)
       (gethash slot (frame-inverse-slots frame))))

(defsetf slotv-inverse set-slotv-inverse)

(defmethod set-slotv-inverse ((frame frame) (slot frame) value)
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-hash-table :test #'eq)))
  (setf (gethash slot (frame-inverse-slots frame)) value))

;;; this is really what we should use, I suppose
(defun add-triple (s p o)
  (pushnew o (slotv s p))
  (if (frame-p o)
      (pushnew s (slotv-inverse o p)))
  nil)					;makes tracing saner

;;; temp -- these probably want to be objects
(defun make-sparql-source (endpoint)
  endpoint)

;;; bulk fill +++
;;; query (sexpy sparql syntax from lsw) ___

(defun describe-sframe (frame)
;  (fill-sframe frame)
  (princ "Forward:")
  (pprint (mt:ht-contents (frame-slots frame)))
;  (fill-sframe-inverse frame)
  (when (frame-inverse-slots frame)
    (princ "Inverse:")
    (pprint (mt:ht-contents (frame-inverse-slots frame))))
  )

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

