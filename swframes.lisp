(in-package :swframes)

#|
An RDF-backed frame system

Idle thoughts:
- could wire this into CLOS in the manner of ActiveRDF.


|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-frame-source* nil))

(export '(frame frame-name frame-named frame-label
	  %frame-slots %frame-inverse-slots
	  reset-frames for-all-frames all-frames
	  fill-frame fill-frame-inverse
	  slotv slotv-inverse
	  svf svif
	  add-triple
	  describe-frame))

(defun frame-name (frame)
  (abbreviate-uri (frame-uri frame)))  

(defun frame-named (name)
  (intern-uri (expand-uri name)))

;; hook into old code, including listener
(defun frames::frame-fnamed (name &optional force?)
  (declare (ignore force?))
  (frame-named name))

(defun frames::slotv (frame slot)
  (slotv frame slot))

(defun frames::set-slotv (frame slot value)
  (set-slotv frame slot value))

(defun frames::frame-slots-of (frame)
  (%frame-slots frame))

(defun frame-label (frame)
  (or (car (slotv frame (intern-uri "http://www.w3.org/2000/01/rdf-schema#label")))
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

(defmethod reset-frame ((frame frame))
  (clrhash (frame-slots frame))
  (setf (frame-loaded? frame) nil)
  (setf (frame-inverse-slots frame) nil))

(defmethod delete-frame ((frame frame))
  (reset-frame frame)
  (unintern-uri (frame-uri frame)))

;;; debugging
(defun frame-fresh? (frame)
  (unless (eq frame (intern-uri (frame-uri frame)))
    (error "~A is stale" frame))
  t)

(defmethod fill-frame-inverse ((frame frame))
  (fill-frame frame))

(defmethod fill-frame ((frame frame) &key force?)
  (when (or force? (not (frame-loaded? frame)))
    (if (frame-source frame)
	(progn
	  (fill-frame-sparql frame)
	  (fill-frame-inverse-sparql frame))
	(dereference frame))
    (setf (frame-loaded? frame) t)))
	

#|
;;; via sparql
(defmethod fill-frame ((frame frame) &key force?)
  (when (or force? (not (frame-loaded? frame)))
    (if (ignore-errors
	  (fill-frame-sparql frame)
	  (fill-frame-inverse-sparql frame)
	  t)
	(setf (frame-loaded? frame) t)
	(warn "Attempt to dereference ~A failed" frame))
    ))

;;;  via dereferencing
(defmethod fill-frame ((frame frame) &key force?)
  (when (or force? (not (frame-loaded? frame)))
    (if (ignore-errors
	  (dereference frame)
	  t)
	(setf (frame-loaded? frame) t)
	(warn "Attempt to dereference ~A failed" frame))
    ))
|#

(defmethod fill-frame-sparql ((frame frame) &optional source)
    (let ((*default-frame-source* (or source
				      (frame-source frame)
				      *default-frame-source*)))
      (dolist (binding (do-sparql 
			*default-frame-source*
			(format nil "select ?p ?o where { <~A> ?p ?o . }" (frame-uri frame))))
; replaced with add-triple
;	(pushnew (sparql-binding-elt binding "o")
;		 (gethash (sparql-binding-elt binding "p") (frame-slots frame)))
	(add-triple frame
		    (sparql-binding-elt binding "p")
		    (sparql-binding-elt binding "o"))
	)
      (setf (frame-loaded? frame) t)
      ))

		   
(defmethod fill-frame-inverse-sparql ((frame frame))
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-hash-table :test #'eq))
  (let ((*default-frame-source* (or (frame-source frame)
				    *default-frame-source*)))
    (dolist (binding (do-sparql 
		      *default-frame-source*
		       `(:select (?s ?p) () (?s ?p ,frame))))
; replaced with add-triple
;      (push (sparql-binding-elt binding "s")
;	    (gethash (sparql-binding-elt binding "p") (frame-inverse-slots frame)))
      (add-triple (sparql-binding-elt binding "s") 
		  (sparql-binding-elt binding "p")
		  frame)
      ))))
  
(defvar *fill-by-default?* nil)

;;; optional argument doesn't play well with setf.
(defmethod slotv ((frame frame) (slot frame) &optional (fill? *fill-by-default?*))
  (frame-fresh? frame)
  (frame-fresh? slot)
  (if fill? (fill-frame frame))
  (gethash slot (frame-slots frame)))

(defsetf slotv set-slotv)

(defmethod set-slotv ((frame frame) (slot frame) value)
  ;; enforce rule that slot values are lists...
  (unless (listp value)
    (setf value (list value)))
  (setf (gethash slot (frame-slots frame)) value))

(defmethod slotv-inverse ((frame frame) (slot frame) &optional (fill? *fill-by-default?*))
  (if fill? (fill-frame frame))
  (and (frame-inverse-slots frame)
       (gethash slot (frame-inverse-slots frame))))

(defsetf slotv-inverse set-slotv-inverse)

(defmethod set-slotv-inverse ((frame frame) (slot frame) value)
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-hash-table :test #'eq)))
  (setf (gethash slot (frame-inverse-slots frame)) value))

;;; convenience, analagous to #^
(defun svf (slot)
  #'(lambda (x) (slotv x slot)))

(defun svif (slot)
  #'(lambda (x) (slotv-inverse x slot)))

;;; this is really what we should use, I suppose
(defun add-triple (s p o)
  (pushnew o (slotv s p) :test #'equal)
  (if (frame-p o)
      (pushnew s (slotv-inverse o p) :test #'equal))
  nil)					;makes tracing saner

;;; query (sexpy sparql syntax from lsw) ___

(defun describe-frame (frame &optional (fill? t))
  (when fill? (fill-frame frame))
  (princ "Forward:")
  (pprint (mt:ht-contents (frame-slots frame)))
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

;;; Slow, obviously.
(defun frames-with-value (v &optional slot)
  (collecting
    (for-all-frames (f)
      (block frame
	(if slot
	    (when (member v (slotv f pred) :test #'equal)
	      (utils::collect f)
	      (return-from frame))
	  (dolist (slot (%frame-slots f))
	    (when (member v (slotv f slot) :test #'equal)
	      (utils::collect f)
	      (return-from frame))))))))


