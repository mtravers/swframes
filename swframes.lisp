(in-package :swframes)

#|
An RDF-backed frame system

Idle thoughts:
- could wire this into CLOS in the manner of ActiveRDF.


|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-frame-source* nil))

;;; Growing an API here...
(export '(uri
	  *default-frame-source* *mark-new-frames-loaded?*
	  frame frame-name frame-named frame-label
	  %frame-slots %frame-inverse-slots frame-empty?
	  reset-frames for-all-frames all-frames
	  fill-frame fill-frame-inverse
	  slotv slotv-inverse
	  svf svif
	  add-triple
	  rename-frame delete-frame
	  describe-frame
	  sw-register-namespace))

(defun frame-name (frame)
  (abbreviate-uri (frame-uri frame)))  

(defun frame-label (frame)
  (or (best-string (slotv frame (intern-uri "http://www.w3.org/2000/01/rdf-schema#label")))
      (frame-name frame)))

;; try to find the english...this is simplistic and probably implementation dependent
(defun best-string (list)
  (or (dolist (elt list)
	(if (typep elt '(SIMPLE-BASE-STRING 12))
	    (return elt)))
      (car list)))

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

;;; Does not remove all references to frame (it could, I suppose, if we were rigorous about inverses III)
(defmethod delete-frame ((frame frame))
  (reset-frame frame)
  (unintern-uri (frame-uri frame)))

;;; debugging
(defun frame-fresh? (frame &optional (error? t))
  (unless (eq frame (intern-uri (frame-uri frame)))
    (if error?
	(error "~A is stale" frame)
	(return-from frame-fresh? nil)))
  t)

(defmethod fill-frame-inverse ((frame frame))
  (fill-frame frame))

(defmethod fill-frame ((frame frame) &key force?)
  (when (or force? (not (frame-loaded? frame)))
    (if (frame-source frame)
	(progn
	  (fill-frame-sparql frame)
	  (fill-frame-inverse-sparql frame))
	(mt:report-and-ignore-errors	;+++
	 (dereference frame)))
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

(defun frame-empty? (frame)
  (and (null (%frame-slots frame))
       (null (%frame-inverse-slots frame))))

(defmethod fill-frame-sparql ((frame frame) &optional source)
    (let ((*default-frame-source* (or source
				      (frame-source frame)
				      *default-frame-source*)))
      (dolist (binding (do-sparql 
			*default-frame-source*
			(format nil "select ?p ?o where { <~A> ?p ?o . }" (frame-uri frame))))
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
  (frame-fresh? frame)
  (frame-fresh? slot)
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

;;; convenience, analagous to #^ (which is now implemented)
(defun svf (slot)
  #'(lambda (x) (slotv x slot)))

(defun svif (slot)
  #'(lambda (x) (slotv-inverse x slot)))


;;; Experimenting with an extention of slot semantics (in use by #^ now)
(defmethod msv ((frame frame) slot)
  (slotv frame slot))

(defmethod msv ((frames list) slot)
  (let ((result nil))
    (dolist (f frames result)
      ;; warning: depends on nunion only being destructive to its FIRST argument
      (setf result (nunion result (slotv f slot))))))

;;; this is really what we should use, I suppose
(defun add-triple (s p o &key (test #'eql))
  (frame-fresh? s)			;+++ do this under a safety switch, here and elsewhere
  (frame-fresh? p)
  (if (frame-p 0) (frame-fresh? o))
  (pushnew o (slotv s p) :test test)
  (if (frame-p o)
      (pushnew s (slotv-inverse o p) :test test))
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


