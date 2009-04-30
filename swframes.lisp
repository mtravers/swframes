(in-package :swframes)

#|
An RDF-backed frame system

Idle thoughts:
- could wire this into CLOS in the manner of ActiveRDF.


|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-frame-source* nil))

;;; Growing an API here...should clean this up, consolidate some stuff
(export '(uri
	  *default-frame-source* *mark-new-frames-loaded?*
	  frame frame-p frame-name frame-named frame-label frame-uri intern-uri
	  most-significant-name 
	  %frame-slots %frame-inverse-slots frame-empty?
	  reset-frames for-all-frames all-frames
	  fill-frame fill-frame-inverse
	  slotv slotv-inverse
	  svf svif
	  msv msv-inverse msv-hack
	  add-triple
	  rename-frame delete-frame write-frame destroy-frame
	  describe-frame
	  sw-register-namespace))

(defun frame-name (frame)
  (abbreviate-uri (frame-uri frame)))  

(defun frame-label (frame)
  (or (best-string (slotv frame (intern-uri "http://www.w3.org/2000/01/rdf-schema#label")))
      (most-significant-name (frame-name frame))
      ))

;;; +++ sometimes you want the part following #
(defun most-significant-name (string)
  (car (last (utils:string-split string #\/))))

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
  (when (frame-inverse-slots frame)
    (wlisp::hash-keys (frame-inverse-slots frame))))

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
;;; Does not delet from db
(defmethod delete-frame ((frame frame))
  (reset-frame frame)
  (unintern-uri (frame-uri frame)))

(defun delete-frames-matching (uri-frag)
  (for-all-frames (f)
		  (if (search uri-frag (frame-uri f))
		      (delete-frame f))))

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
    (let ((*fill-by-default?* nil))	;prevent recursion
      (if (frame-source frame)
	  (fill-frame-from frame (frame-source frame)) ;defaulting (handled in method now)??
	  (mt:report-and-ignore-errors	;+++
	   (dereference frame)))
      (setf (frame-loaded? frame) t))))

(defun frame-empty? (frame)
  (and (null (%frame-slots frame))
       (null (%frame-inverse-slots frame))))


  
(defvar *fill-by-default?* nil)

(defmethod %slotv ((frame frame) (slot frame))
  (gethash slot (frame-slots frame)))  

;;; optional argument doesn't play well with setf.
(defmethod slotv ((frame frame) (slot frame) &optional (fill? *fill-by-default?*))
  (if fill? (fill-frame frame))
  (%slotv frame slot))

(defsetf slotv set-slotv)
(defsetf %slotv set-slotv)

;;; note that this and set-slotv-inverse never do fills
;;; this can't really do inverses, can it? we'd have to a difference...
(defmethod set-slotv ((frame frame) (slot frame) value)
  (let ((old (%slotv frame slot)))
    ;; enforce rule that slot values are lists...
    (unless (listp value)
      (setf value (list value)))
    (setf (gethash slot (frame-slots frame)) value)
    ;; +++ fairly serious change ... verify that this works
    (when old
      (dolist (removed (set-difference old value :test #'equal))
	(when (frame-p removed)
	  (deletef frame (gethash slot (frame-inverse-slots removed))))))
    (dolist (added (set-difference value old :test #'equal))
      (when (frame-p added)
	(pushnew frame (gethash slot (frame-inverse-slots added)))))))

#|
Test
(setf (slotv #$blither5 #$relatedTo) #$blather5)
(describe-frame #$blather5)


|#
	

(defmethod %slotv-inverse ((frame frame) (slot frame))
  (and (frame-inverse-slots frame)
       (gethash slot (frame-inverse-slots frame))))

(defmethod slotv-inverse ((frame frame) (slot frame) &optional (fill? *fill-by-default?*))
  (if fill? (fill-frame frame))
  (%slotv-inverse frame slot))

(defsetf slotv-inverse set-slotv-inverse)
(defsetf %slotv-inverse set-slotv-inverse)

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
(defun delistify (thing)
  (if (and (listp thing)
	   (= 1 (length thing)))
      (car thing)
      thing))

;;; +++ these should have setfs
(defmethod msv ((frame frame) slot)
  (delistify (slotv frame slot)))

(defmethod msv ((frames list) slot)
  (let ((result nil))
    (dolist (f frames (delistify result))
      ;; warning: depends on nunion only being destructive to its FIRST argument
      (setf result (nunion result (slotv f slot) :test #'equal)))))

(defmethod msv-inverse ((frame frame) slot)
  (delistify (slotv-inverse frame slot)))

(defmethod msv-inverse ((frames list) slot)
  (let ((result nil))
    (dolist (f frames (delistify result))
      ;; warning: depends on nunion only being destructive to its FIRST argument
      (setf result (nunion result (slotv-inverse f slot) :test #'equal)))))

(defun slot-has? (frame slot value)
  (member value (slotv frame slot)))

;;; This is the real underlying primitive.  Never fills
;;; Note the default test is equal.  This could be slow.
(defun add-triple (s p o &key (test #'equal) to-db)
  (if (frame-p o) (frame-fresh? o))
  (pushnew o (%slotv s p) :test test)
  (when (frame-p o)
    (pushnew s (%slotv-inverse o p) :test test))
  (when to-db
    (write-triple (frame-source s) s p o))
  nil)					;makes tracing saner

;;; see comment on delete-triple
(defun remove-triple (s p o  &key (test #'equal) to-db)
  (deletef p (%slotv s p) :test test)
  (when (frame-p o)
    (deletef s (%slotv-inverse o p) :test test))
  (when to-db
    (delete-triple (frame-source s) s p o)))

;;; query (sexp sparql syntax from lsw) 
(defun describe-frame (frame &optional (fill? t))
  (when fill? (fill-frame frame))
  (format t "~&Forward:")
  (pprint (mt:ht-contents (frame-slots frame)))
  (when (frame-inverse-slots frame)
    (format t "~&Inverse:")
    (pprint (mt:ht-contents (frame-inverse-slots frame))))
  frame )

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

;;; Test inverse
(add-triple #$a #$has #$b)
(assert (member #$b (slotv-inverse #$b #$has)))

|#

(defun name-eq (s1 s2)
  (equal (symbol-name s1) (symbol-name s2)))

;;; Slow, obviously.
(defun frames-with-value (v &optional slot)
  (collecting
    (for-all-frames (f)
      (block frame
	(if slot
	    (when (member v (slotv f slot) :test #'equal)
	      (utils::collect f)
	      (return-from frame))
	  (dolist (slot (%frame-slots f))
	    (when (member v (slotv f slot) :test #'equal)
	      (utils::collect f)
	      (return-from frame))))))))


