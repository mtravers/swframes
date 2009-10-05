(in-package :swframes)

#|
An RDF-backed frame system
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-frame-source* nil))

(export '(make-frame
	  *default-frame-source* *fill-by-default?*
	  frame frame-p frame-name frame-named frame-label frame-uri intern-uri
	  most-significant-name 
	  %frame-slots %frame-inverse-slots
	  reset-frames for-all-frames all-frames
	  fill-frame fill-frame-inverse frame-loaded?
	  %slotv
	  slotv slotv-inverse
	  slot-accessor inverse-slot-accessor
	  msv msv-inverse 
	  ssv ssv-inverse ssv-accessor
	  declare-special-slot
	  add-triple remove-triple
	  rename-frame delete-frame write-frame destroy-frame with-sparul-group
	  describe-frame df dft
	  register-namespace def-namespace expand-uri abbreviate-uri))

(defun frame-name (frame)
  "Returns the URI of a frame, possibly abbreviated"
  (abbreviate-uri (frame-uri frame)))  

;;; Get the label, optionally filling
;;; Could logically use all subPropertys of rdfs:label, obtainable through:
;;; (do-sparql-one-var nil '(:select * nil (?p #$rdfs:subPropertyOf #$rdfs:label)))
(defun frame-label (frame &optional fill?)
  "Returns the preferred human-readable label of a frame"
  (labels ((ssv-safe (frame slot)
	     (car (slotv frame slot fill?))))
    (or (ssv-safe frame #$rdfs:label) 
	(ssv-safe frame #$skos:prefLabel)
	(ssv-safe frame #$http://purl.org/science/owl/sciencecommons/ggp_has_primary_symbol)
	(ssv-safe frame #$bp:SHORT-NAME)
	(ssv-safe frame #$bp:NAME)
	(most-significant-name (frame-name frame))
	)))

;;; +++ sometimes you want the part following #
(defun most-significant-name (string)
  (car (last (utils:string-split string #\/))))

;;; names should be reversed
(defun %frame-slots (frame)
  "Return a list of known slots for this frame"
  (and (frame-slots frame)
       (wlisp::hash-keys (frame-slots frame))))

(defun %frame-inverse-slots (frame)
  "Return a list of known inverse slots for this frame"
  (and (frame-inverse-slots frame)
       (wlisp::hash-keys (frame-inverse-slots frame))))

(defmacro for-frame-slots ((frame slot value) &body body)
  "Iterate BODY over all slots of a frame, successively binding SLOT and VALUE vars"
  `(and (frame-slots ,frame)
	(maphash #'(lambda (,slot ,value)
		     #+CCL (declare (ccl::ignore-if-unused ,slot ,value))
		     ,@body)
		 (frame-slots ,frame))))

(defmacro for-frame-inverse-slots ((frame slot value) &body body)
  "Iterate BODY over all inverse slots of a frame, successively binding SLOT and VALUE vars"
  `(and (frame-inverse-slots ,frame)
	(maphash #'(lambda (,slot ,value)
		     ,@body)
		 (frame-inverse-slots ,frame))))

;;; Probably don't want to do this, will lose code frames
(defun reset-frames ()
  (clrhash *uri->frame-ht*))

(defmacro for-all-frames ((var) &body body)
  "Map BODY over all known frames"
  `(maphash #'(lambda (uri ,var)
		(declare (ignore uri))
		,@body)
	    *uri->frame-ht*))

(defun reset-frames ()
  (for-all-frames (f)
		  (unless (eq (frame-source f) *code-source*)
		    (unintern-frame f))))

(defun all-frames ()
  "Returns list of all known frames"
  (collecting (for-all-frames (f) (collect f))))

(defmethod reset-frame ((frame frame))
  "Clears frame slot and load information"
  (setf (frame-slots frame) nil)
  (setf (frame-loaded? frame) nil)
  (setf (frame-inverse-slots frame) nil))

(defmethod delete-frame ((frame frame))
  #.(doc
     "Delete frame from memory.  Attempts to remove all references from other frames, but this is not guaranteed."
     "Does not delete from database (see DESTROY-FRAME)")
  ;;; remove references (that we know about)
  (for-frame-slots (frame slot value)
		   (dolist (elt value)
		     (when (and (frame-p elt) (frame-inverse-slots elt))
		       (deletef frame (gethash slot (frame-inverse-slots elt)))))) ;NNN
  (for-frame-inverse-slots (frame slot value)
			   (dolist (elt value)
			     (when (frame-p elt)
			       (deletef frame (gethash slot (frame-slots elt)))))) ;NNN

  (reset-frame frame)
  (unintern-uri (frame-uri frame)))

(defun delete-frame-recursive (frame depth)
  #.(doc
     "Deletes FRAME and those frames it references, up to depth DEPTH"
     "Does not delete frames defined in code")
  (unless (zerop depth)
    (when (and (frame-fresh? frame nil)	;skip if already deleted
	       (not (frame-from-code frame))) ;don't delete classes etc.
      (for-frame-slots (frame slot value)
		       (dolist (elt value)
			 (when (frame-p elt)
			   (delete-frame-recursive elt (- depth 1)))))
      (delete-frame frame)
      ;; don't do inverses since we don't want to delete classes etc.
      )))

(defun frames-matching (uri-frag)
  (collecting 
   (for-all-frames (f)
		   (if (search uri-frag (frame-uri f))
		       (collect f)))))

(defun delete-frames-matching (uri-frag)
  (for-all-frames (f)
		  (if (search uri-frag (frame-uri f))
		      (delete-frame f))))

(defun frame-fresh? (frame &optional (error? t))
  "Tests to make sure frame is not stale (out of sync with interned version). Mostly for debugging"
  (unless (eq frame (frame-named (frame-uri frame)))
    (if error?
	(error "~A is stale" frame)
	(return-from frame-fresh? nil)))
  t)

(defmethod fill-frame-inverse ((frame frame))
  (fill-frame frame))

(defmethod fill-frame ((frame frame) &key force? (source (frame-source frame)) (inverse? t))
  #.(doc
     "Ensure that the contents of FRAME (its slots and inverse-slots) are up to date as defined by SOURCE."
     "Does nothing if FRAME is already marked as loaded, unless FORCE? is true.")
  (when (or force? (not (frame-loaded? frame)))
    (setf (frame-loaded? frame) nil)
    ;; reset-frame was here, but moved to sparql.  This all needs rethinking
    (let ((*fill-by-default?* nil)	;prevent recursion
;	  (existing-nslots (hash-table-count (frame-slots frame)))
	  )
      (if source
	  (progn (fill-frame-from frame source :inverse? inverse?)
		 ;; if nothing from db, try dereferncing
		 (unless (frame-loaded? frame)
		   (report-and-ignore-errors	;+++
		    (setf (frame-source frame) nil)
		    (dereference frame))))
	  (progn ; utils:report-and-ignore-errors	;+++
	   (dereference frame)))
      (set-frame-loaded? frame))))


;;; Called by rdfs-defmethod and other things to mark that a frame is defined from code, and not
;;; expected to be read from the database.
(defun frame-from-code (f)
  (setf (frame-loaded? f)
	t
	(frame-source f)
	*code-source*))

(defvar *fill-by-default?* t)

(defun %slotv (frame slot)
  (and (frame-slots frame)
       (gethash slot (frame-slots frame))))  

(defsetf %slotv %set-slotv)

(defun make-slot-hashtable ()
  (make-hash-table :test #'eq))

(defun %set-slotv (frame slot value)
  (setf (gethash slot (frame-slots-force frame)) value))  

;;; optional argument doesn't play well with setf.
(defun slotv (frame slot &optional (fill? *fill-by-default?*))
  "Returns the value of SLOT in FRAME (will always be a list)"
  (if (eq fill? t) (fill-frame frame))
  (or (%slotv frame slot)
      (when (eq fill? :if)
	(fill-frame frame)
	(%slotv frame slot))))

(defvar *check-slot-domains?* nil)

;;; note that this and set-slotv-inverse never do fills
;;; this can't really do inverses, can it? we'd have to a difference...
(defun set-slotv (frame slot value)
  (when *check-slot-domains?*
    (awhen (ssv slot #$rdfs:domain)
	   (check-class-membership frame it))
    (awhen (ssv slot #$rdfs:range)
	   (check-class-membership frame it)))
  (let ((old (%slotv frame slot)))
    ;; enforce rule that slot values are lists...
    (unless (listp value) 
      (error "Arg to set-slotv must be list: ~A ~A ~A" frame slot value))
    (%set-slotv frame slot value)
    ;; +++ fairly serious change ... verify that this works 
    ;; (too slow for long lists)
    (when old
      (dolist (removed (set-difference old value :test #'equal))
	(when (frame-p removed)
	  (deletef frame (gethash slot (frame-inverse-slots-force removed))))))
    (dolist (added (set-difference value old :test #'equal))
      (when (frame-p added)
	(pushnew frame (gethash slot (frame-inverse-slots-force added)))))
    value))

(defsetf slotv set-slotv)

(defun frame-slots-force (frame)
  (or (frame-slots frame)
      (setf (frame-slots frame)
	    (make-slot-hashtable))))

(defun frame-inverse-slots-force (frame)
  (or (frame-inverse-slots frame)
      (setf (frame-inverse-slots frame)
	    (make-slot-hashtable))))

(defun %slotv-inverse (frame slot)
  (and (frame-inverse-slots frame)
       (gethash slot (frame-inverse-slots frame))))

(defun slotv-inverse (frame slot &optional (fill? *fill-by-default?*))
  (if fill? (fill-frame frame))
  (%slotv-inverse frame slot))

(defsetf slotv-inverse set-slotv-inverse)
(defsetf %slotv-inverse set-slotv-inverse)

(defun set-slotv-inverse (frame slot value)
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-slot-hashtable)))
  (setf (gethash slot (frame-inverse-slots frame)) value))

(defun delistify (thing)
  (if (and (listp thing)
	   (= 1 (length thing)))
      (car thing)
      thing))

(defun listify (thing)
  (if (listp thing)
      thing
      (list thing)))

(defun slot-accessor (slot &optional fill?)
  "Returns a one-argument function that accepts a frame and returns the contents of SLOT on that frame."
  #'(lambda (f) 
      (slotv f slot fill?)))

(defun inverse-slot-accessor (slot &optional fill?)
  "Returns a one-argument function that accepts a frame and returns the value of the inverse of SLOT on that frame."
  #'(lambda (f) 
      (slotv-inverse f slot fill?)))

(defun msv (frames slot &optional (fill? *fill-by-default?*))
  #.(doc
     "Set values, possibly multiple, on SLOT of FRAMES."
     "MSV deals transparently with multiple values (that is, it returns a single element if that's all there is, otherwise a list)"
     "It also accepts lists of FRAMES, in which case the results of the individual slot values are unioned together.")
  (if (listp frames)
      (let ((result nil))
	(dolist (f frames (delistify result))
	  ;; warning: depends on nunion only being destructive to its FIRST argument
	  (setf result (nunion result (slotv f slot fill?) :test #'equal))))
      (delistify (slotv frames slot fill?))))

(defsetf msv set-msv)

(defun set-msv (frames slot value)
  (dolist (frame (listify frames))
    (setf (slotv frame slot) (listify value))))

(defun msv-inverse (frames slot)
  (if (listp frames)
      (let ((result nil))
	(dolist (f frames (delistify result))
	  ;; warning: depends on nunion only being destructive to its FIRST argument
	  (setf result (nunion result (slotv-inverse f slot) :test #'equal))))
      (delistify (slotv-inverse frames slot))))

;;; SSV functions enforce single values 

(defun ssv (frame slot &optional (fill? *fill-by-default?*))
  "Returns the value of SLOT in FRAME, which must be a single element (or missing) or an error is signalled."
  (let ((v (slotv frame slot fill?)))
    (if (> (length v) 1)
	(error "Multiple values where one expected"))
    (car v)))

(defun set-ssv (frame slot value)
  (setf (slotv frame slot) (list value))
  value)

(defsetf ssv set-ssv)

(defun ssv-inverse (frame slot)
  (let ((v (slotv-inverse frame slot)))
    (if (> (length v) 1)
	(error "Multiple values where one expected"))
    (car v)))

(defun ssv-accessor (slot)
  #'(lambda (f) 
      (ssv f slot)))

(defun slot-has? (frame slot value)
  (member value (slotv frame slot)))

;;; Note the default test is equal.  This could be slow.
;;; +++ setf %slotv was not primitive, now fixed, but who knows if this will work now.
(defun add-triple (s p o &key (test (if (frame-p o) #'eq #'equal)) to-db remove-old)
  (when remove-old
    (remove-triple s p '?o :to-db to-db :test test))
  (if (frame-p o) (frame-fresh? o))
  (pushnew o (%slotv s p) :test test)
  ;; PPP this can be a performance bottleneck for things like types that can have thousands of members.  
  ;; Need to use hashtables or some structure with better performance 
  (when (frame-p o)
    (pushnew s (%slotv-inverse o p) :test #'eq))
  (when to-db
    (let ((source (if (typep to-db 'frame-source)
		      to-db
		      (frame-source s))))
      (write-triple source s p o)))
  nil)

;;; see comment on delete-triple
(defun remove-triple (s p o &key (test #'equal) to-db &aux savedo)
  (if (var-p o)
      (progn 
	(setf savedo (%slotv s p) (%slotv s p) nil)
	(setf (%slotv s p) nil))
      (deletef o (%slotv s p) :test test))
  (if savedo
      (dolist (o savedo)
	(when (frame-p o)
	  (deletef s (%slotv-inverse o p) :test test)))
      (when (frame-p o)
	(deletef s (%slotv-inverse o p) :test test)))
  (when to-db
    (let ((source (if (typep to-db 'frame-source)
		      to-db
		      (frame-source s))))
      (delete-triple source s p o))))

;;; query (sexp sparql syntax from lsw) 
(defun describe-frame (frame &optional (fill? nil))
  "Describe the contents of FRAME, optionally filling"
  (when fill? (fill-frame frame :force? t))
  (format t "~&Forward:")
  (when (frame-slots frame)
    (pprint (utils:ht-contents (frame-slots frame))))
  (when (frame-inverse-slots frame)
    (format t "~&Inverse:")
    (pprint (utils:ht-contents (frame-inverse-slots frame))))
  frame )

(defun df (frame &optional (fill? nil))
  "Describe the contents of FRAME"
  (describe-frame frame fill?))
(defun dft (frame)
  "Describe the contents of FRAME, filling it first."
  (df frame t))

(defun name-eq (s1 s2)
  (equal (symbol-name s1) (symbol-name s2)))

;;; Slow, obviously.
(defun frames-with-value (v &optional slot)
  (collecting
    (for-all-frames (f)
      (block frame
	(if slot
	    (when (member v (slotv f slot) :test #'equal)
	      (collect f)
	      (return-from frame))
	  (dolist (slot (%frame-slots f))
	    (when (member v (slotv f slot) :test #'equal)
	      (collect f)
	      (return-from frame))))))))

(defun default-uri-generator (frame)
  (aif (rdfs-classes frame)
       (gensym-instance-frame (car it) :fast? t)
       (gen-child-uri frame)))

;;; Another method for generating unique URIs, if no class is found.
(defun gen-child-uri (frame &optional n)
  (unless n
    (setf n (or (ssv frame #$slots/last_child) 0)))
  (let ((uri (string+ (frame-uri frame) "/" (fast-string n))))
    (if (uri-used? (or (frame-source frame) *default-frame-source*) uri)
	(gen-child-uri frame (+ n 1))
	(progn
	  (setf (ssv frame #$slots/last_child) n)
	  (intern-uri uri)))))

;;; +++ it would be better to have info on how to treat slots on the slots themselves, or in classes.
(defun frame-copy (frame &key deep-slots omit-slots (uri-generator #'default-uri-generator))
  (if (not (frame-p frame))
      frame				;nonframes remain the same (makes recursion easier)
      (let ((nframe (funcall uri-generator frame)))
	(setf (frame-loaded? nframe) t)
	(maphash #'(lambda (slot value)
		     (cond ((member slot omit-slots))
;			   ((member slot shallow-slots)
;			    (setf (slotv nframe slot) (copy-list value)))
			   ((or (member slot deep-slots)
				(ssv slot (setq xxx #$crx:slots/deep-copy)))
			    (setf (slotv nframe slot) (mapcar #'(lambda (sf)
								  (frame-copy sf :deep-slots deep-slots :omit-slots omit-slots :uri-generator uri-generator))
							      value)))
			   ;;; Shallow copy
			   (t
			    (setf (slotv nframe slot) (copy-list value)))
			   ))
		 (frame-slots frame))
	nframe)))

;;; find-label: do a breadth first search from a frame until we hit something with a label, and produce a string
(defun find-label (f)
  (or (slotv f (intern-uri "http://www.w3.org/2000/01/rdf-schema#label") t)
      (let ((fringe (list f f))
	    (done nil))
	(do ((f (pop fringe) (pop fringe)))
	    ((null fringe))
	  (push f done)
	  (for-frame-slots (f s v)
			   (dolist (elt v)
			     (when  (and (frame-p elt) (not (member elt done)))
			       (print elt)
			       (aif (slotv f (intern-uri "http://www.w3.org/2000/01/rdf-schema#label") t)
				    (return-from find-label (format nil "~A of ~A" (frame-label s) it))
				    (pushnew elt fringe)))))))))
				  
 
