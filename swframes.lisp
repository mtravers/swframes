(in-package :swframes)

#|
An RDF-backed frame system
|#

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
	  for-frame-slots for-frame-inverse-slots
	  add-triple remove-triple
	  frame-copy rename-frame delete-frame write-frame destroy-frame with-write-group
	  write-slot write-triple
	  describe-frame df dft
	  register-namespace def-namespace expand-uri abbreviate-uri))

(defun frame-name (frame)
  "Returns the URI of a frame, possibly abbreviated"
  (abbreviate-uri (frame-uri frame)))

;;; Get the label, optionally filling
;;; Could logically use all subPropertys of rdfs:label, obtainable through:
;;; (do-sparql-one-var nil '(:select * nil (?p #$rdfs:subPropertyOf #$rdfs:label)))
(defun frame-label (frame &optional fill?)
  (unless (frame-p frame)
    (return-from frame-label (fast-string frame)))
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

(defun most-significant-name (string)
  (car (last (string-split string #\/))))

;;; names should be reversed
(defun %frame-slots (frame)
  "Return a list of known slots for this frame"
  (and (frame-slots frame)
       (hash-keys (frame-slots frame))))

(defun %frame-inverse-slots (frame)
  "Return a list of known inverse slots for this frame"
  (and (frame-inverse-slots frame)
       (hash-keys (frame-inverse-slots frame))))

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

;;; Just clear out special frames, since equality doesn't work on them
;;; No longer used, I was apparently brain damaged.
(defmethod reset-frame-limited ((frame frame))
  (for-frame-slots (frame slot value)
		   (when (%slotv slot #$crx:specialhandling)
		     (remhash slot (frame-slots frame)))))


(defgeneric delete-frame (frame) 
  (:documentation   #.(doc
     "Delete FRAME from memory.  Attempts to remove all references from other frames, but this is not guaranteed."
     "Does not delete from database (see DESTROY-FRAME)")))

(defmethod delete-frame ((frame frame))
  ;;; remove references (that we know about)
  (for-frame-slots (frame slot value)
		   (unless (%slotv slot #$crx:specialhandling)
		     (dolist (elt value)
		       (when (and (frame-p elt) (frame-inverse-slots elt))
			 (deletef frame (gethash slot (frame-inverse-slots elt))))))) ;NNN
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

(defgeneric fill-frame (frame &key force? source inverse?)
  (:documentation
   #.(doc
      "Ensure that the contents of FRAME (its slots and inverse-slots) are up to date as defined by SOURCE."
      "Does nothing if FRAME is already marked as loaded, unless FORCE? is true."
      "INVERSE? loads inverse-slots, default is T")))

(defvar *fill-by-default?* t "True if slot functions do a fill by default.  Initally T, can be dynamically bound")

(defparameter *dereference?* nil)

(defmethod fill-frame ((frame frame) &key force? (source (or (frame-source frame) *default-frame-source*)) (inverse? t) reset?)
  (when (or force?
	    (not (frame-loaded? frame))
	    (not (equal source (frame-source frame)))
	    )
    ;; dangerous
    (when reset?
      (clrhash (frame-slots frame))
      (when (frame-inverse-slots frame)
	(clrhash (frame-inverse-slots frame))))
    (let ((*fill-by-default?* nil))	;prevent recursion
      (if source
	  (progn (fill-frame-from frame source :inverse? inverse?)
		 ;; if nothing from db, try dereferencing
		 (unless (frame-loaded? frame)
		   (progn		;was report-and-ignore-errors
		    (setf (frame-source frame) nil)
		    (when *dereference?*
		      (dereference frame force?))))) ;+++
	  (when *dereference?* (dereference frame force?)))		;+++
      (classify-frame frame)		
      (set-frame-loaded? frame t source)))
  frame)

;;; Gets overwritten later, here to support load, probably not the right solution +++
(defmethod dereference ((frame frame) &optional force?)
  )

;;; Called by rdfs-defmethod and other things to mark that a frame is defined from code, and not
;;; expected to be read from the database.
(defun frame-from-code (f)
  (setf (frame-loaded? f)
	t
	(frame-source f)
	*code-source*))

(defun %slotv (frame slot)
  (and (frame-slots frame)
       (gethash slot (frame-slots frame))))  

(defsetf %slotv %set-slotv)

(defun make-slot-hashtable ()
  (make-hash-table :test #'eq))

(defun %set-slotv (frame slot value)
  (setf (gethash slot (frame-slots-force frame)) value))  

;;; This is slow, but makes for convenient code
;;; +++ should disambiguate
(defun coerce-slot (slot frame &key (error? t))
  (when (frame-p slot) (return-from coerce-slot slot))
  (let ((s (fast-string slot)))
    (for-frame-slots (frame aslot value)
      (declare (ignore value))
      (when (equalp (most-significant-name (frame-uri aslot)) s)
	(return-from coerce-slot aslot)))
    ;; No existing slot, try classes
    ;; +++ with current way coerce-slot-for-class works, always will use the first class
    (dolist (class (rdfs-classes frame))
      (awhen (coerce-slot-for-class slot class)
	(return-from coerce-slot it)))
    (if error?
	(error "Cannot coerce ~A to a slot of ~A" slot frame)
	nil)
    ))

(defun coerce-slot-for-class (slot class)
  (when (frame-p slot) (return-from coerce-slot-for-class slot))
  ;; +++ some check to see if it's there
  (intern-uri (string+ (frame-name class) "/s/" (string-downcase (fast-string slot)))))

;;; optional argument doesn't play well with setf.
(defun slotv (frame slot &optional (fill? *fill-by-default?*))
  "Returns the value of SLOT in FRAME (will always be a list)"
  (setf slot (coerce-slot slot frame))
  (if (eq fill? t) (fill-frame frame))
  (let ((v  (or (%slotv frame slot)
		(when (eq fill? :if)
		  (fill-frame frame)
		  (%slotv frame slot)))))
    ;; (dolist (v-elt v)			;CCC do a classify here.  Will only work if fill has happened.
;;       (when (frame-p v-elt)
;; 	(classify-frame v-elt)))
    v))

(defvar *check-slot-domains?* nil)

;;; note that this and set-slotv-inverse never do fills
;;; this can't really do inverses, can it? we'd have to a difference...
(defun set-slotv (frame slot value)
  (setf slot (coerce-slot slot frame))
  (if (%slotv slot #$crx:specialhandling)
      (%set-slotv frame slot value)
      (progn
	(when *check-slot-domains?*
	  (awhen (ssv slot #$rdfs:domain)
		 (assert (rdfs-classp frame it)))
	  (awhen (ssv slot #$rdfs:range)
		 (assert (rdfs-classp frame it))))
	(let ((old (%slotv frame slot)))
	  ;; enforce rule that slot values are lists...
	  (unless (listp value) 
	    (error "Arg to set-slotv must be list: ~A ~A ~A" frame slot value))
	  (%set-slotv frame slot value)
	  ;; (too slow for long lists) PPP
	  (when old
      (dolist (removed (set-difference old value :test #'equal))
	(when (frame-p removed)
	  (deletef frame (gethash slot (frame-inverse-slots-force removed))))))
	  (dolist (added (set-difference value old :test #'equal))
	    (when (frame-p added)
	      (pushnew frame (gethash slot (frame-inverse-slots-force added)))))
	  value))))

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
;+++ need an inverse form  (setf slot (coerce-slot slot frame))	;
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
	(error "Multiple values where one expected: ~A/~A was ~A" frame slot v))
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

(defun add-triple (s p o &key (test (if (frame-p o) #'eq #'equal)) to-db remove-old)
  (if (%slotv p #$crx:specialhandling)
      (add-triple-special s p o)	;+++ forward call
      (progn
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
	nil)))

;;; this should do rdfs-defmethod, but that mechanism doesn't exist yet
(defun add-triple-special (s p o)
  (pushnew o (%slotv s p)))

;;; see comment on delete-triple
;;; +++ should handle vars in ?s -- and needs to be nailed down in general.
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
    (pprint (ht-contents (frame-slots frame))))
  (when (frame-inverse-slots frame)
    (format t "~&Inverse:")
    (pprint (ht-contents (frame-inverse-slots frame))))
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
       (gensym-instance-frame (car (order-classes it)) :fast? t)
       (gen-child-uri frame)))

;;; Another method for generating unique URIs, if no class is found.
(defun gen-child-uri (frame &optional n)
  (unless n
    (setf n (or (ssv frame #$crx:slots/last_child) 0)))
  (let ((uri (string+ (frame-uri frame) "/" (fast-string n))))
    (if (uri-used? (or (frame-source frame) *default-frame-source*) uri)
	(gen-child-uri frame (+ n 1))
	(progn
	  (setf (ssv frame #$crx:slots/last_child) n)
	  (intern-uri uri)))))

(defun frame-copy (frame &key deep-slots omit-slots (uri-generator #'default-uri-generator))
  #.(doc "Make a new frame by copying the contents of FRAME."
	 "Slots listed in omit-slots are not copied."
	 "Slots listed in DEEP-SLOTS or marked with #$crx:slots/deep-copy will have copies made of their contents, otherwise a shallow copy is performed.")
  (if (not (frame-p frame))
      frame				;nonframes remain the same (makes recursion easier)
      (let ((nframe (funcall uri-generator frame)))
	(setf (frame-loaded? nframe) t)
	(maphash #'(lambda (slot value)
		     (cond ((member slot omit-slots))
			   ((or (member slot deep-slots)
				(ssv slot #$crx:slots/deep-copy))
			    (setf (slotv nframe slot) (mapcar #'(lambda (sf)
								  (frame-copy sf :deep-slots deep-slots :omit-slots omit-slots :uri-generator uri-generator))
							      value)))
			   ;;; Shallow copy
			   ((%slotv slot #$crx:specialhandling)
			    (setf (%slotv nframe slot) value))
			   (t
			    (setf (slotv nframe slot) (copy-list value)))
			   ))
		 (frame-slots frame))
	nframe)))

;;; find-label: do a breadth first search from a frame until we hit something with a label, and produce a string
(defun find-label (f)
  (or (slotv f #$rdfs:label t)
      (let ((fringe (list f f))
	    (done nil))
	(do ((f (pop fringe) (pop fringe)))
	    ((null fringe))
	  (push f done)
	  (for-frame-slots (f s v)
			   (dolist (elt v)
			     (when  (and (frame-p elt) (not (member elt done)))
			       (print elt)
			       (aif (slotv f #$rdfs:label t)
				    (return-from find-label (format nil "~A of ~A" (frame-label s) it))
				    (pushnew elt fringe)))))))))
				  
 
#|
;;; Debugging, search db for templates with bad template
(dolist (temp (rdfs-find :all :class #$crx:Template)) 
  (print (list temp (report-and-ignore-errors (template-string temp)))))
|#

;;; for debugging, of course
;(trace set-ssv set-slotv set-msv add-triple remove-triple)

;;; Logically belongs to code-source, but has to come after more frame machinery is defined.

;;; Write classes defined in code to a database.  This is only called by hand at the moment.
(defun write-code-source-classes (to)
  (with-write-group (to)		;+++ this needs to be moved after definition
    (dolist (class (slotv-inverse #$rdfs:Class #$rdf:type))
      (when (eq *code-source* (frame-source class))
	(write-frame class :source to)))))
