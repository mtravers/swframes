(in-package :swframes)

#|
An RDF-backed frame system

Ideas/todos
- could wire this into CLOS in the manner of ActiveRDF.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-frame-source* nil))

;;; Growing an API here...should clean this up, consolidate some stuff
(export '(;uri obso
	  make-frame
	  *default-frame-source* *mark-new-frames-loaded?* *fill-by-default?*
	  frame frame-p frame-name frame-named frame-label frame-uri intern-uri
	  most-significant-name 
	  %frame-slots %frame-inverse-slots frame-empty?
	  reset-frames for-all-frames all-frames
	  fill-frame fill-frame-inverse frame-loaded?
	  %slotv
	  slotv slotv-inverse
	  slot-accessor inverse-slot-accessor
	  svf svif
	  msv msv-inverse 
;SSS	  msv-hack
	  ssv ssv-inverse ssv-accessor
	  declare-special-slot
	  add-triple
	  rename-frame delete-frame write-frame destroy-frame
	  describe-frame df dft
	  sw-register-namespace def-namespace))

(defun frame-name (frame)
  (abbreviate-uri (frame-uri frame)))  

;;; Get the label, optionally filling
;;; Could logically use all subPropertys of rdfs:label, obtainable through:
;;; (do-sparql-one-var nil '(:select * nil (?p #$rdfs:subPropertyOf #$rdfs:label)))
(defun frame-label (frame &optional fill?)
  (or (ssv frame #$rdfs:label fill?) 
      (ssv frame #$skos:prefLabel fill?)
      (ssv frame #$http://purl.org/science/owl/sciencecommons/ggp_has_primary_symbol fill?)
      (ssv frame #$bp:SHORT-NAME fill?)
      (ssv frame #$bp:NAME fill?)
      (most-significant-name (frame-name frame))
      ))

;;; +++ sometimes you want the part following #
(defun most-significant-name (string)
  (car (last (utils:string-split string #\/))))

;;; names should be reversed
(defun %frame-slots (frame)
  (and (frame-slots frame)
       (wlisp::hash-keys (frame-slots frame))))

(defun %frame-inverse-slots (frame)
  (and (frame-inverse-slots frame)
       (wlisp::hash-keys (frame-inverse-slots frame))))

(defmacro for-frame-slots ((frame slot value) &body body)
  `(and (frame-slots ,frame)
	(maphash #'(lambda (,slot ,value)
		     ,@body)
		 (frame-slots ,frame))))

(defmacro for-frame-inverse-slots ((frame slot value) &body body)
  `(and (frame-inverse-slots ,frame)
	(maphash #'(lambda (,slot ,value)
		     ,@body)
		 (frame-inverse-slots ,frame))))

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
  (setf (frame-slots frame) nil)
  (setf (frame-loaded? frame) nil)
  (setf (frame-inverse-slots frame) nil))

;;; Does not remove all references to frame (it could, I suppose, if we were rigorous about inverses III)
;;; Does not delete from db
(defmethod delete-frame ((frame frame))
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
  (utils:collecting 
   (for-all-frames (f)
		   (if (search uri-frag (frame-uri f))
		       (utils::collect f)))))


(defun delete-frames-matching (uri-frag)
  (for-all-frames (f)
		  (if (search uri-frag (frame-uri f))
		      (delete-frame f))))

;;; debugging
(defun frame-fresh? (frame &optional (error? t))
  (unless (eq frame (frame-named (frame-uri frame)))
    (if error?
	(error "~A is stale" frame)
	(return-from frame-fresh? nil)))
  t)

(defmethod fill-frame-inverse ((frame frame))
  (fill-frame frame))

(defmethod fill-frame ((frame frame) &key force? (source (frame-source frame)) (inverse? t))
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
		   (utils:report-and-ignore-errors	;+++
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

(defun frame-empty? (frame)
  (and (null (%frame-slots frame))
       (null (%frame-inverse-slots frame))))
  
(defvar *fill-by-default?* t)

(defmethod %slotv ((frame frame) (slot frame))
  (and (frame-slots frame)
       (gethash slot (frame-slots frame))))  

(defsetf %slotv %set-slotv)

(defun make-slot-hashtable ()
  (make-hash-table :test #'eq))

(defmethod %set-slotv ((frame frame) (slot frame) value)
  (setf (gethash slot (frame-slots-force frame)) value))  

;;; optional argument doesn't play well with setf.
(defmethod slotv ((frame frame) (slot frame) &optional (fill? *fill-by-default?*))
  (if (eq fill? t) (fill-frame frame))
  (or (%slotv frame slot)
      (when (eq fill? :if)
	(fill-frame frame)
	(%slotv frame slot))))

;;; note that this and set-slotv-inverse never do fills
;;; this can't really do inverses, can it? we'd have to a difference...
(defmethod set-slotv ((frame frame) (slot frame) value)
  (let ((old (%slotv frame slot)))
    ;; enforce rule that slot values are lists...
    (unless (listp value) 
;      (setf value (list value)))
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
    (setf (frame-inverse-slots frame) (make-slot-hashtable)))
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

(defun listify (thing)
  (if (listp thing)
      thing
      (list thing)))

(defun slot-accessor (slot &optional fill?)
  #'(lambda (f) 
      (slotv f slot fill?)))

(defun inverse-slot-accessor (slot &optional fill?)
  #'(lambda (f) 
      (slotv-inverse f slot fill?)))





;;; MSV functions deal transparently with multiple values (return a single elt if that's all there is, otherwise a list)

;;; +++ these should have setfs
(defmethod msv ((frame frame) slot &optional (fill? *fill-by-default?*))
  (delistify (slotv frame slot fill?)))

(defmethod msv ((frames list) slot &optional (fill? *fill-by-default?*))
  (let ((result nil))
    (dolist (f frames (delistify result))
      ;; warning: depends on nunion only being destructive to its FIRST argument
      (setf result (nunion result (slotv f slot fill?) :test #'equal)))))

(defsetf msv set-msv)

(defmethod set-msv ((frame frame) (slot frame) value)
  (setf (slotv frame slot) (listify value)))

(defmethod msv-inverse ((frame frame) slot)
  (delistify (slotv-inverse frame slot)))

(defmethod msv-inverse ((frames list) slot)
  (let ((result nil))
    (dolist (f frames (delistify result))
      ;; warning: depends on nunion only being destructive to its FIRST argument
      (setf result (nunion result (slotv-inverse f slot) :test #'equal)))))

;;; SSV functions enforce single values (useful for debugging).

(defmethod ssv ((frame frame) slot &optional (fill? *fill-by-default?*))
  (let ((v (slotv frame slot fill?)))
    (if (> (length v) 1)
	(error "Multiple values where one expected"))
    (car v)))

(defmethod set-ssv (frame slot value)
  (setf (slotv frame slot) (list value))
  value)

(defsetf ssv set-ssv)

(defmethod ssv-inverse ((frame frame) slot)
  (let ((v (slotv-inverse frame slot)))
    (if (> (length v) 1)
	(error "Multiple values where one expected"))
    (car v)))

(defun ssv-accessor (slot)
  #'(lambda (f) 
      (ssv f slot)))

(defun slot-has? (frame slot value)
  (member value (slotv frame slot)))

;;; This is the real underlying primitive.  Never fills.
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
  (when fill? (fill-frame frame :force? t))
  (format t "~&Forward:")
  (when (frame-slots frame)
    (pprint (utils:ht-contents (frame-slots frame))))
  (when (frame-inverse-slots frame)
    (format t "~&Inverse:")
    (pprint (utils:ht-contents (frame-inverse-slots frame))))
  frame )

(defun df (frame &optional (fill? nil)) (describe-frame frame fill?))
(defun dft (frame) (df frame t))

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
				  


 
