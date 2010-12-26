(in-package :sw)

;;; Part 2 of swframes, moved to separate file because ACL is tightassed about setf defs

(defun slot-has? (frame slot value &key (test #'eq))
  (member value (slotv frame slot) :test test))

(defun add-triple (s p o &key (test (if (frame-p o) #'eq #'equal)) to-db remove-old)
  (if (%slotv p #$sw:specialhandling)
      (add-triple-special s p o)	
      (progn
	(when remove-old
	  (remove-triple s p '?o :to-db to-db :test test))
	(if (frame-p o) (frame-fresh? o))
	(pushnew-end o (%slotv s p) :test test)
	;; PPP this can be a performance bottleneck for things like types that can have thousands of members.  
	;; Need to use hashtables or some structure with better performance 
	(when (and (frame-p o) *track-inverses*)
	  (pushnew-end s (%slotv-inverse o p) :test #'eq))
	(when to-db
	  (let ((source (if (typep to-db 'frame-source)
			    to-db
			    (frame-source s))))
	    (write-triple source s p o)))
	nil)))

;;; this should do rdfs-defmethod, but that mechanism doesn't exist yet (+++)
(defun add-triple-special (s p o)
  (pushnew-end o (%slotv s p)))

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
	(when (and *track-inverses* (frame-p o))
	  (deletef s (%slotv-inverse o p) :test test)))
      (when (and *track-inverses* (frame-p o))
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
    (setf n (or (ssv frame #$sw:slots/last_child) 0)))
  (let ((uri (string+ (frame-uri frame) "/" (fast-string n))))
    (if (uri-used? (or (frame-source frame) *default-frame-source*) uri)
	(gen-child-uri frame (+ n 1))
	(progn
	  (setf (ssv frame #$sw:slots/last_child) n)
	  (intern-uri uri)))))

(defun frame-delete-slot (frame slot)
  (let ((v (slotv frame slot)))
    (remhash slot (frame-slots frame))
    (dolist (velt v)
      (when (and *track-inverses* (frame-p velt))
	(deletef frame (%slotv-inverse velt slot))))))

(defun frame-copy (frame &key new-frame deep-slots omit-slots (uri-generator #'default-uri-generator))
  #.(doc "Make a new frame by copying the contents of FRAME."
	 "Slots listed in OMIT-SLOTS are not copied."
	 "Slots listed in DEEP-SLOTS or marked with #$sw:slots/deep-copy will have copies made of their contents, otherwise a shallow copy is performed.")
  (if (not (frame-p frame))
      frame				;nonframes remain the same (makes recursion easier)
      (let ((nframe (or new-frame (funcall uri-generator frame))))
	(setf (frame-loaded? nframe) t)	;???
	(maphash #'(lambda (slot value)
		     (cond ((member slot omit-slots))
			   ((or (member slot deep-slots)
				(ssv slot #$sw:slots/deep-copy))
			    (setf (slotv nframe slot) (mapcar #'(lambda (sf)
								  (frame-copy sf :deep-slots deep-slots :omit-slots omit-slots :uri-generator uri-generator))
							      value)))
			   ;;; Shallow copy
			   ((%slotv slot #$sw:specialhandling)
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
			       (aif (slotv f #$rdfs:label t)
				    (return-from find-label (format nil "~A of ~A" (frame-label s) it))
				    (pushnew elt fringe)))))))))
				  
 
;;; Logically belongs to code-source, but has to come after more frame machinery is defined.
;;; Write classes defined in code to a database.  This is only called by hand at the moment.
(defun write-code-source-classes (to)
  (with-write-group (to)
    (dolist (class (slotv-inverse #$rdfs:Class #$rdf:type))
      (when (eq *code-source* (frame-source class))
	(write-frame class :source to)))))

;;; Declare ourselves
(pushnew :swframes *features*)
