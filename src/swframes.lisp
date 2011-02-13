(in-package :swframes)

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

(export '(make-frame
	  *default-frame-source* *fill-by-default?*
	  frame frame-p frame-name frame-named frame-label frame-uri intern-uri
	  clean-string most-significant-name frame-id-suffix ;create-valid-frame-name conflicts with biolisp
	  %frame-slots %frame-inverse-slots
	  reset-frames for-all-frames all-frames
	  fill-frame fill-frame-inverse frame-loaded? post-fill
	  %slotv
	  slotv slotv-inverse
	  slot-accessor inverse-slot-accessor
	  msv msv-inverse 
	  ssv ssv-inverse ssv-accessor ssv-inverse-accessor
	  declare-special-slot
	  for-frame-slots for-frame-inverse-slots
	  add-triple remove-triple
	  frame-copy rename-frame delete-frame write-frame destroy-frame with-write-group
	  write-slot write-triple
	  describe-frame df dft
	  register-namespace def-namespace expand-uri abbreviate-uri frame-namespace))

;;; A flag to turn off tracking of inverse values.  Not the right thing, but can help with some performance issues.
(defvar *track-inverses* t)

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
	(most-significant-name (frame-name frame))
	)))

(defun most-significant-name (string)
  (car (last (string-split string #\/))))

(defun frame-id-suffix (frame)
  (format nil "~{_~a~}" (last (puri:uri-parsed-path (puri:parse-uri (frame-uri frame)))
			      2)))

;;; names should be reversed
(defun %frame-slots (frame)
  "Return a list of known slots for this frame"
  (and (frame-slots frame)
       (hash-keys (frame-slots frame))))

(defun %frame-inverse-slots (frame)
  "Return a list of known inverse slots for this frame"
  (and (frame-inverse-slots frame)
       (hash-keys (frame-inverse-slots frame))))

;;; for-frame-slots and for-frame-inverse-slots moved to swframes-0

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

(defmethod reset-frame ((frame frame) &optional (inverse? t))
  "Clears frame slot and load information"
  (setf (frame-slots frame) nil)
  (setf (frame-loaded? frame) nil)
  (setf (frame-inverse-slots frame) nil)
  (setf (frame-inverse-loaded? frame) nil))

;;; Just clear out special frames, since equality doesn't work on them
;;; No longer used, I was apparently brain damaged.
(defmethod reset-frame-limited ((frame frame))
  (for-frame-slots (frame slot value)
		   (when (%slotv slot #$sw:specialhandling)
		     (remhash slot (frame-slots frame)))))


(defgeneric delete-frame (frame) 
  (:documentation   #.(doc
     "Delete FRAME from memory.  Attempts to remove all references from other frames, but this is not guaranteed."
     "Does not delete from database (see DESTROY-FRAME)")))

(defmethod delete-frame ((frame frame))
  ;;; remove references (that we know about)
  (for-frame-slots (frame slot value)
		   (unless (%slotv slot #$sw:specialhandling)
		     (dolist (elt value)
		       (when (and *track-inverses*
				  (frame-p elt)
				  (frame-inverse-slots elt))
			 (deletef frame (gethash slot (frame-inverse-slots elt))))))) 
  (for-frame-inverse-slots (frame slot value)
			   (dolist (elt value)
			     (when (frame-p elt)
			       (deletef frame (gethash slot (frame-slots elt))))))

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

(defmethod fill-frame-inverse ((frame frame) &key force? (source (or (frame-source frame) *default-frame-source*)))
  (when (or force?
	    (not (frame-inverse-loaded? frame)))
    (fill-frame-inverse-from frame source)
    (setf (frame-inverse-loaded? frame) t)))

(defgeneric fill-frame (frame &key force? source inverse?)
  (:documentation
   #.(doc
      "Ensure that the contents of FRAME (its slots and inverse-slots) are up to date as defined by SOURCE."
      "Does nothing if FRAME is already marked as loaded, unless FORCE? is true."
      "INVERSE? loads inverse-slots, default is *track-inverses*")))

(defvar *fill-by-default?* nil
  "True if slot functions do a fill by default. Can be dynamically bound. ")

(defparameter *dereference?* nil)

(defmethod fill-frame ((frame frame) &key force? (source (or (frame-source frame) *default-frame-source*)) (inverse? *track-inverses*) reset?)
  (when (or force?
	    (not (frame-loaded? frame))
	    (and (frame-source frame) 
		 (not (equal source (frame-source frame)))))
    ;; dangerous
    (when reset?
      (clrhash (frame-slots frame))
      (when (frame-inverse-slots frame)
	(clrhash (frame-inverse-slots frame))))
    (let ((*fill-by-default?* nil))	;prevent recursion
      (if source
	  (progn
	    (fill-frame-from frame source)
	    (when inverse?
	      (fill-frame-inverse frame :source source :force? force?))
	    ;; if nothing from db, try dereferencing
	    (unless (frame-loaded? frame)
	      (progn		;was report-and-ignore-errors
		(setf (frame-source frame) nil)
		(when *dereference?*
		  (dereference frame force?))))) ;+++
	  (when *dereference?* (dereference frame force?)))		;+++
      (classify-frame frame)		
      (set-frame-loaded? frame t source)
      (let ((*fill-by-default?* nil))
	(post-fill frame))
      ))
  frame)

;;; Classes can add methods to this for special actions
(defmethod post-fill ((frame frame)) )

;;; Gets overwritten later, here to support load, probably not the right solution +++
(defmethod dereference ((frame frame) &optional force?)
  )

;;; Called by defmethod$ and other things to mark that a frame is defined from code, and not
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
(defun set-slotv (frame slot value)
  (setf slot (coerce-slot slot frame))
  (if (%slotv slot #$sw:specialhandling)
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
	  (when *track-inverses*
	    (when old
	      (dolist (removed (set-difference old value :test #'equal))
		(when (frame-p removed)
		  (deletef frame (gethash slot (frame-inverse-slots-force removed))))))
	    (dolist (added (set-difference value old :test #'equal))
	      (when (frame-p added)
		(pushnew-end frame (gethash slot (frame-inverse-slots-force added)))))
	    value)))))

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
  (if fill? (fill-frame-inverse frame))
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

;;; Called by make-instance$, saves some time and storage
(defun set-msv-if (frames slot value)
  (when value
    (set-msv frames slot value)))

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
	(error "Multiple values where one expected: ~A.~A was ~A" frame slot v))
    (car v)))

(defun set-ssv (frame slot value)
  (setf (slotv frame slot) (when value (list value)))
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

(defun ssv-inverse-accessor (slot)
  #'(lambda (f) 
      (ssv-inverse f slot)))
