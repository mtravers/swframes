(in-package :swframes)

(export '(rdfs-def-class rdfs-make-instance 
	  rdfs-classes rdfs-classp rdfs-subclasses
	  rdfs-defmethod rdfs-call rdfs-find

	  defclass$ defmethod$ make-instance$
	  ))

#|
Notes:

Defclass
- Might want to use a short name for slots and have uri generated with a template.
- yes, have a standard abbrev so make-instance can use keywords

Make-instance
-  has to make a unique name, which requires a sparql source? Not sure what to do about that
   - bad thought -- names could be uniquified on writing (ie, a frame keeps identity but changes its name) 
     - would screw interactive use

Todo (+++):

rdfs-lists (important...to translate from/to frame rep, slots need to have a property that says if the value is a list (as opposed to just a collection of elements))

|#



(defun all-superclasses (c1)
  (transitive-closure c1 (slot-accessor #$rdfs:subClassOf nil)))

(defun all-subclasses (c1)
  (transitive-closure c1 (inverse-slot-accessor #$rdfs:subClassOf nil)))
  
(defun is-subclass? (c1 c2)
  (member c2 (all-superclasses c1)))

(defun is-superclass? (c1 c2)
  (member c2 (all-subclasses c1)))

;;; Way inefficient, but 
(defun collapse-class-list (cl)
  (filter-out #'(lambda (c1)
		  (some #'(lambda (c2)
			    (and (not (eq c1 c2))
				 (is-superclass? c1 c2)))
			cl))
	      cl))

(defun frame-supertypes (frame)
  (slotv frame #$rdfs:subClassOf))

(defmacro defclass$ (class superclasses &body slots)
  #.(doc
     "Define an RDFS class.  CLASS is a frame SUPERCLASSES is a list of frames."
     "SLOTS is a list of slot defining forms, which are of the form:"
     "  (SLOT :option value ...)"
     "where slot is a frame, options are:"
     "  :class: indicates a class for the slot (see declare-special-slot) "
     "  :range: the range of the slot (domain is CLASS) "
     "  :dependent: indicates that values in the slot are dependent objects (ie, they are deleted if the main instance is) "
     "  :deep-copy ")
  (let ((clauses nil))
    (labels ((a! (s p o)
	       (push `(frame-from-code ,s) clauses)
	       (push `(frame-from-code ,p) clauses)
	       (when (frame-p o)
		 (push `(sw::frame-from-code ,o) clauses))
	       (push `(add-triple ,s ,p ,o) clauses)
	       )
	     (a!x (s p o)
	       (let ((old (ssv s p nil)))
		 (cond ((eq old o))
		       ((null old)
			(a! s p o))
		       (t (error "~A already has ~A ~A, can't set to ~A" s p old o)))))
	     )
      (a! class #$rdf:type #$rdfs:Class)
      (a! class #$rdfs:label (most-significant-name (frame-uri class)))
      (mapc #'(lambda (superclass)
		(a! class #$rdfs:subClassOf superclass))
	    superclasses)
      (mapc #'(lambda (slotdef)
		(unless (listp slotdef) (setf slotdef (list slotdef)))
		(setf slotdef
		      (cons (coerce-slot-for-class (car slotdef) class)
			    (cdr slotdef)))
		(macrolet ((handle-slot-property (sprop &body body)
			     `(awhen (member ,sprop (cdr slotdef))
				     ,@body
				     (setf slotdef (remove (cadr it) (remove (car it) slotdef))))))
		  (let ((slot (car slotdef)))
		    (a! slot #$rdf:type #$rdf:Property)
		    (a!x slot #$rdfs:domain class)
		    (handle-slot-property :range (a!x slot #$rdfs:range (cadr it)))
		    (handle-slot-property :class (push `(declare-special-slot ,slot ,(cadr it)) clauses))
		    (handle-slot-property :uitype (push `(setf (ssv ,slot #$crx:slots/uitype) ,(cadr it)) clauses))
		    (handle-slot-property :dependent (push `(setf (ssv ,slot #$crx:slots/dependent) t) clauses))
		    (handle-slot-property :deep-copy (push `(setf (ssv ,slot #$crx:slots/deep-copy) t) clauses))
		    (when (cdr slotdef)
		      (error "Unknown slot properties in ~A" slotdef))
		    )))
	    slots)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 ,@clauses
	 ,(defclass-form class superclasses)
	      ,class))))

;;; Support older name (deprecated, I guess)
(defmacro rdfs-def-class (&rest rest)
  `(defclass$ ,@rest))

;;; Put in some checking;  should be under a flag. 
;;; Also option for specifying a name or partial name.
(defvar *fast-instances?* t)

;;; +++ should have an optional frame arg, when we know the name
(defun make-instance$ (class &rest slots)
  "Make an instance of CLASS.  Slots are alternating frame/values.  The URI is generated automatically."
  (flet ((check-class (thing class)
	   (when class
	     (assert (rdfs-classp thing class) nil "~A is not of rdfs-class ~A" thing class))))
    (check-class class #$rdfs:Class)
    (let ((frame (gensym-instance-frame class :fast? *fast-instances?*)))
      (setf (ssv frame #$rdf:type) class)
      (setf (frame-loaded? frame) t)	;if we are consing this from scratch in memory, it is considered loaded
      (set-frame-class frame class t)
      (do ((rest slots (cddr rest)))
	  ((null rest) frame)
	(let ((slot (or (coerce-slot (car rest) frame :error? nil)
			(coerce-slot-for-class (car rest) class)
			)))
	  (check-class frame (#^rdfs:domain slot)) 
	  (check-class (cadr rest) (#^rdfs:range slot)) 
	  (if (rdfs-classp slot #$crx:slots/LispValueSlot)
	      (setf (ssv frame slot) (cadr rest))
	      (setf (msv frame slot) (cadr rest))))))))

;;; Older form (deprecated)
(defun rdfs-make-instance (class &rest slots)
  (apply #'make-instance$ class slots))

(defun rdfs-find (value &key slot class (source *default-sparql-endpoint*) word? fill? case-insensitize? limit from)
  #.(doc
     "Find instances of CLASS that have VALUE on SLOT."
     "VALUE can be :all, in which case all instances of CLASS are returned"
     "If SLOT is nil, VALUE can be on any slot of instance. "
     "If WORD? is true, does a text search of VALUE as a word contained in the actual slot value")
  (let ((sparql (rdfs-find-sparql value :slot slot :class class :word? word? :limit limit :from from))
	result)
    (when case-insensitize?
      (setf sparql (case-insensitize-2 sparql)))
    (setf result
	  (if fill?
	      (bulk-load-query source sparql)
	      (do-sparql-one-var source sparql)))
    ;; Set the class if we know it.  Seems like this should be done more places.
    (when class
      (mapc #'(lambda (r) (set-frame-class r class)) result))
    result))

;;; generalize to multiple slot/values.  +++
(defun rdfs-find-sparql (value &key slot class word? limit from)
  (let ((vvar (if word? (gensym "?V"))))
    `(:select (?s) (:distinct t :limit ,limit :from ,from)
	      ,@(unless (eq value :all)
			`((?s ,(if slot slot '?p) ,(if word? vvar value))))
	      ,@(if class `((?s #$rdf:type ,class)))
	      ;; UGH quoting, but I think this is right...
	      ,@(if word? `((:filter (:regex ,vvar ,(format nil "\\\\W~A\\\\W" value) "i"))))
	      )))

(defun rdfs-classp (frame class)
  (if (frame-p frame)
      (or (eq class #$rdfs:Resource)	;special handling, everything is a Resource (???)
	  (slot-has? frame #$rdf:type class)
	  (some #'(lambda (subclass)
		    (rdfs-classp frame subclass))
		(rdfs-subclasses class)))
      ;;non-frame
      (eq class #$rdfs:Literal)		;+++ are their subtypes of this?
      ))

#|  alternate
(defun rdfs-class-p (thing class)
  (and (frame-p thing)
       (member class (rdfs-classes thing))))
|#

(defun rdfs-subclasses (class)
  (slotv-inverse class #$rdfs:subClassOf))

;;; Method system; now based on CLOS

;;; This now can just be an ordinary defmethod.  
;;; +++ support method types.
;;; +++ other args can be frame classes as well.
(defmacro rdfs-defmethod (name args &body body)
  "Define a method that dispatches on the RDFS class of the first element of ARGS. Synatx is similar to CLOS defmethod."
  (let ((arg1 (if (listp (car args)) (car (car args)) (car args)))
	(class (if (listp (car args))
		   (rdfs-clos-class (cadr (car args)))
		   'frame)))
    `(defmethod ,name ((,arg1 ,class) ,@(cdr args))
       ,@body)))

(defun classify-arg (arg)
  (when (frame-p arg)
    (classify-frame arg))
  arg)

;;; +++ not sure I like this, it means that ordinary calls won't necessarily get the proper classes.
;;; +++ an :around method would be better but I assume it doesn't work to change classes mid-method!
(defmacro rdfs-call (name &rest args)
  `(,name ,@(mapcar #'(lambda (arg) `(classify-arg ,arg)) args)))

(defun rdfs-classes (thing &optional order?)
  "Returns all classes of THING."
  ;; try to avoid filling
  (let ((classes (or (transitive-closure (slotv thing #$rdf:type nil) (slot-accessor  #$rdfs:subClassOf nil))
		     (transitive-closure (#^rdf:type thing) #'(lambda (x) (slotv x #$rdfs:subClassOf))))))
    (if order?
	(order-classes classes)
	classes)))

;;; This is bogus -- needs to use the transitive closure of #$rdfs:subClassOf to be accurate.
(defun order-classes (classes)
  (sort classes #'(lambda (c1 c2) (member c2 (slotv c1 #$rdfs:subClassOf)))))



