(in-package :swframes)

(export '(rdfs-def-class rdfs-make-instance 
	  rdfs-classes rdfs-classp rdfs-subclasses
	  rdfs-defmethod rdfs-call rdfs-call-if rdfs-find))

#|
Notes:
- range
  - need to avoid property name collisions 
- what to do with these triples?  Write them out?
  - current solution: mark the URIs using frame-from-code

Defclass
- Might want to use a short name for slots and have uri generated with a template.
- yes, have a standard abbrev so make-instance can use keywords

Make-instance
-  has to make a unique name, which requires a sparql source? Not sure what to do about that
   - bad thought -- names could be uniquified on writing (ie, a frame keeps identity but changes its name) 
     - would screw interactive use

Todo:

rdfs-lists (important...to translate from/to frame rep, slots need to have a property that says if the value is a list (as opposed to just a collection of elements))

|#

(defmacro rdfs-def-class (name superclasses &body slots)
  (let ((clauses nil))
    (flet ((a! (s p o)
	     (push `(add-triple ,s ,p ,o) clauses)
	     (push `(frame-from-code ,s) clauses)
	     (push `(frame-from-code ,p) clauses)
	     (when (frame-p o)
	       (push `(sw::frame-from-code ,o) clauses))
	     ))
      (a! name  #$rdf:type #$rdfs:Class)
      (mapc #'(lambda (superclass)
		(a! name #$rdfs:subClassOf superclass))
	    superclasses)
      (mapc #'(lambda (slotdef)
		(let ((slot (car slotdef)))
		  (a! slot #$rdf:type #$rdfs:Property)
		  (a! slot #$rdf:domain name)))
	    slots)
      `(progn ,@clauses
	      ,name))))

;;; Put in some checking;  should be under a flag. 
;;; Also option for specifying a name or partial name.
(defun rdfs-make-instance (class &rest slots)
  (flet ((check-class (thing class)
	   (when class
	     (assert (rdfs-classp thing class)))))
    (check-class class #$rdfs:Class)
    (let ((frame (gensym-instance-frame class)))
      (setf (slotv frame #$rdf:type) class)
      (setf (frame-loaded? frame) t)	;+++ new, not sure of this
      (do ((rest slots (cddr rest)))
	  ((null rest) frame)
	(check-class frame (#^rdfs:domain (car rest))) 
	(check-class (cadr rest) (#^rdfs:range (car rest))) 
	(setf (slotv frame (car rest)) 
	      (cadr rest))))))

(defun rdfs-find (value &key slot class source word?)
  (do-sparql-one-var source
    (rdfs-find-sparql value :slot slot :class class :word? word?)))

(defun rdfs-find-sparql (value &key slot class word?)
  (let ((vvar (if word? (gensym "?V"))))
    `(:select (?s) ()
	      ,@(unless (eq value :all)
			`((?s ,(if slot slot '?p) ,(if word? vvar value))))
	      ,@(if class `((?s #$rdf:type ,class)))
	      ;; UGH quoting, but I think this is right...
	      ,@(if word? `((:filter (:regex ,vvar ,(formatn "\\\\W~A\\\\W" value) "i"))))
	      )))

;;; This has to be relative to a frame source so you can check for taken ids. Or something.
(defun gensym-instance-frame (class &optional start)
  (let* ((last (or start (msv class #$crx:last_used_id)))
	 (next (if last
		   (1+ (coerce-number last))
		   0))
	 (uri (string+ (frame-uri class) "/" (fast-string next))))
    (if (uri-used? *default-frame-source* uri)
	(gensym-instance-frame class next)
	(progn
	  (setf (msv-hack #$crx:last_used_id class) next)
	  (write-slot class #$crx:last_used_id *default-frame-source*)
	  (intern-uri uri)))))

(defgeneric uri-used? (source uri))

(defun rdfs-classp (frame class)
  (and (frame-p frame)
       (or (slot-has? frame #$rdf:type class)
	   (some #'(lambda (subclass)
		     (rdfs-classp frame subclass))
		 (rdfs-subclasses class)))))

(defun rdfs-subclasses (class)
  (slotv-inverse class #$rdfs:subClassOf))


;;; Method system.  Need to think about this, RDF things can be multiply typed and there is no ordering.  
;;; Alternative is to map rdf to CLOS classes which have a more well-behaved orderng.
;;; Another alternative: define an ordinary function which does dispatching.  Eliminates need for rdfs-call, allows tracing

(defun rdfs-methodtable (symbol)
  (or (get :rdfs-methods symbol)
      (setf (get :rdfs-methods symbol)
	    (make-hash-table :test #'eq))))

(defmacro rdfs-defmethod (name args &body body)
  (let ((realargs (cons (car (car args)) (cdr args)))
	(class (cadr (car args))))
    `(setf (gethash ,class (rdfs-methodtable ',name))
	   #'(lambda ,realargs ,@body))))


;;; temp broken by #^ stuff
;;; no ordering, blah
(defun rdfs-classes (thing)
  (utils::transitive-closure (#^rdf:type thing) #'(lambda (x) (slotv x #$rdfs:subClassOf))))

(defun order-classes (classes)
  (sort classes #'(lambda (c1 c2) (member c2 (slotv c1 #$rdfs:subClassOf)))))

;;; this is way wrong, but will do for now
(defun rdfs-method (name thing &optional (errorp t))
  (fill-frame thing)
  (let ((classes (order-classes (rdfs-classes thing)))
	(methodtable (rdfs-methodtable name)))
    (or (some #'(lambda (class) (gethash class methodtable )) classes)
	(if errorp
	    (error "no method found for ~a on ~a" name thing)
	    nil))))

(defmacro rdfs-call (name firstarg &rest restargs)
  `(funcall (rdfs-method ',name ,firstarg)
	  ,firstarg
	  ,@restargs))

(defmacro rdfs-call-if (name firstarg &rest restargs)
  `(let ((method (rdfs-method ',name ,firstarg nil)))
     (when method
       (funcall method
		,firstarg
		,@restargs))))
