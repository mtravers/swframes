(in-package :swframes)

#|
Notes:
- range
- need to avoid property name collisions 
- what to do with these triples?  Write them out?

Defclass
- Might want to use a short name for slots and have uri generated with a template.
- yes, have a standard abbrev so make-instance can use keywords


Make-instance
-  has to make a unique name, which requires a sparql source? Not sure what to do about that
   - bad thought -- names could be uniquified on writing (ie, a frame keeps identity but changes its name) 
     - would screw interactive use


Todo:
rdfs-typep
rdfs-types

rdfs-lists (important...to translate from/to frame rep, I'm guessing slots need to have a property that says if the value is a list (as opposed to just a collection of elements))


|#


(defmacro rdfs-def-class (name superclasses &body slots)
  (let ((clauses nil))
    (flet ((a! (s p o)
	     (push `(add-triple ,s ,p ,o) clauses)))
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
	     (assert (rdfs-typep thing class)))))
    (check-class class #$rdfs:Class)
    (let ((frame (gensym-instance-frame class)))
      (setf (slotv frame #$rdf:type) class)
      (do ((rest slots (cddr rest)))
	  ((null rest) frame)
	(check-class frame (#^rdfs:domain (car rest))) 
	(check-class (cadr rest) (#^rdfs:range (car rest))) 
	(setf (slotv frame (car rest)) 
	      (cadr rest))))))

(defun rdfs-find (value &key slot class (source *collabrx-sparql*))
  (do-sparql-one-var source
    `(:select (?s) ()
	      (?s ,(if slot slot '?p) ,value)
	      ,@(if class `((?s #$rdf:type ,class))))))

;;; This has to be relative to a frame source so you can check for taken ids. Or something.
;;; Currently broken

(defun gensym-instance-frame (class)
  (let ((uri (string+ (frame-uri class) "/" (string (gensym (frame-label class))))))
    (if (uri-used? *collabrx-sparql* uri)
	(gensym-instance-frame class)
	(intern-uri uri))))

(defgeneric uri-used? (source uri))

(defun rdfs-typep (frame class)
  (or (slot-has? frame #$rdf:type class)
      (some #'(lambda (subclass)
		(rdfs-typep frame subclass))
	    (rdfs-subclasses class))))

(defun rdfs-subclasses (class)
  (slotv-inverse class #$rdfs:subClassOf))


;;; Method system.  Need to think about this, RDF things can be multiply typed and there is no ordering.  
;;; Alternative is to map rdf to CLOS classes which have a more well-behaved orderng.

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
;(defun rdfs-classes (thing)
;  (utils::transitive-closure (#^rdf:type thing) #'#^rdfs:subClassOf))

;;; this is way wrong, but will do for now
(defun rdfs-method (name thing)
  (let ((classes (rdfs-classes thing))
	(methodtable (rdfs-methodtable name)))
    (some #'(lambda (class) (gethash class methodtable )) classes)))

(defmacro rdfs-call (name firstarg &rest restargs)
  `(apply (rdfs-method ',name ,firstarg)
	  ,firstarg
	  ,restargs))
