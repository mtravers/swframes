(in-package :swframes)

#|
Notes:
- range
- need to avoid property name collisions 
- what to do with these triples?  Write them out?

Defclass
- Might want to use a short name for slots and have uri generated with a template.

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
      (a! name  #$rdfs:type #$rdfs:Class)
      (mapc #'(lambda (superclass)
		(a! name #$rdfs:subClassOf superclass))
	    superclasses)
      (mapc #'(lambda (slotdef)
		(let ((slot (car slotdef)))
		  (a! slot #$rdfs:type #$rdfs:Property)
		  (a! slot #$rdfs:domain name)))
	    slots)
      `(progn ,@clauses))))


;;; This is obviously wrong, but I'm not sure what's right.
;;; Probably source should be a writeable sparql endpoint?

(defclass programmatic-frame-source (frame-source)
  ())

(defmethod fill-frame-from ((frame frame) (source programmatic-frame-source))
  )

(defvar *programmatic-frame-source* (make-instance 'programmatic-frame-source))

;;; does no checking, we should have a flag. Also option for specifying a name or partial name.
(defun rdfs-make-instance (class &rest slots)
  (let ((frame (gensym-instance-frame class)))
    (setf (frame-source frame) *programmatic-frame-source*)
    (setf (slotv frame #$rdf:type) class)
    (do ((rest slots (cddr rest)))
	((null rest) frame)
      (setf (slotv frame (car rest)) 
	    (cadr rest)))))

(defun rdfs-find (value &key slot class (source *collabrx-main*))
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
	(uri uri))))

(defgeneric uri-used? (source uri))



