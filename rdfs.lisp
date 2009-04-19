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


|#


(defmacro rdfs-def-class (name superclasses &body slots)
  (let ((clauses nil))
    (flet ((a! (s p o)
	     (push `(assert-triple s p o) clauses)))
      (a! name  #$rdfs:type #$rdfs:Class)
      (mapc #'(lambda (superclass)
		(a! name #$rdfs:subClassOf superclass))
	    superclasses)
      (mapc #'(lambda (slotdef)
		(let ((slot (car slotdef)))
		  (a! slot #$rdfs:type #$rdfs:Property)
		  (a! slot #$rdfs:domain name))))
      `(progn ,@clauses))))



(defun rdfs-make-instance (name class &rest slots)

  )
