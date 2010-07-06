(in-package :swframes)

;;; Note: this has been pulled out of sparql machinery, some further cleanup may be required.

(rdfs-def-class #$rdf:Property ())

(rdfs-def-class #$sw:slots/specialSlot (#$rdf:Property))

;;; special write behaviors:  don't write, serialize/deserialize lisp, list handling...

(defun declare-special-slot (slot type)
  #.(doc 
     "Declares SLOT to have special behavior defined by TYPE.  Current TYPEs are:"
     "#$sw:slots/LispValueSlot:"
     "   Slots of this class can hold any printable Lisp object."
     "#$sw:slots/TransientSlot:"
     "  Slots of this class never write their values to the database.")
  (setf (ssv slot #$rdf:type) type
        (ssv slot #$sw:specialhandling) t) ;CCC +++ if we methodize slot functions this flag may be able to go away
  (classify-frame slot))

;;; debugging only
(defun undeclare-special-slot (slot)
  (setf (slotv slot #$rdf:type) nil
        (slotv slot #$sw:specialhandling) nil)  )

;;; LispValueSlot

(rdfs-def-class #$sw:slots/LispValueSlot (#$sw:slots/specialSlot))

(rdfs-defmethod deserialize-value ((slot #$sw:slots/LispValueSlot) value)
		(if (stringp value)
		    (read-from-string value)
		    value))

;;; TransientSlot

(rdfs-def-class #$sw:slots/TransientSlot (#$sw:slots/specialSlot))

;;; Sometimes these unserializable slots get serialized, so ignore them
(rdfs-defmethod deserialize-slot ((p #$sw:slots/TransientSlot) frame value)
		(declare (ignore frame value))
		nil)

(rdfs-defmethod deserialize-value ((p #$sw:slots/TransientSlot) value)
		(declare (ignore value))
		nil)


