(in-package :swframes)

#|
A frame source is a sparql endpoint or other source of frames and content.

Dereferencing is a "frame source" of sorts...

|#

(export '(frame-source writeable? with-frame-source))

(defclass frame-source ()
  ()
;  (:abstract t)
  (:documentation "A CLOS object that represents a source of frame information; possibly writeable as well.  This is an abstract class"))

(defgeneric writeable? (frame-source)
  (:documentation "True if the source is capable of being written to."))
  
(defmacro with-frame-source ((source) &body body)
  "Execute BODY with *default-frame-source* set to SOURCE"
  `(let ((*default-frame-source* ,source))
     ,@body))
	 
;;; Singleton class to represent frames defined in code

(defclass code-source (frame-source) 
  ())

(defvar *code-source* (make-instance 'code-source))

(defmethod uri-used? ((source code-source) uri)
  (frame-named (expand-uri uri)))

;;; Done in memory, so is done (CCC)
(defmethod delete-triple ((source code-source) s p o)
  )

(defmethod write-triple ((source code-source) s p o)
  )

;;; CCC this is getting called from compile, no idea why
(defmethod make-load-form ((source code-source) &optional env)
  `(or *code-source*
       (setf *code-source* (make-instance 'code-source))))



;;; Write classes defined in code to a database.  This is only called by hand at the moment.
(defun write-code-source-classes (to)
  (with-write-group (to)		;+++ this needs to be moved after definition
    (dolist (class (slotv-inverse  #$rdfs:Class #$rdf:type))
      (when (eq *code-source* (frame-source class))
	(write-frame class :source to)))))

;;; class FRAME not defined yet.
(defmethod fill-frame-from (frame (source code-source) &key inverse?)
  (declare (ignore frame source inverse?))
  )

#|
;;; Not used (but probably should be +++) 

(defclass frame-generation-mixin (frame-source)
  ((uri-base :initarg :uri-base :initform nil)))

;;; Generate a guaranteed unique new URI
(defgeneric gensym-uri (frame-source &optional prefix))
|#
