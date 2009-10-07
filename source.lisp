(in-package :swframes)

#|
A frame source is a sparql endpoint or other source of frames and content.

Dereferencing is a "frame source" of sorts...

|#

(export '(frame-source writeable? with-frame-source))

(defclass frame-source ()
  ()
;  (:abstract t)
  (:documentation "A CLOS object that represents a source of frame information; possibly writeable as well.  This is an abstract class")
  )


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

;;; class FRAME not defined yet.
(defmethod fill-frame-from (frame (source code-source) &key inverse?)
  (declare (ignore frame source inverse?))
  )

#|
;;; Not used (but probably should be)
(defclass frame-generation-mixin (frame-source)
  ((uri-base :initarg :uri-base :initform nil)))

;;; Generate a guaranteed unique new URI
;;; +++ this should be developed out
(defgeneric gensym-uri (frame-source &optional prefix))
|#
