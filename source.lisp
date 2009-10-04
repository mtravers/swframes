(in-package :swframes)

#|
A frame source is a sparql endpoint or other source of frames and content.

Dereferencing is a "frame source" of sorts...

|#

(export '(frame-source writeable? with-frame-source))

(defclass frame-source ()
  ()
;  (:abstract t)
  (:documentation "A CLOS object that represents a source of frame information; possibly writeable as well")
  )

(defclass frame-generation-mixin (frame-source)
  ((uri-base :initarg :uri-base :initform nil)))

(defgeneric writeable? (frame-source))

;;; Generate a guaranteed unique new URI
;;; +++ this should be developed out
(defgeneric gensym-uri (frame-source &optional prefix))
  
(defmacro with-frame-source ((source) &body body)
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
