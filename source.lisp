(in-package :swframes)

#|
A frame source is a sparql endpoint or other source of frames and content.

Dereferencing is a "frame source" of sorts...

|#

(export '(frame-source writeable? with-frame-source *default-frame-source*))

;;; set later on
(defvar *default-frame-source* nil
  "A FRAME-SOURCE used by default when frames are created.  Can by dynamically bound.")	

(defclass* frame-source ()
  ((name nil)
   (writeable? nil))
;  (:abstract t)
  :initable-instance-variables
  (:documentation "A CLOS object that represents a source of frame information; possibly writeable as well.  This is an abstract class"))

(defmethod* print-object ((sparql frame-source) stream)
  (format stream "#<~A ~A ~A>" 
	  (type-of sparql)
	  (or name "?")
	  (if writeable? "[w]" "[nw]")
	  ))

(defgeneric writeable? (frame-source)
  (:documentation "True if the source is capable of being written to."))
  
(defmethod uri-used? ((source frame-source) uri)
  (frame-named (expand-uri uri)))

(defmethod do-write-group ((source null) async? proc)
  (do-write-group *default-frame-source* async? proc))

(defmethod do-write-group ((source frame-source) async? proc)
  (if async?
      (background-funcall proc)
      (funcall proc)))

(defmacro with-frame-source ((source) &body body)
  "Execute BODY with *default-frame-source* set to SOURCE"
  `(let ((*default-frame-source* ,source))
     ,@body))
	 
(defmacro with-write-group ((&optional (endpoint '*default-frame-source*) &key async?) &body body)
  `(do-write-group ,endpoint ,async?
     #'(lambda ()
	 ,@body)))

#|
;;; Not used (but probably should be +++) 

(defclass frame-generation-mixin (frame-source)
  ((uri-base :initarg :uri-base :initform nil)))

;;; Generate a guaranteed unique new URI
(defgeneric gensym-uri (frame-source &optional prefix))
|#
