(in-package :sw)

;;; Singleton class to represent frames defined in code

(defclass code-source (frame-source) 
  ())

(defvar *code-source* (make-instance 'code-source))

;;; Done in memory, so nothing more to do
(defmethod delete-triple ((source code-source) s p o &key write-graph)
  )

(defmethod write-triple ((source code-source) s p o &key write-graph)
  )

(defmethod make-load-form ((source code-source) &optional env)
  `(or *code-source*
       (setf *code-source* (make-instance 'code-source))))

;;; class FRAME not defined yet.
(defmethod fill-frame-from (frame (source code-source) &key inverse?)
  (declare (ignore frame source inverse?))
  )
