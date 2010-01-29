(in-package :sw)

;;; Singleton class to represent frames defined in code

(defclass code-source (frame-source) 
  ())

(defvar *code-source* (make-instance 'code-source))

(defmethod uri-used? ((source code-source) uri)
  (frame-named (expand-uri uri)))

;;; Done in memory, so nothing more to do
(defmethod delete-triple ((source code-source) s p o &key write-graph)
  )

(defmethod write-triple ((source code-source) s p o &key write-graph)
  )

;;; CCC this is getting called from compile, no idea why
(defmethod make-load-form ((source code-source) &optional env)
  `(or *code-source*
       (setf *code-source* (make-instance 'code-source))))

(defmethod do-write-group ((source code-source) async? proc)
  (funcall proc))



;;; class FRAME not defined yet.
(defmethod fill-frame-from (frame (source code-source) &key inverse?)
  (declare (ignore frame source inverse?))
  )
