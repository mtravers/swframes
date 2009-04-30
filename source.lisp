(in-package :swframes)

#|
A frame source is a sparql endpoint or other source of frames and content.

Concept still developing.  

Dereferencing is a "frame source" of sorts...

|#

(export '(frame-source writeable? with-frame-source))

(defclass frame-source ()
  ()
;  (:abstract t)
  )

(defclass frame-generation-mixin (frame-source)
  ((uri-base :initarg :uri-base :initform nil)))

(defgeneric writeable? (frame-source))

;;; Generate a guaranteed unique new URI
(defgeneric gensym-uri (frame-source &optional prefix))
  
;;; our local, writeable frame repository
(defvar *local-source*) 		;+++ unify with *collabrx-source*

#|  This has to come later
(setq *local-source*
      (make-instance 'sparql-endpoint
		     :uri "http://virtuoso.collabrx.com/sparql/"
		     :base-uri "http://collabrx.com/rdf/"
		     ))      


|#


(defmacro with-frame-source ((source) &body body)
  `(let ((*default-frame-source* ,source))
     ,@body))
	 
