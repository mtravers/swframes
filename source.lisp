(in-package :swframes)

#|
Just starting on this.

A frame source is a sparql endpoint or other source of frames and content.

Concept still developing.  

Dereferencing is a "frame source" of sorts...

|#

(defclass frame-source ()
  ()
;  (:abstract t)
  )

(defgeneric writeable? (frame-source))

;;; Generate a guaranteed unique new URI
(defgeneric gensym-uri (frame-source &optional prefix))
  
