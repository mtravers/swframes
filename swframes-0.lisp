(in-package :swframes)

#|
This file has the minimum needed to get the frame system working (esp. the reader)
|#

(defstruct (frame (:print-function frame-printer))
  uri
  (slots (make-hash-table :test #'eq)) ;ht mapping preds to values (poss. multiple)
  (inverse-slots nil)
  ;; Below here is various state-manipulation info; very in flux
  source
  loaded?				;T if slots have been loaded
  dirty?				;T if needs to be written back out, or list of preds to write out.
  dereferenced? 
  )

(defun frame-printer (frame stream ignore)
  (mt:report-and-ignore-errors
    (format stream "#$~A" (frame-name frame))))

(set-dispatch-macro-character #\# #\$ 'pound-dollar-frame-reader)
(set-dispatch-macro-character #\# #\^ 'pound-carat-frame-reader)

;;; +++ would be good to allow #$"sdasdad" for hard to parse names
(defun pound-dollar-frame-reader (stream char arg)
  (declare (ignore char arg))
  (uri (frames::read-fname stream)))

;;; old school
(defun pound-carat-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let ((slot (uri (frames::read-fname stream))))
    `(lambda (f) (msv f ,slot))))

;;; new, works with #^.  Slightly ugly
(defun pound-carat-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((slot (uri (frames::read-fname stream)))
	 ;; +++ probably these should be in their own package
	 (symbol (intern (frame-uri slot) :keyword)))
    (compile symbol #'(lambda (f) (msv f slot)))
    (eval (print `(defsetf ,symbol (f) (v) `(set-slotv ,f ,,slot ,v))))
    symbol))

;;; I suppose we should have an #v (or something) for inverse-slots...

;;; reader 
(defun uri (thing)
  (typecase thing
    (frame thing)
    (string (intern-uri (expand-uri thing)))
    (t (error "Can't turn ~A into a URI" thing))))

;;; Gets redefed later
(defun expand-uri (string)
  string)

(defvar *uri->frame-ht* (make-hash-table :test 'equal))

(defun intern-uri (uri &optional source)
  (assert (stringp uri))
  (or (gethash uri *uri->frame-ht*)
      (setf (gethash uri *uri->frame-ht*)
	    (make-frame :uri uri 
			:source source
			))))

(defun unintern-uri (uri)
  (remhash uri *uri->frame-ht*))

;;; this isn't working for some reason...interned  objects are not frames?
(defmethod make-load-form ((frame frame) &optional ignore)
  (declare (ignore ignore))
  `(intern-uri ,(frame-uri frame)))
