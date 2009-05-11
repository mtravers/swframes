(in-package :swframes)

#|
This file has the minimum needed to get the frame system working (esp. the reader)
|#

(defstruct (frame (:print-function frame-printer))
  uri
  (slots (make-hash-table :test #'eq)) ;ht mapping preds to values (poss. multiple)
  (inverse-slots (make-hash-table :test #'eq))
  ;; Below here is various state-manipulation info; very in flux
  source
  loaded?				;T if slots have been loaded
  dirty?				;T if needs to be written back out, or list of preds to write out.
  dereferenced? 
  )

(defun frame-printer (frame stream ignore)
  (mt:report-and-ignore-errors
    (format stream "#$~A" (frame-name frame))))

(set-dispatch-macro-character #\# #\$ 'pound-dollar-frame-reader (frames::frames-readtable))
(set-dispatch-macro-character #\# #\^ 'pound-carat-frame-reader (frames::frames-readtable))
(set-dispatch-macro-character #\# #\v 'pound-inverse-frame-reader (frames::frames-readtable))

(set-dispatch-macro-character #\# #\$ 'pound-dollar-frame-reader )
(set-dispatch-macro-character #\# #\^ 'pound-carat-frame-reader )
(set-dispatch-macro-character #\# #\v 'pound-inverse-frame-reader )


(defun pound-dollar-frame-reader (stream char arg)
  (declare (ignore char arg))
  (uri (read-fname stream)))

;;; +++ would be good to allow #$"sdasdad" for hard to parse names
(defun read-fname (stream)
  (let ((name
	 (read-until 
	  stream
	  (lambda (char)
	    (or (member char *whitespace*)
		(member char '(#\( #\)))))
	  (new-string)
	  t)))
    (assert (not (char= #\# (char name 0)))) ;catch this common error
    name))

(defpackage :swfuncs)

;;; New, works with setf without a lot of hair.   But it means we have to type #'#^ to use it as a functional argument...ugh.
;;; Whups -- fun defined at read time, won't necessarily be available later. Damn! 
;;; Poss solution -- put all def'd symbols in a special variable somewhere, which gets written to a fasl as the last step of compilation
;;; of a system, and read back in early.  Ugly..
(defun pound-carat-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((slot (uri (frames::read-fname stream)))
	 (symbol (intern (frame-uri slot) :swfuncs)))
    (compile symbol #'(lambda (f) (msv f slot)))
    (eval `(defsetf ,symbol (f) (v) `(set-slotv ,f ,,slot ,v)))
    symbol
    ))


;;; go back to this, which won't work with setf
(defun pound-carat-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((slot (uri (frames::read-fname stream))))
    `(lambda (f) (msv f ,slot))))
    

(defun pound-inverse-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((slot (uri (frames::read-fname stream)))
	 (symbol (intern (string+ "inverse_" (frame-uri slot)) :swfuncs)))
    (compile symbol #'(lambda (f) (msv-inverse f slot)))
; No setf for now
;    (eval `(defsetf ,symbol (f) (v) `(set-slotv-inverse ,f ,,slot ,v)))
    symbol))

;;; See above
(defun pound-inverse-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((slot (uri (frames::read-fname stream))))
    `#'(lambda (f) (msv-inverse f ,slot))))

;;; use this to temporarily patch all (setf (#^slot ... and similar forms to (setf (msv-hack #$slot ...))
(defmacro msv-hack (slot frame)
  `(msv ,frame ,slot))

(defsetf msv-hack (s f) (v) `(set-slotv ,f ,s ,v))

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

(defvar *default-frame-source* nil)	;Bind this for frame creation 
(defvar *mark-new-frames-loaded?* nil)	;Bind this for frame creation 

(defun intern-uri (uri &optional (source *default-frame-source*) (mark-loaded? *mark-new-frames-loaded?*))
  (assert (stringp uri))
  (setf uri (expand-uri uri))
  (or (frame-named uri)
      (intern-uri-0 uri 
		    (make-frame :uri uri 
				:source source
				:loaded? mark-loaded?
				))))

(defun frame-named (uri)
  (gethash (expand-uri uri) *uri->frame-ht*))

(defun intern-uri-0 (uri frame)
  (setf (gethash uri *uri->frame-ht*) frame))

(defun unintern-uri (uri)
  (remhash uri *uri->frame-ht*))

;;; Dangerous
(defun rename-frame (f new-name)
  (if (frame-named new-name) 
      (error "There is already a frame named ~A" new-name))
  (unintern-uri (frame-uri f))
  (setf (frame-uri f) new-name)
  (intern-uri-0 new-name f))

;;; this isn't working for some reason...interned  objects are not frames?
(defmethod make-load-form ((frame frame) &optional ignore)
  (declare (ignore ignore))
  `(intern-uri ,(frame-uri frame)))

;;; when test framework is in place
'(define-test rename (x)
  (let ((x (uri "test27")))
    (assert (frame-fresh? x nil))
    (rename-frame x "test27renamed")
    (assert (frame-fresh? x nil))
    (assert (equal (frame-uri x) "test27renamed"))))
