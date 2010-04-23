(in-package :sw)

(export '(dump-frames
	  dump-frames-to-fasl
	  nt-writer))

;;; Not a source really, a sink.  Maybe need to refine these classes (and/or integrate with tuplesets) ++
(defclass* bulk-out (frame-source)
  ())

(defclass* file-bulk-out (bulk-out)
  (file stream)
  (:initable-instance-variables file))

(defgeneric dump-frames (source frames)
  (:documentation "Write out a set of frames to the SOURCE (actually a sink)."))

(defmethod* dump-frames ((source file-bulk-out) frames)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (setf stream s)
    (dolist (f frames)
      (frame-dump source f))))

(defmethod frame-dump ((source bulk-out) frame)
  (for-frame-slots (frame slot value)
		   (dolist (elt value)
		     (write-triple source frame slot elt))))

;;; NTriples writer 
(defclass* nt-writer (file-bulk-out) ()
	   (:documentation "Source (sink) for writing out frames in Ntriples format (not to be confused with n3)"))

(defmethod* write-triple ((out nt-writer) s p o &key write-graph)
  (declare (ignore write-graph))
  (terpri stream)
  (write-entity out s)
  (write-char #\Space stream)
  (write-entity out p)
  (write-char #\Space stream)
  (write-entity out o)
  (write-char #\Space stream)
  (write-char #\. stream))
	  
(defmethod* write-entity ((out nt-writer) (thing frame))
  (format stream "<~A>" (frame-uri thing)))

;;; +++ prob inadequate type handling
(defmethod* write-entity ((out nt-writer) (thing t))
  (unless (or (typep thing 'string) (typep thing 'number))
    (prin1 (fast-string thing) stream)))

;;; given a set of frames, finds references to frames outside the set
;;; for now, does not process predicates or inverse links (++ add those as options)
(defun frameset-external-refs (frames)
  (collecting
    (dolist (f frames)
      (for-frame-slots (f slot value)
		       (dolist (elt value)
			 (if (and (frame-p elt)
				  (not (member elt frames)))
			     (collect-new elt)))))))
			      
;;; Fasl dump
(defvar *fasl-dump-temp*)
(defun dump-frames-to-fasl (frames file &key (variable '*fasl-dump-temp*))
  (set variable frames)
  (dump-vars-to-file (list variable) file))
