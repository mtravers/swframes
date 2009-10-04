(in-package :sw)

;;; Maybe break out a mixin for anything
(defclass* in-memory-tuple-set (base-tuple-set)
  ((tuples nil)
   ;; maps fields to index hash tables
   (indices (make-hash-table))
   (tail nil)				;iterator support
   )
  (:initable-instance-variables tuples))

(defmethod* tset-tuple-list ((tset in-memory-tuple-set))
  tuples)

(defmethod* tset-add-tuple ((tset in-memory-tuple-set) tuple)
  (push tuple tuples)
  (update-indices tset tuple))

(defmethod* tset-count ((tset in-memory-tuple-set))
  (length tuples))

(defmethod* tset-map-tuples ((tset in-memory-tuple-set) proc)
  (dolist (tuple tuples)
    (funcall proc tuple)))

;;; iterator support
(defmethod* tset-reset-position ((tset in-memory-tuple-set))
  (setf tail tuples))

(defmethod* tset-iterator-next ((tset in-memory-tuple-set))
  (pop tail))

(defmethod* tset-fields ((ts in-memory-tuple-set))
  ;; just use the first one as a representative sample. +++
  (tuple-fields (car tuples)))

(defmethod* tset-sort ((ts in-memory-tuple-set) field predicate)
  (setf tuples
	(sort tuples predicate :key (tuple-field-accessor field)) ))

;;; Indexing

(defmethod tset-indexable? ((tset in-memory-tuple-set))
  t)

(defmethod* update-indices ((tset in-memory-tuple-set) tuple)
  (maphash #'(lambda (field index)
	       ;; +++ multiple values
	       (push tuple (gethash (tuple-field tuple field) index)))
	   indices))

(defmethod* add-index ((tset in-memory-tuple-set) field)
  (let ((index (setf (gethash field indices)
		     (make-hash-table :test #'equal))))
    (dolist (tuple tuples)
      (push tuple (gethash (tuple-field tuple field) index)))
    index))

(defmethod* tset-field-index ((tset in-memory-tuple-set) field)
  (or (gethash field indices)
      (add-index tset field)))
      
(defmacro for-values ((v tset field) &body body)
  `(maphash #'(lambda (,v val)
		,@body)
	    (tset-field-index ,tset ,field)
	    ))

;;; Returns a list, but ought to return a new set...
(defmethod* lookup-tuples ((tset in-memory-tuple-set) field value)
  (let ((index (gethash field indices)))
    (if index
	(gethash value index)
	(lookup-tuples-slow tset #'(lambda (tup) (equal (tuple-field tup field) value))))))

(defmethod* lookup-tuples-slow ((tset in-memory-tuple-set) predicate)
  (collecting
   (dolist (tup tuples)
     (when (funcall predicate tup)
       (collect tup)))))

(defun file-to-memory-tset (file)
  (let ((ots (make-instance 'file-tuple-set :file file))
	(nts (make-instance 'in-memory-tuple-set)))
    (tset-copy ots nts)
    nts))
