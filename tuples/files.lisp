(in-package :sw)

;;; header optional

(defclass* stream-tuple-set (base-tuple-set)
  ((stream nil)
   (fields nil)                         ;+++ move up?
   (delimiter #\Tab)
   (n-header-lines 1))
  (:initable-instance-variables delimiter n-header-lines fields))

(defmethod* tset-fields ((tset stream-tuple-set))
  (or fields
      (progn (maybe-init-query tset)
             fields)))

#|
Not clear what to do -- ext grid apparently demands a count.
Well, this should be NIL and the kludgery handled at the grid level, at least.
|#
(defmethod tset-count ((tset stream-tuple-set))
;  nil
  10000000
  )


;;; A very stupid method (+++ have a mixin that caches positions)
(defmethod tset-set-position ((tset stream-tuple-set) pos)
  (reopen-stream tset)
  (dotimes (n pos)
    (parse-line tset)))                 ;+++ pass-tuple

(defmethod tset-reset-position ((tset stream-tuple-set))
  (reopen-stream tset))

;;; stupid default method
(defmethod tset-set-position ((tset base-tuple-set) pos)
  (tset-reset-position tset)
  (dotimes (n pos)
    (tset-iterator-next tset)))

;;; rereads the whole file each time!  Temp +++
;;; this is a general method for streams that are stupid...
(defmethod tset-subseq ((tset base-tuple-set) start length)
  (tset-set-position tset start)
  (utils:collecting
   (dotimes (n length)
     (let ((tup (tset-iterator-next tset)))
       (if tup 
	   (utils::collect tup)
	   (return))))))

;;; Header lines can be zero or >1, but only at most one is processed.
(defclass* file-tuple-set (stream-tuple-set)
  (file)
  :initable-instance-variables)

(defmethod* get-line ((tset stream-tuple-set))
  (or (read-line stream nil nil)
      (throw :eof nil)))

(defun remove-redundant-quotes (str)
  (if (and (>= (length str) 2)
           (char= (char str 0) #\")
           (char= (char str (1- (length str))) #\"))
      (subseq str 1 (1- (length str)))
      str))

(defun coerce-value (value)
  (typecase value
    (null (error "Cant' coerce nil to a value"))
    (list
     (warn "Multiple values ~A" value)
     (coerce-value (car value)))
    (number value)
    (keyword value)
    (string
     (if (zerop (length value))
         nil
         ;; +++ this could be much faster
         (let ((n (ignore-errors (read-from-string value))))
           (if (numberp n) n value))))
    (t (error "can't coerce ~A" value))))

(defmethod* parse-line ((tset stream-tuple-set) &key field-process)
  (mapcar #'(lambda (field)
              (setf field (remove-redundant-quotes field))
              (when field-process
		(setf field (funcall field-process field)))
              field)
          (string-split (get-line tset) delimiter)))

(defmethod* maybe-init-query :before ((tset file-tuple-set))
            (unless stream
              (setf stream (open file))))

(defmethod* reopen-stream ((tset file-tuple-set))
;  (when stream
;    (close stream))
  (setf stream (open file))
  (maybe-init-query tset)
  )

;;; assumes we are at the start of the stream
(defmethod* maybe-init-query ((tset stream-tuple-set))
  (let ((headers nil))
    (when (plusp n-header-lines)
      (setf headers (parse-line tset :field-process #'coerce-field))
      ;; discard extra lines
      (dotimes (i (1- n-header-lines))
        (parse-line tset)))
    (unless fields
      (setf fields (mapcar #'coerce-field headers)))))

(defmethod* read-data-line ((tset stream-tuple-set))
  (catch :eof
    (let ((tuple (make-tuple)))
      (mapc #'(lambda (field val)
                (setf (tuple-field tuple field) val))
            fields (parse-line tset :field-process #'coerce-value))
      tuple)))

;;; I am my own iterator, for now +++
(defmethod tset-iterator ((tset stream-tuple-set))
  (tset-reset-position tset)
  tset)

(defmethod tset-iterator-next ((tset stream-tuple-set))
  (read-data-line tset))

