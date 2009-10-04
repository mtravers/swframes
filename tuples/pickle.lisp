(in-package :sw)

;;;; ??? should this be a tset itself? It has fields, etc.

(defclass* tset-writer ()
  (tset
   (fields nil)
   (separator #\Tab))
  :initable-instance-variables)

(defclass* tset-stream-writer (tset-writer)
  (stream)
  :initable-instance-variables)

;;; change the name of these -- html streams are open and closed by caller
(defmethod* open-stream ((writer tset-stream-writer))
  stream
  )

(defmethod close-stream ((writer tset-stream-writer))
  )

(defclass* tset-file-writer (tset-stream-writer)
  (file)
  :initable-instance-variables)

(defmethod* open-stream ((writer tset-file-writer))
  (setf stream (open file :direction :output :if-exists :supersede)))

(defmethod* close-stream ((writer tset-file-writer))
  (close stream)
  (setf stream nil))

(defmacro for-all-tuples ((var tset) &body body)
  `(progn
     (tset-reset-position ,tset)
     (do ((,var (tset-iterator-next ,tset)
		(tset-iterator-next ,tset)))
	 ((null ,var))
       ,@body)))

(defmethod* write-tset ((writer tset-writer))
  (unless fields
    (setf fields (tset-fields tset)))
  (setf stream (open-stream writer))
  (write-header-line writer)
  (let ((serial -1))
    (for-all-tuples (tuple tset)
		    (write-tuple writer tuple (incf serial))))
  (close-stream writer))

(defmethod* write-header-line ((writer tset-stream-writer))
  (format stream (format nil "~~&~~{~~A~~^~A~~}" separator) fields));+++ (circular-list separator)

(defmethod* write-tuple ((writer tset-stream-writer) tuple &optional serial)
  (declare (ignore serial))
  (let ((values
	 (collecting
	  (dolist (f fields)
	    (collect (tuple-field tuple f))))))
    (format stream (format nil "~~%~~{~~A~~^~A~~}" separator) values)));+++ (circular-list separator))))    
    
(defclass* tset-pickler (tset-writer)
  (tset-frame
   tuple-prefix
   field-uris
   sparql)
  :initable-instance-variables)
  

(defmethod open-stream ((writer tset-pickler))
  )

(defmethod close-stream ((writer tset-pickler))
  )

(defmethod* write-header-line ((writer tset-pickler))
  )

;;; how this works depends on how tuples and frames interact.  

(rdfs-def-class #$crx:tuple ())

(def-cached-function tuple-field-slot (key)
  (let ((slot (intern-uri (formatn "crx:tuplefield/~A" (string key)))))
    (declare-special-slot slot #$crx:slots/LispValueSlot)
    slot))

(defun tuple->frame (tuple &key base)
  (let ((f (gensym-instance-frame #$crx:tuple :fast? t :base base)))
    (tuple-dofields (key val) tuple
		    (setf (ssv f (tuple-field-slot key)) val))
    f))

(defmethod* write-tuple ((writer tset-pickler) tuple &optional serial)
  (let ((frame (tuple->frame tuple :base (string+ (frame-uri tset-frame) "/tuple"))))
    (add-triple frame #$crx:slots/tuple-order serial)
    (add-triple tset-frame #$crx:slots/includes-tuple frame :to-db t) ;have to do these one at a time or it times out
    (write-frame frame :source sparql :no-delete? t)
  ))

;;; make multiple values into single values (needed by display)
(defun frame->tuple (frame)
  (collecting 
   (maphash #'(lambda (key value)
		(collect (cons key (car value))))
	    (frame-slots frame)
	    )))
    
;;; New method, much better

;;; class represents a tset in the kb, can pull out subsequences of tuples
(defclass* pickled-tset (base-tuple-set)
  (frame
   (count nil)
   (fields nil))
  (:initable-instance-variables))

(defmethod* tset-fields ((ts pickled-tset))
  (or fields
      (setf fields
	    ;; just use the first one as a representative sample. +++
	    (delete #$crx:slots/tuple-order (tuple-fields (car (tset-subseq ts 0 1)))))))


(defmethod* tset-count ((ts pickled-tset))
  (or count
      (setf count
	    (coerce-number
	     (cadr (caar  
		    (do-sparql nil
		      `(:select ("count(*)") () 
				(,frame #$crx:slots/includes-tuple ?s)))))))))

(defmethod* tset-subseq ((tset pickled-tset) start length)
  (let ((tuple-frames 
	 (bulk-load-query nil
			      `(:select (?s) () 
					(,frame #$crx:slots/includes-tuple ?s)
					(?s #$crx:slots/tuple-order ?order)
					(:filter (>= ?order ,start))
					(:filter (< ?order ,(+ start length)))))))

    (when tuple-frames
      (mapcar #'frame->tuple tuple-frames))))


(defun depickle (frame)
  (setf (ssv frame #$crx:bioblog/tupleset)
	(make-instance 'pickled-tset :frame frame)))

; (knewos::depickle #$crx:bioblog/GridContent/115)

(defmethod tset-pickle ((tset tuple-set) &key tset-frame)
  (reopen-stream tset)
  (unless tset-frame
    (setf tset-frame
	  (gensym-instance-frame #$crx:tupleset)))
  (let ((writer (make-instance 'tset-pickler
			       :sparql *default-frame-source*
			       :tset-frame tset-frame
			       :tset tset)))
    (write-tset writer)))
    
