(in-package :sw)

;;; Not a source really, a sink.  Maybe need to refine these classes (and/or integrate with tuplesets) +++
(defclass bulk-out (frame-source)
  ())

(defclass* file-bulk-out (bulk-out)
  (file stream)
  (:initable-instance-variables file))

(defmethod* dump-frames ((source file-bulk-out) frames)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (setf stream s)
    (dolist (f frames)
      (frame-dump source f))))

(defmethod frame-dump ((source bulk-out) frame)
  (for-frame-slots (frame slot value)
		   (dolist (elt value)
		     (write-triple source frame slot elt))))

;;; NTriples writer (not to be confused with n3)
(defclass nt-writer (file-bulk-out) ())

(defmethod* write-triple ((out nt-writer) s p o)
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

;;; +++ prob inadequate -- should check for printability.
(defmethod* write-entity ((out nt-writer) (thing t))
  (prin1 (mt:fast-string thing) stream))


#|
Recipe for building a local semweb drugbank.

(bio::parse-drugcards bio::*drugbank-drugcards-file*)

(let ((writer (make-instance 'nt-writer :file "/misc/kbs/drugbank.nt")))
  (dump-frames writer bio::*drugbank-frames*))

> scp /misc/kbs/drugbank.nt bigmac.local:/usr/local/virtuoso/bundles/drugbank.nt

On Bigmac:

/usr/local/virtuoso/virtuoso-opensource/bin/isql localhost:9000

;;; 192 - allows strings with escaped "s to be parsed.
isql> db.dba.ttlp(file_to_string_output('/usr/local/virtuoso/bundles/drugbank.nt'), '', 'http://collabrx.com/drugbank', 192)

Note: should have something to bulk-erase earlier triples.

|#			 

;;; given a set of frames, finds references to frames outside the set
;;; for now, does not process predicates or inverse links (+++ add those as options)
(defun frameset-external-refs (frames)
  (mt:collecting
    (dolist (f frames)
      (for-frame-slots (f slot value)
		       (dolist (elt value)
			 (if (and (frame-p elt)
				  (not (member elt frames)))
			     (mt::collect-new elt)))))))
			      
				   
			      
