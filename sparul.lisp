(in-package :swframes)

;;; see /misc/sourceforge/hg/crx_rails/lib/active_rdf_patches/active_rdf_sparul.rb
;;; transactions

;;; no-op for now (+++)
(defmacro with-sparul-transaction ((endpoint) &body body)
  `(progn ,@body))

(defmethod* write-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (do-sparql sparql
    (build-insert sparql s p o)))

(defmethod* delete-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (do-sparql sparql
	   (build-delete sparql s p o)))

(defmethod* build-insert ((sparql sparql-endpoint) s p o)
  (format nil
	  "INSERT INTO GRAPH ~A { ~A ~A ~A }"
	  (sparql-term (uri write-graph)) (sparql-term s) (sparql-term p) (sparql-term O)))

(defmethod* build-delete ((sparql sparql-endpoint) s p o)
  (let ((base (format nil "DELETE FROM GRAPH ~A { ~A ~A ~A }" (sparql-term (uri write-graph)) (sparql-term s) (sparql-term p) (sparql-term O))))
    (when (or (symbolp s) (symbolp p) (symbolp o) )
      (pushstring base (format nil " WHERE { ~A ~A ~A }" (sparql-term s) (sparql-term p) (sparql-term O))))))


;;; A stupid method that deletes all existing triples and writes them all anew.
(defmethod write-frame ((frame frame) &optional (sparql (frame-source frame)))
  (with-sparul-transaction (sparql)
    (delete-triple sparql frame '?p '?o)
    (dolist (slot (%frame-slots frame))
      (dolist (val (slotv frame slot))
	(write-triple sparql frame slot val))))
  frame) 

;;; Tests
#|


(defvar *collabrx-sparql-writeable*
  (make-instance 'sparql-endpoint
		 :uri "http://sparql.collabrx.com/sparql/"
		 :writeable? t
		 :write-graph "http://collabrx.com/my_graph"))

(test2 *collabrx-sparql-writeable*)

(defmethod test3 ((sparql sparql-endpoint))
  (let* ((frame (genuri sparql "http://collabrx.com/testing/"))
	 (uri (frame-uri frame)))
    (setf (slotv frame #$foo) '(23))
    (setf (slotv frame #$bar) '(#$foo))
    (write-frame sparql frame)
    (print `(frame ,frame written))
    (delete-frame frame)			;clear it out
    (setf frame (intern-uri uri))
    (assert (null (slotv frame #$foo)))
    (fill-frame-sparql frame sparql)	;+++ this API should change
    (print (slotv frame #$foo))
    (assert (member "23" (slotv frame #$foo) :test #'equal))
    (print `(frame ,frame read back))
    ))

|#
