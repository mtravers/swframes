(in-package :swframes)

(use-package :clos*)

;;; see /misc/sourceforge/hg/crx_rails/lib/active_rdf_patches/active_rdf_sparul.rb

#|
Theory of dirtiness (Not yet implemented):
- frames have a slot of dirty predicates
|#

(defclass* sparql-endpoint ()
  (uri
   (writeable? nil)
   (write-graph nil))
  :initable-instance-variables
  )

;;; transactions

;;; no-op for now (+++)
(defmacro with-sparul-transaction ((endpoint) &body body)
  `(progn ,@body))

(defmethod* write-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (execute sparql
	   (build-insert sparql s p o)))

(defmethod* delete-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (execute sparql
	   (build-delete sparql s p o)))

(defmethod* build-insert ((sparql sparql-endpoint) s p o)
  (format nil "INSERT INTO GRAPH ~A { ~A ~A ~A }" (sparql-term (uri write-graph)) (sparql-term s) (sparql-term p) (sparql-term O)))

(defmethod* build-delete ((sparql sparql-endpoint) s p o)
  (let ((base (format nil "DELETE FROM GRAPH ~A { ~A ~A ~A }" (sparql-term (uri write-graph)) (sparql-term s) (sparql-term p) (sparql-term O))))
    (when (or (symbolp s) (symbolp p) (symbolp o) )
      (pushstring base (format nil " WHERE { ~A ~A ~A }" (sparql-term s) (sparql-term p) (sparql-term O))))))

(defmethod* execute ((sparql sparql-endpoint) (command string))
  (knewos::run-sparql uri command :make-uri #'intern-uri))

(defmethod execute ((sparql sparql-endpoint) (command list))
  (execute sparql (generate-sparql command)))

;;; A stupid method that deletes all existing triples and writes them all anew.
(defmethod write-frame ((sparql sparql-endpoint) (frame frame))
  (with-sparul-transaction (sparql)
    (delete-triple sparql frame '?p '?o)
    (dolist (slot (%frame-slots frame))
      (dolist (val (slotv frame slot))
	(write-triple sparql frame slot val))))) 

;;; Tests

(defmethod test1 ((sparql sparql-endpoint))
  (execute sparql '(:select (?s ?p ?o) (:limit 10) (?s ?p ?o) )))

(setq e (make-instance 'sparql-endpoint
		       :uri "http://virtuoso.collabrx.com/sparql"))
(test1 e)


(setq e (make-instance 'sparql-endpoint
		       :uri "http://virtuoso.collabrx.com/sparql"))
(test1 e)

(defmethod test2 ((sparql sparql-endpoint))
  (write-triple sparql #$foo #$bar 23))

(setq ew (make-instance 'sparql-endpoint
			:uri "http://virtuoso.collabrx.com/sparql"
			:writeable? t
			:write-graph "http://collabrx.com/my_graph"))
(test2 ew)
