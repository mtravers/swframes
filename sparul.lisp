(in-package :swframes)

;;; see /misc/sourceforge/hg/crx_rails/lib/active_rdf_patches/active_rdf_sparul.rb
;;; transactions

(defvar *sparql-group* nil)

(defmacro with-sparql-group ((endpoint) &body body)
  `(let ((*sparql-group* (list ,endpoint nil)))
      ,@body
      (when (cadr *sparql-group*)
	(do-sparql ,endpoint
	  (with-output-to-string (out)
	    (dolist (s (cadr *sparql-group*))
	      (write-string s out)
	      (terpri out)))
	  ))))

(defmethod do-grouped-sparql ((sparql sparql-endpoint) string)
  (if (and *sparql-group*
	   (eq sparql (car *sparql-group*)))
      (push-end string (cadr *sparql-group*))
      (do-sparql sparql string)))

(defmethod* write-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  ;; temp expedient +++
  (if (wb::form-is-not-printable? o)
      (warn "Can't write ~A to SPARQL, omitteing" o)
      (do-grouped-sparql sparql
	(build-insert sparql s p o))))

;;; +++ this isn't parallel with add-triple, so rethink names
(defmethod* delete-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (do-grouped-sparql sparql
    (build-delete sparql s p o)))

;;; default these (+++ bad idea probably, and needs a better variable at least)
(defmethod write-triple ((sparql null) s p o)
  (write-triple *default-frame-source* s p o))

(defmethod delete-triple ((sparql null) s p o)
  (delete-triple *default-frame-source* s p o))

(defmethod* build-insert ((sparql sparql-endpoint) s p o)
  (format nil
	  "INSERT INTO GRAPH ~A { ~A ~A ~A }"
	  (sparql-term (uri write-graph)) (sparql-term s) (sparql-term p) (sparql-term O)))

(defmethod* build-delete ((sparql sparql-endpoint) s p o)
  (let ((base (format nil "DELETE FROM GRAPH ~A { ~A ~A ~A }" (sparql-term (uri write-graph)) (sparql-term s) (sparql-term p) (sparql-term O))))
    (when (or (symbolp s) (symbolp p) (symbolp o) )
      (pushstring base (format nil " WHERE { ~A ~A ~A }" (sparql-term s) (sparql-term p) (sparql-term O))))
    base))

(defmethod write-frame ((frame frame) &optional (sparql (frame-source frame)))
  (with-sparql-group (sparql)
    (delete-triple sparql frame '?p '?o)
    (dolist (slot (%frame-slots frame))
      (aif (%slotv slot #$crx:specialhandling)
	   (rdfs-call write-slot slot frame sparql)
	  ;; normal behavior
	   (dolist (val (slotv frame slot))
	     (write-triple sparql frame slot val)))))
  ;; if we just wrote this out, then it's up to date!
  (setf (frame-loaded? frame) t)
  frame)

;;; write out a single slot
(defmethod write-slot ((frame frame) (slot frame) &optional (sparql (frame-source frame)))
  (with-sparql-group (sparql)
    (delete-triple sparql frame slot '?o)
    (aif (%slotv slot #$crx:specialhandling)
	 (rdfs-call write-slot slot frame sparql)
	 ;; normal behavior
	 (dolist (val (slotv frame slot))
	   (write-triple sparql frame slot val)))))

(rdfs-def-class #$crx:slots/specialSlot ())
(rdfs-def-class #$crx:slots/LispValueSlot (#$crx:slots/specialSlot))


(rdfs-defmethod write-slot ((slot #$crx:slots/LispValueSlot) frame sparql)
		(let ((*print-readably* t))
		  (dolist (val (slotv frame slot))
		    (write-triple sparql frame slot (prin1-to-string val)))))

;;; need to do the inverse on read! +++

(defun declare-special-slot (slot type)
  (setf (slotv slot #$rdf:type) type
	(slotv slot #$crx:specialhandling) t))

;;; special write behaviors:  don't write, serialize/deserialize lisp, list handling...

;;; Nuke frame from db
(defmethod destroy-frame ((frame frame) &optional (sparql (frame-source frame)))
  (let ((dependents
	 (collecting  
	  (for-frame-slots (frame slot value)
			   (when (%slotv slot #$crx:slots/dependent)
			     (dolist (v value)
			       (collect-new v)))))))
    (with-sparql-group (sparql)
      (delete-triple sparql frame '?p '?o)
      (delete-triple sparql '?s '?p frame))
    ;; also do locally
    (delete-frame frame)
    (dolist (d dependents)
      (destroy-frame d))
    ))

;;; Delete EVERYTHING in this graph.
;;; Times out on our Virtuoso instance, no idea why.
(defmethod* nuke-everything ((sparql sparql-endpoint))
  (assert writeable?)			;+++ OK, this should be done in a class
  (do-sparql sparql
    (build-delete sparql '?s '?p '?o)))


(defmethod* nuke-everything ((sparql sparql-endpoint))
  (assert writeable?)			;+++ OK, this should be done in a class
  (let ((all (do-sparql *default-frame-source* `(:select (?s ?p ?o) ( :from ,(intern-uri write-graph)) (?s ?p ?o)))))
    (dolist (binding all)
      (delete-triple sparql (sparql-binding-elt binding "s") (sparql-binding-elt binding "p") (sparql-binding-elt binding "o")))))

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
