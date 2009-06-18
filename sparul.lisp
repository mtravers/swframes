(in-package :swframes)

;;; see /misc/sourceforge/hg/crx_rails/lib/active_rdf_patches/active_rdf_sparul.rb
;;; transactions

(defvar *sparul-group* nil)

;;; async is NOT WORKING PROPERLY yet, so don't use it!
(defmacro with-sparul-group ((endpoint &key async?) &body body)
  `(let ((prior-group *sparul-group*)	;make sure we only do it after all groups unwound
	 (*sparul-group* (or *sparul-group* (list ,endpoint nil))))
     (unless (eq (car *sparul-group*) ,endpoint)
       (error "Bad nested SPARUL groups"))
     ,@body
     (when (and (cadr *sparul-group*)
		(not prior-group))
       (flet ((do-it ()
		(do-sparql ,endpoint
		  (with-output-to-string (out)
		    (dolist (s (cadr *sparul-group*))
		      (write-string s out)
		      (terpri out))))))
	 (if ,async?
	     (in-background-thread (do-it))
	     (do-it))))))

(defmethod do-grouped-sparul ((sparql sparql-endpoint) string)
  (if (and *sparul-group*
	   (eq sparql (car *sparul-group*)))
      (push-end string (cadr *sparul-group*))
      ;; otherwise do immediately
      (do-sparql sparql string)))

(defmethod* write-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  ;; temp expedient +++
  (if (wb::form-is-not-printable? o)
      (warn "Can't write ~A to SPARQL, omitting" o)
      (do-grouped-sparul sparql
	(build-insert sparql s p o))))

;;; +++ this isn't parallel with add-triple, so rethink names
(defmethod* delete-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (do-grouped-sparul sparql
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

(defmethod write-frame ((frame frame) &key (sparql (frame-source frame)) (async? nil))
  (let ((dependents (frame-dependents frame)))
    (with-sparul-group (sparql :async? async?)
      (delete-triple sparql frame '?p '?o)
      (dolist (slot (%frame-slots frame))
	(aif (%slotv slot #$crx:specialhandling)
	     (rdfs-call write-slot slot frame sparql)
	     ;; normal behavior
	     (dolist (val (slotv frame slot))
	       (write-triple sparql frame slot val))))
      ;; write out dependents
      (dolist (d dependents)
	(write-frame d))
    ;; if we just wrote this out, then it's up to date!
      (setf (frame-loaded? frame) t))
    frame))

;;; write out a single slot
(defmethod write-slot ((frame frame) (slot frame) &optional (sparql (frame-source frame)))
  (with-sparul-group (sparql)
    (delete-triple sparql frame slot '?o)
    (if (%slotv slot #$crx:specialhandling)
	(rdfs-call write-slot slot frame sparql)
	;; normal behavior
	(dolist (val (slotv frame slot))
	  (write-triple sparql frame slot val)))))

(rdfs-def-class #$crx:slots/specialSlot ())
(rdfs-def-class #$crx:slots/LispValueSlot (#$crx:slots/specialSlot))

(rdfs-defmethod write-slot ((slot #$crx:slots/LispValueSlot) frame sparql)
		(handler-case 
		    (let ((*print-readably* t))
		      (dolist (val (slotv frame slot))
			(write-triple sparql frame slot (prin1-to-string val))))
		  (print-not-readable (e)
		    (warn "Can't save nonreadable object in ~A/~A" frame slot) 
		    )))

;;; need to do the inverse on read! See deserialize-value (+++ make more parallel)

(defun declare-special-slot (slot type)
  (setf (slotv slot #$rdf:type) type
	(slotv slot #$crx:specialhandling) t))

;;; special write behaviors:  don't write, serialize/deserialize lisp, list handling...

(defun frame-dependents (frame)
  (collecting  
   (for-frame-slots (frame slot value)
		    (when (%slotv slot #$crx:slots/dependent)
		      (dolist (v value)
			(collect-new v))))))


;;; Nuke frame from db
(defmethod destroy-frame ((frame frame) &optional (sparql (frame-source frame)))
  (let ((dependents (frame-dependents frame)))
    (with-sparul-group (sparql)
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

