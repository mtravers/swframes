(in-package :swframes)

;;; see /misc/sourceforge/hg/crx_rails/lib/active_rdf_patches/active_rdf_sparul.rb
;;; transactions

(defvar *sparul-group* nil)
(defvar *sparul-group-limit* 1000)      ;max # of groups (virtuoso has a 10000 LINE limit, this is groups)

;;; error handling +++
(defmacro in-background-thread (&body body)
  `(#+ACL 
    mp:process-run-function
    #-ACL
    acl-compat.mp:process-run-function
    (string (gensym "THREAD"))
    #'(lambda () ,@body)))

;;; async is NOT WORKING PROPERLY yet, so don't use it! +++
(defmacro with-sparul-group ((endpoint &key async?) &body body)
  "Causes all writes to endpoint within the dynamic scope to be delayed until the form is exited (or in other words, it saves all SPARQL insert/delete commands and does them at the end)."
  `(let ((prior-group *sparul-group*)   ;make sure we only do it after all groups unwound
	 (*sparul-group* (or *sparul-group* (list ,endpoint nil)))
	 retval)
     (unless (equal (car *sparul-group*) ,endpoint)
       (error "Bad nested SPARUL groups: ~A ~A" (car *sparul-group*) ,endpoint))
     (setf retval (progn ,@body))
     (let ((clauses (cadr *sparul-group*))) ;make sure this gets captured
       (when (and clauses
		  (not prior-group))
	 (flet ((do-it ()
;		  (print `(clauses ,clauses))
		  (do-sparql ,endpoint
		    (with-output-to-string (out)
		      (dolist (s clauses)
			(write-string s out)
			(terpri out))))))
	   (if ,async?
	       (in-background-thread (do-it))
	       (do-it)))))
     retval))

(defmethod do-grouped-sparul ((sparql sparql-endpoint) string)
  (if (and *sparul-group*
           (eq sparql (car *sparul-group*)))
      (progn
        (push-end string (cadr *sparul-group*))
        (when (> (length (cadr *sparul-group*)) *sparul-group-limit*)
          (do-sparql (car *sparul-group*)
            (with-output-to-string (out)
              (dolist (s (cadr *sparul-group*))
                (write-string s out)
                (terpri out))))
          (setf (cadr *sparul-group*) nil)))
      ;; otherwise do immediately
      (do-sparql sparql string)))

(defmethod* write-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (if (%slotv p #$crx:specialhandling)
      (rdfs-call write-triple-special p s o sparql)
      ;; normal
      (%write-triple sparql s p o)))

(defmethod* %write-triple ((sparql sparql-endpoint) s p o)
  (do-grouped-sparul sparql
    (generate-sparql sparql `(:insert (,s ,p ,o) (:into ,write-graph)))
    ))

;;; +++ this isn't parallel with add-triple, so rethink names
(defmethod* delete-triple ((sparql sparql-endpoint) s p o)
  (assert writeable?)
  (do-grouped-sparul sparql
    (generate-sparql sparql `(:delete (,s ,p ,o) (:from ,write-graph)))
    ))

;;; default these
(defmethod write-triple ((sparql null) s p o)
  (write-triple *default-frame-source* s p o))

(defmethod delete-triple ((sparql null) s p o)
  (delete-triple *default-frame-source* s p o))

(defgeneric write-frame (frame &key source async? no-delete?)
  (:documentation
   #.(doc
      "Write FRAME to SOURCE"
      "ASYNC? causes the write to be done in a separate thread"
      "NO-DELETE? causes the previous contents in the database to be retained (not recommended).")))

(defmethod write-frame ((frame frame) &key (source (frame-source frame)) (async? nil) (no-delete? nil) )
  (let ((dependents (frame-dependents frame)))
    (with-sparul-group (source :async? async?)
      (unless no-delete?
        (delete-triple source frame '?p '?o))
      (dolist (slot (%frame-slots frame))
	(write-slot frame slot :source source :no-delete? t)) 
      ;; write out dependents
      (dolist (d dependents)
        (write-frame d))
      ;; if we just wrote this out, then it's up to date!
      (set-frame-loaded? frame t source))
    frame))

(defgeneric write-slot (frame slot &key source no-delete? value)
  (:documentation
   #.(doc "Write a single slot of a single frame to SOURCE."
	  "VALUE if provided is a single value to set the slot to.")))

(defmethod write-slot (frame slot &key (source (frame-source frame)) no-delete? (value nil value-provided?))
  (when value-provided?
    (setf (ssv frame slot) value))
  (unless no-delete?
    (delete-triple source frame slot '?o))
  (if (%slotv slot #$crx:specialhandling)
      (rdfs-call write-slot-special slot frame source)
      ;; normal
      (dolist (val (slotv frame slot))
	(write-triple source frame slot val))))

(rdfs-def-class #$crx:slots/specialSlot ())

;;; special write behaviors:  don't write, serialize/deserialize lisp, list handling...

(defun declare-special-slot (slot type)
  #.(doc 
     "Declares SLOT to have special behavior defined by TYPE.  Current TYPEs are:"
     "#$crx:slots/LispValueSlot:"
     "   Slots of this class can hold any printable Lisp object."
     "#$crx:slots/TransientSlot:"
     "  Slots of this class never write their values to the database.")
  (setf (ssv slot #$rdf:type) type
        (ssv slot #$crx:specialhandling) t))

;;; debugging only
(defun undeclare-special-slot (slot)
  (setf (slotv slot #$rdf:type) nil
        (slotv slot #$crx:specialhandling) nil)  )

(rdfs-def-class #$crx:slots/LispValueSlot (#$crx:slots/specialSlot))

;;; this probably shouldn't ever get called.
(rdfs-defmethod write-triple-special ((p #$crx:slots/LispValueSlot) s o sparql)
		(let ((*print-readably* t)
		      (oo (typecase o
			    (fixnum o)
			    (otherwise (prin1-to-string o)))))
		  (handler-case
		      (%write-triple sparql s p oo)
		    (print-not-readable (e)
		      (declare (ignore e))
		      (error "Can't save nonreadable object ~A in ~A / ~A" o s p)
		      ))))

(rdfs-defmethod write-slot-special ((p #$crx:slots/LispValueSlot) s sparql)
		(let* ((*print-readably* t)
		       (o (%slotv s p))
		       (oo (typecase o
			     (fixnum o)
			     (otherwise (prin1-to-string o)))))
		  (handler-case
		      (%write-triple sparql s p oo)
		    (print-not-readable (e)
		      (declare (ignore e))
		      (error "Can't save nonreadable object ~A in ~A / ~A" o s p)
		      ))))

(rdfs-def-class #$crx:slots/TransientSlot (#$crx:slots/specialSlot))
(rdfs-defmethod write-triple-special ((p #$crx:slots/TransientSlot) s o sparql)
		(declare (ignore s o sparql))
		)

(rdfs-defmethod write-slot-special ((p #$crx:slots/TransientSlot) s sparql)
		(declare (ignore s sparql))
		)

;;; Sometimes these unserializable slots get serialized, so ignore them
(rdfs-defmethod deserialize-slot ((p #$crx:slots/TransientSlot) frame value)
		(declare (ignore frame value))
		nil)


(defun frame-dependents (frame)
  (collecting
   (for-frame-slots (frame slot value)
                    (when (%slotv slot #$crx:slots/dependent)
                      (dolist (v value)
			(assert (frame-p v) nil "Non-frame value ~A in slot ~A of ~A" v slot frame)
                        (collect-new v))))))

(defgeneric destroy-frame (frame &optional source)
  (:documentation
   #.(doc
      "Deletes frame from database, then calls DELETE-FRAME to remove from memory.")))

;;; Nuke frame from db
(defmethod destroy-frame ((frame frame) &optional (sparql (frame-source frame)))
  (let ((dependents (frame-dependents frame)))
    (with-sparul-group (sparql)
      (delete-triple sparql frame '?p '?o)
      (delete-triple sparql '?s '?p frame))
    ;; also do locally
    (delete-frame frame)
    (dolist (d dependents)		;+++ this should probably done at delete-frame level.
      (destroy-frame d))
    ))

;;; Delete EVERYTHING in this graph.
;;; Times out on our Virtuoso instance.
(defmethod* nuke-everything ((sparql sparql-endpoint))
  (assert writeable?) 
  (do-sparql sparql
    `(:delete (?s ?p ?o) (:from ,write-graph))))

;;; alternate method --
(defmethod* nuke-everything2 ((sparql sparql-endpoint))
  (assert writeable?)
  (let ((all (do-sparql *default-frame-source* `(:select (?s ?p ?o) ( :from ,(intern-uri write-graph)) (?s ?p ?o)))))
    (dolist (binding all)
      (delete-triple sparql (sparql-binding-elt binding "s") (sparql-binding-elt binding "p") (sparql-binding-elt binding "o")))))

