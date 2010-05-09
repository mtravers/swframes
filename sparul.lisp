(in-package :swframes)

;;; transactions

(defvar *sparul-group* nil)
;;; Experimentally verified that 500 breaks Virtuoso
(defparameter *sparul-group-limit* 250)      ;max # of groups (virtuoso has a 10000 LINE limit, this is groups)

;;; error handling +++ also, move these to utils +++
(defmacro in-background-thread (&body body)
  `(background-funcall #'(lambda () ,@body)))

(defun background-funcall (thunk)
  (#+ACL mp:process-run-function
   #-ACL acl-compat.mp:process-run-function
   (string (gensym "THREAD"))
   thunk))

(defmacro maybe-asynchronously (async? &body body)
  (let ((procvar (gensym "PROC")))
    `(let ((,procvar #'(lambda () ,@body)))
       (if ,async?
	   (funcall ,procvar)
	   (background-funcall ,procvar)))))

(defmethod do-write-group ((endpoint sparql-endpoint) async? proc)
  (let ((prior-group *sparul-group*)   ;make sure we only do it after all groups unwound
	(*sparul-group* (or *sparul-group* (list endpoint nil)))
	retval)
    (unless (clos*::oequal (car *sparul-group*) endpoint)
      (error "Bad nested SPARUL groups: ~A ~A" (car *sparul-group*) endpoint))
    (setf retval (funcall proc))
    (let ((clauses (cadr *sparul-group*))) ;make sure this gets captured
       (when (and clauses
		  (not prior-group))
	 (maybe-asynchronously async?
	     (do-sparql endpoint
		    (with-output-to-string (out)
		      (dolist (s clauses)
			(write-string s out)
			(terpri out)))))))
    retval))

;;; Do a possibly-delayed sparul operation.  Queues stuff for later, if the limit is reached it gets unleashed then and there.
(defmethod do-grouped-sparul ((sparql sparql-endpoint) (list list))
  (do-grouped-sparul sparql (generate-sparql sparql list)))

(defmethod do-grouped-sparul ((sparql sparql-endpoint) (string string))
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

(defmethod* write-triple ((sparql sparql-endpoint) s p o &key write-graph)
  (assert writeable?)
  (if (%slotv p #$crx:specialhandling)
      (rdfs-call write-triple-special p s o sparql) ;+++ deal with write-graph here
      ;; normal
      (%write-triple sparql s p o :write-graph (or write-graph (slot-value sparql 'write-graph)))))

(defmethod %write-triple ((sparql sparql-endpoint) s p o &key write-graph)
  (let ((write-graph (or write-graph (slot-value sparql 'write-graph))))
    (do-grouped-sparul sparql
      `(:insert (,s ,p ,o) (:into ,write-graph)))
      ))

(defmethod %write-triple ((sparql null) s p o &key write-graph)
  (%write-triple *default-frame-source* s p o :write-graph write-graph)
  )

;;; +++ this isn't parallel with add-triple, so rethink names
(defmethod delete-triple ((sparql sparql-endpoint) s p o &key write-graph)
  (let ((write-graph (or write-graph (slot-value sparql 'write-graph))))
    (do-grouped-sparul sparql `(:delete (,s ,p ,o) (:from ,write-graph)))
    ))

;;; default these
(defmethod write-triple ((sparql null) s p o &key write-graph)
  (write-triple *default-frame-source* s p o :write-graph write-graph))

(defmethod delete-triple ((sparql null) s p o &key write-graph)
  (delete-triple *default-frame-source* s p o :write-graph write-graph))

(defgeneric write-frame (frame &key source async? no-delete?)
  (:documentation
   #.(doc
      "Write FRAME to SOURCE"
      "ASYNC? causes the write to be done in a separate thread"
      "NO-DELETE? causes the previous contents in the database to be retained (not recommended).")))

(defmethod write-frame ((frame frame) &key source (async? nil) (no-delete? nil) )
  (unless source
    (setf source (or (frame-source frame) *default-frame-source*)))
  (setf (frame-source frame) source)
  (let ((dependents (frame-dependents frame)))
    (with-write-group (source :async? async?)
      (unless no-delete?
        (delete-triple source frame '?p '?o))
      (dolist (slot (%frame-slots frame))
	(write-slot frame slot :source source :no-delete? t)) 
      ;; write out dependents
      (dolist (d dependents)
        (write-frame d :source source :no-delete? no-delete?))
      ;; if we just wrote this out, then it's up to date!
      (set-frame-loaded? frame t source))
    frame))

(defgeneric write-slot (frame slot &key source no-delete? value)
  (:documentation
   #.(doc "Write a single slot of a single frame to SOURCE."
	  "VALUE if provided is a single value to set the slot to.")))

;;; Value is a singleton.
(defmethod write-slot (frame slot &key (source (frame-source frame)) no-delete? (value nil value-provided?))
  (when value-provided?
    (setf (ssv frame slot) value))
  (unless no-delete?
    (delete-triple source frame slot '?o))
  ;;; CCC -- write-triple should be a method that dispatches on slot type, maybe on source
  (let ((special (%slotv slot #$crx:specialhandling)))
    (dolist (val (slotv frame slot nil))
      (if special
	  (rdfs-call write-triple-special slot frame val source)
	  (write-triple source frame slot val)))))

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

(rdfs-defmethod write-triple-special ((p #$crx:slots/LispValueSlot) s o sparql)
		(with-standard-io-syntax ;aka print-readably
		  (let ((oo (typecase o
			      (fixnum o)
			      (otherwise (prin1-to-string o)))))
		    (handler-case
			(%write-triple sparql s p oo)
		      (print-not-readable (e)
			(declare (ignore e))
			(error "Can't save nonreadable object ~A in ~A / ~A" o s p)
			)))))

(rdfs-defmethod write-triple-special ((p #$crx:slots/TransientSlot) s o sparql)
		(declare (ignore s o sparql))
		)

;;; Sometimes these unserializable slots get serialized, so ignore them
(rdfs-defmethod deserialize-value ((p #$crx:slots/TransientSlot) value)
		(declare (ignore value))
		nil)

(defun frame-dependents (frame)
  (collecting
   (for-frame-slots (frame slot value)
                    (when (%slotv slot #$crx:slots/dependent)
                      (dolist (v value)
			(when (frame-p v)
			  (collect-new v)))))))

(defgeneric destroy-frame (frame &optional source)
  (:documentation
   #.(doc
      "Deletes frame from database, then calls DELETE-FRAME to remove from memory.")))

;;; Nuke frame from db
(defmethod destroy-frame ((frame frame) &optional (sparql (or (frame-source frame) *default-frame-source*)))
  (let ((dependents (frame-dependents frame)))
    (with-write-group (sparql)
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

