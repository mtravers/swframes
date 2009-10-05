(in-package :swframes)

;;; see /misc/sourceforge/hg/crx_rails/lib/active_rdf_patches/active_rdf_sparul.rb
;;; transactions

(defvar *sparul-group* nil)
(defvar *sparul-group-limit* 1000)      ;max # of groups (virtuoso has a 10000 LINE limit, this is groups)

;;; async is NOT WORKING PROPERLY yet, so don't use it!
(defmacro with-sparul-group ((endpoint &key async?) &body body)
  "Causes all writes to endpoint within the dynamic scope to be delayed until the form is exited (or in other words, it saves all SPARQL insert/delete commands and does them at the end)."
  `(let ((prior-group *sparul-group*)   ;make sure we only do it after all groups unwound
	 (*sparul-group* (or *sparul-group* (list ,endpoint nil)))
	 retval)
     (unless (equal (car *sparul-group*) ,endpoint)
       ;; +++ was error, temp disabled
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
  (aif (%slotv p #$crx:specialhandling)
       (rdfs-call write-triple-special p s o sparql)
       ;; normal
       (%write-triple sparql s p o)))

(defun %write-triple (sparql s p o)
  (do-grouped-sparul sparql
    (build-insert sparql s p o)))

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

;;; +++ these should be done in same manner as :selects
(defmethod* build-insert ((sparql sparql-endpoint) s p o)
  (format nil
          "INSERT INTO GRAPH ~A { ~A ~A ~A }"
          (sparql-term (make-frame write-graph)) (sparql-term s) (sparql-term p) (sparql-term O)))

(defmethod* build-delete ((sparql sparql-endpoint) s p o)
  (let ((base (format nil "DELETE FROM GRAPH ~A { ~A ~A ~A }" (sparql-term (make-frame write-graph)) (sparql-term s) (sparql-term p) (sparql-term O))))
    (when (or (symbolp s) (symbolp p) (symbolp o) )
      (push-string base (format nil " WHERE { ~A ~A ~A }" (sparql-term s) (sparql-term p) (sparql-term O))))
    base))

(defmethod write-frame ((frame frame) &key (source (frame-source frame)) (async? nil) (no-delete? nil))
  #.(doc
     "Write FRAME to SOURCE"
     "ASYNC? causes the write to be done in a separate thread"
     "NO-DELETE? causes the previous contents in the database to not be retained.")
  (let ((dependents (frame-dependents frame)))
    (with-sparul-group (source :async? async?)
      (unless no-delete?
        (delete-triple source frame '?p '?o))
      (dolist (slot (%frame-slots frame))
	(dolist (val (slotv frame slot))
	  (write-triple source frame slot val)))
      ;; write out dependents
      (dolist (d dependents)
        (write-frame d))
      ;; if we just wrote this out, then it's up to date!
      (set-frame-loaded? frame t source))
    frame))

;;; write out a single slot
(defmethod write-slot ((frame frame) (slot frame) &optional (sparql (frame-source frame)))
  (with-sparul-group (sparql)
    (delete-triple sparql frame slot '?o)
    (dolist (val (slotv frame slot))
      (write-triple sparql frame slot val))))

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

(rdfs-def-class #$crx:slots/TransientSlot (#$crx:slots/specialSlot))
(rdfs-defmethod write-triple-special ((p #$crx:slots/TransientSlot) s o sparql)
		)

;;; Sometimes these unserializable slots get serialized, so ignore them
(rdfs-defmethod deserialize-value ((p #$crx:slots/TransientSlot) value)
		nil)


(defun frame-dependents (frame)
  (collecting
   (for-frame-slots (frame slot value)
                    (when (%slotv slot #$crx:slots/dependent)
                      (dolist (v value)
			(assert (frame-p v) nil "Non-frame value ~A in slot ~A of ~A" v slot frame)
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

;;; OOPS -- if you accidently destroy a frame but have a df of it somewhere, this can recreate it!
(defun recreate-frame (frame forward backward)
  (with-sparul-group (nil)
    (dolist (f forward)
      (dolist (v (cadr f))
        (add-triple frame (car f) v :to-db t)))
    (dolist (i backward)
      (dolist (v (cadr i))
        (add-triple v (car i) frame :to-db t)))))

;;; Delete EVERYTHING in this graph.
;;; Times out on our Virtuoso instance, no idea why.
(defmethod* nuke-everything ((sparql sparql-endpoint))
  (assert writeable?)                   ;+++ OK, this should be done in a class
  (do-sparql sparql
    (build-delete sparql '?s '?p '?o)))

;;; alternate method --
(defmethod* nuke-everything2 ((sparql sparql-endpoint))
  (assert writeable?)                   ;+++ OK, this should be done in a class
  (let ((all (do-sparql *default-frame-source* `(:select (?s ?p ?o) ( :from ,(intern-uri write-1graph)) (?s ?p ?o)))))
    (dolist (binding all)
      (delete-triple sparql (sparql-binding-elt binding "s") (sparql-binding-elt binding "p") (sparql-binding-elt binding "o")))))

