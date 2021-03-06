(in-package :swframes)

;;; +=========================================================================+
;;; | Copyright (c) 2009, 2010  Mike Travers and CollabRx, Inc                |
;;; |                                                                         |
;;; | Released under the MIT Open Source License                              |
;;; |   http://www.opensource.org/licenses/mit-license.php                    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  Mike Travers


;;; transactions

(defvar *sparul-group* nil)
;;; Experimentally verified that 500 breaks Virtuoso
(defparameter *sparul-group-limit* 250)      ;max # of groups (virtuoso has a 10000 LINE limit, this is groups)

;;; Background write thread

(defvar *background-write-queue* nil)

(eval-when (:execute)
  (background-funcall 
   #'(lambda ()
       (do () (())
	 (acl-compat.mp:process-wait "SPARQL background writer" #'(lambda () *background-write-queue*))
	 (report-and-ignore-errors
	   (print `(running a thunk))
	   (funcall (pop *background-write-queue*)))))))


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
	   (background-funcall ,procvar)
	   (funcall ,procvar)))))

;;; New better technique
(defmacro maybe-in-background-thread (async? &body body)
  (let ((procvar (gensym "PROC")))
    `(let ((,procvar #'(lambda () ,@body)))
       (if ,async?
	   (push-end ,procvar *background-write-queue*)
	   (funcall ,procvar)))))

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
	 (maybe-in-background-thread async?
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
  (if (%slotv p #$sw:specialhandling)
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
  (let ((special (%slotv slot #$sw:specialhandling)))
    (dolist (val (slotv frame slot nil))
      (if special
	  (rdfs-call write-triple-special slot frame val source)
	  (write-triple source frame slot val)))))


(defmethod$ write-triple-special ((p #$sw:slots/LispValueSlot) s o sparql)
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

(defmethod$ write-triple-special ((p #$sw:slots/TransientSlot) s o sparql)
  (declare (ignore s o sparql))
  )

(defun frame-dependents (frame)
  (collecting
   (for-frame-slots (frame slot value)
                    (when (%slotv slot #$sw:slots/dependent)
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

