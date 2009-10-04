(in-package :sw)

(export '(
	  tuple-field
	  tuple-set tset-fields tset-count tset-tuple-list tset-subseq
	  tuple-dofields
	  generate-global-id
	  stream-tuple-set file-tuple-set
	  tset-stream-writer write-tset
	  tset-pickle depickle
	  ))

#|
;;; The tuple interface

A tuple is just a dictionary or equivalent.

Keys (fields) are keyword symbols or frames.  Values are anything, including other tuples.

It would be nice to be able to have types and other metadata on fields. Currently we can do this with frames.

Tuples can be implemented as alists, hashtables, or other objects.  Right now they are alists, but the idea is to use generic functions so other implementations are possible.

Tuples may be found in isolation or as part of tuple-sets or tuple-streams.

;;; Create an empty tuple
(make-tuple &optional class)

;;; Field access.
;;; Setting a field to NIL is equivalent to removing it. (nothing depends on this yet)
(tuple-field tuple field)
(setf (tuple-field tuple field) value)
(tuple-field-accessor field)
(tuple-fields tuple)
(dofields (key value) tuple
          body)

Tuple sources/sinks/sets
A tupleset is any collection of roughly-similar tuples.

API
- none of these are implemented yet
- some operations might not be implemented on all classes

(tset-add-tuple tset tuple)
(tset-remove-tuple tset tuple)
(tset-make-tuple tset)
(tset-remove-tuples tset selector)
(tset-clear tset)
(tset-count tset)

(tset-map-tuples tset procedure)
(tset-for-tuples (tuple) body)
(tset-contains-tuple tset tuple)
(tset-contains-a-tuple tset selector)
   Selector is a predicate on tuples
(tset-fields tset)
   Known common fields; individual tuples may differ
(tset-add-field tset field)
(tset-remove-field tset field)
(tset-tuple-iterator tset)
   Returns an iterator over tuples
(tset-tuple-list tset)
   Returns set as a list
(tset-filter tset predicate)
(tset-sort tset sort-function)
(tset-subseq tset start length)
   The above three return a new tset (+++ may not be implementable?)
(tset-add-listener tset function)
(tset-remove-listener tset function)
   TBD

Um, think this out:
tset-chunky-souce:  something that gives you a chunk of data back, hopefully start and size params
tset-source-stream: something capable of handing out tuples one at a time, in order
tset-cache:         something that can sit inbetween the above...


;;; don't know about these, for the most part I've sluffed around them
(more? iterator)
(next iterator)
(next* iterator)  ;;; next or nil, more lispy API

(new-tuple set)
(add-tuple set tuple)
(map-tuples set proc)
(tuple-count set)  ;;; may be expensive or undefined
(tuple-list set)  ;;; all tuples as list, may be large
;;; Should give the union over all tuples?


A tuplesource is a tupleset that can produce its tuples one at at time

;;; for streaming/iterating
(next-tuple stream)
(more-tuples? stream)
(reset-stream set)

(make-tuple set)
|#

(defun make-tuple (&rest fields)
  ;; avoid (setf (tuple-field ...)) because it assumes a non-nil tuple
    (do ((tuple (list (cons :tuple-id (generate-global-id))))
         (rest fields (cddr rest)))
        ((null rest)
         tuple)
      (setf (tuple-field tuple (car rest)) (cadr rest))))

(defvar *last-tuple-id* -1)
(defun generate-global-id ()
  (incf *last-tuple-id*))

(defun coerce-field (field)
  (cond ((keywordp field)
         field)
        ((stringp field)
	 ;; Beware, the BioLisp keywordize has different behavior
	 (keywordize (utils::remove-all-whitespace (utils:fast-string field))))
        (t (error "Can't coerce ~A to field name" field))))

(defgeneric tuple-field (tuple field-name))
(defgeneric tuple-fields (tuple))
(defgeneric tuple-remove-field (tuple field-name))
(defgeneric (setf tuple-field) (tuple field-name value))

;;; using eql now, so field names should be keywords (and URIs eventually)
(defmethod tuple-field ((tuple list) field)
  (cdr (assoc (coerce-field field) tuple :test #'eql)))

(defgeneric tuple-fields (tuple))
(defgeneric tuple-field-count (tuple))

(defmethod tuple-fields ((tuple list))
  (mapcar #'car tuple))

(defmethod tuple-field-count ((tuple list))
  (length tuple))

(defun tuple-field-accessor (field)
  #'(lambda (tuple) (tuple-field tuple field)))

;;; could be made more efficient
(defmacro tuple-dofields ((key val) tuple &body body)
  `(dolist (,key (tuple-fields ,tuple))
     (let ((,val (tuple-field ,tuple ,key)))
       ,@body)))

(defmethod (setf tuple-field) (new-value (tuple list) field)
  (setq field (coerce-field field))
  (if (null new-value)
      (tuple-remove-field tuple field)
      (aif (assoc field tuple)
	   (rplacd it new-value)
	   (rplacd tuple
		   (cons (cons field new-value)
			 (cdr tuple))))))

(defgeneric tuple-remove-field (tuple field))

(defmethod tuple-remove-field ((tuple list) field)
  (deletef field tuple :key #'car))

(defmacro with-tuple-fields (var-forms tuple &body body)
  (once-only (tuple)
    `(let ,(mapcar #'(lambda (vform)
                       `(,(car vform)
                          (tuple-field ,tuple ,(cadr vform))))
                   var-forms)
       ,@body)))

(defmacro set-tuple-fields (tuple &rest field-values)
  (once-only (tuple)
    `(progn
       ,@(collecting
          (do ((rest field-values (cddr rest)))
              ((null rest))
            (collect `(setf (tuple-field ,tuple ,(car rest)) ,(cadr rest))))))))


;;; tuples are equal if all fields are EQUAL, other than :tuple-id
(defun tuples-equal? (t1 t2)
  (and (= (tuple-field-count t1)
          (tuple-field-count t2))
       (progn
         (tuple-dofields (key val) t1
                   (unless (eq key :tuple-id)
                     (unless (equal val
                                    (tuple-field t2 key))
                       (return-from tuples-equal? nil))))
         t)))

;;; Iterators

;;; abstract
(defclass base-iterator () ())

;;; better API
(defmethod next* ((iterator base-iterator))
  (if (more? iterator)
      (next iterator)
      nil))

;;; Sets

(defclass* tuple-set ()
  ())

;;; abstract (no abstract class in Lisp? )
(defclass* base-tuple-set (tuple-set)
  ())

(defmethod tset-indexable? ((tset tuple-set))
  nil)

(defmethod %new-tuple ((tuple-set base-tuple-set))
  (make-tuple))

(defmethod tset-make-tuple ((tuple-set base-tuple-set))
  (let ((tuple (%new-tuple tuple-set)))
    (tset-add-tuple tuple-set tuple)
    tuple))

;;; default kludge
(defmethod tset-iterator ((tuple-set base-tuple-set))
  (tset-reset-position tuple-set)
  tuple-set)

(defmethod tset-map-tuples ((tuple-set base-tuple-set) proc)
  (let ((iterator (tset-iterator tuple-set)))
    (do ((tuple (tset-iterator-next iterator)
                (tset-iterator-next iterator)))
        ((null tuple))
      (funcall proc tuple))))

(defmethod tset-map-tuples ((tset list) proc)
  (dolist (tup tset)
    (funcall proc tup)))

(defmacro tset-do-tuples ((var tset) &body body)
  `(tset-map-tuples ,tset #'(lambda (,var) ,@body)))

(defmethod tset-tuple-list ((tuple-set base-tuple-set))
  (collecting
    (tset-map-tuples tuple-set #'collect)))

(defmethod tset-copy ((from tuple-set) &optional (to (make-instance 'in-memory-tuple-set)))
  (tset-do-tuples (tup from)
                  (tset-add-tuple to tup))
  to)

;;; +++ overridden in filter.lisp
(defmethod tset-filter ((from tuple-set) predicate)
  (let ((to (make-instance 'in-memory-tuple-set)))
    (tset-do-tuples (tup from)
                    (when (funcall predicate tup)
                      (tset-add-tuple to tup)))
    to))

;;; collect all field values as a list
(defmethod* tset-extract-field ((tset tuple-set) field)
  (collecting
   (tset-do-tuples (tup tset)
                   (collect (tuple-field tup field)))))

;;; ----
;;; ----------------------------------------------------------------

;;; A plain list can act as a tuple-set, sometimes.  Except then an empty list can't have object identity, so we have to kludge a bit.

(defmethod make-tuple-set ((class (eql 'list)))
  (list nil))

(defmethod tset-count ((ts list))
  (length ts))                          ;adjust for empty list +++

(defmethod tset-iterator ((ts list))
  (make-instance 'list-iterator :list ts))

;;; Push-end to preserve object identity
(defmethod tset-add-tuple ((ts list) tuple)
  (if (null (car ts))
      (rplaca ts tuple)
      (push-end tuple ts)))

;;; slightly dangerous because list can be modified
(defmethod tset-tuple-list ((ts list))
  ts)

(defmethod tset-fields ((ts list))
  ;; just use the first one as a representative sample.
  (tuple-fields (car ts)))

;;; ----------------------------------------------------------------

;;; A list-based implementation of a tuple set (a plain list can be used as a set too)

(defclass* list-tuple-set (base-tuple-set)
  ((tuples nil))
  :initable-instance-variables)

(defmethod* tset-count ((tuple-set list-tuple-set))
  (length tuples))

(defmethod* tset-iterator ((tuple-set list-tuple-set))
  (make-instance 'list-iterator :list tuples))

(defmethod* tset-add-tuple ((tuple-set list-tuple-set) tuple)
  (push tuple tuples))

;;; slightly dangerous because list can be modified
(defmethod* tset-tuple-list ((tuple-set list-tuple-set))
  tuples)

(defmethod* tset-fields ((ts list-tuple-set))
  ;; just use the first one as a representative sample.
  (tuple-fields (car tuples)))

;;; Iterator for lists

(defclass* list-iterator (base-iterator)
  (list)
  :initable-instance-variables)

(defmethod* more? ((iterator list-iterator))
  list)

(defmethod* next ((iterator list-iterator))
  (pop list))

;;; ----------------------------------------------------------------




;;; -------

(defvar *tuple-set*)

(defmacro making-tuple-set (&body body)
  `(let ((*tuple-set* (make-instance 'list-tuple-set)))
     (macrolet ((make-tuple ()
                  `(new-tuple *tuple-set*)))
       ,@body)
     *tuple-set*))




