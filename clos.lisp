(in-package :sw)

(export '(defmethod$ make-instance$))

#|
Theory:

- Maps RDFS classes to CLOS classes
- Lisp class names are the URI keywordified. 
- $-versions of standard Lisp functions work on URIs


Todo:

- rdfs-def-class should be extended (rename defclass$ for consistency)
- metaclass?
- SPARQL endpoint schema snarfer.
- maintain relation between URI/frame/symbol/class in organized way

Notes:



|#

(defclass rdfs-class (frame)
  ())

(defmethod initialize-instance :after ((f rdfs-class) &rest ignore)
  (with-slots (uri) f
    (unless uri
      (setf uri (gensym-instance-uri (class-frame (class-of f)))))))

(defun class-frame (class)
  (get (class-name class) :frame))

(defun frame-as-symbol (frame)
  (keywordize (frame-uri frame)))

(defun defclass-form (frame supertypes)
  `(defclass ,(frame-as-symbol frame)
       ,(append (mapcar #'frame-as-symbol supertypes)
		(list 'rdfs-class))
     ()))

;;; Convert a RDFS class frame into a CLOS class
(defun rdfs-clos-class (frame &key (force? t) (error? t))
  (let* ((sym (frame-as-symbol frame))
	 (class (find-class sym nil)))
    (cond ((and class 
		  ;; +++ I'm not sure how to do this in an implementation-indepent way. 
		  (not (typep class 'ccl:forward-referenced-class)))
	   class)
	  (force?
	   (eval (defclass-form frame (slotv frame #$rdfs:subClassOf)))
	   (find-class sym t))
	  (error?
	   (error "Can't turn ~A into class" frame))
	  (t nil))))

(defun ensure-clos-class (frame)
  (rdfs-clos-class frame :force? t :error? t))

;;; Set class of frame based on its RDF type
(defun classify-frame (f &optional error?)
  (when (eq 'frame (type-of f))
    (let ((rclasses (collapse-class-list (slotv f #$rdf:type))))
      (cond ((= 1 (length rclasses))
	     (set-frame-class f (car rclasses) error?))
	    ((null rclasses)
	     (if error? (error "Can't set class, no type for ~A" f)))
	    (t
	     ;; Multiple types, so heuristicate -- pick one that already is a CLOS class
	     (dolist (c rclasses)
	       (when (rdfs-clos-class c :force? nil :error? nil)
		 (set-frame-class f c error?)
		 (return c))))))))

(defun set-frame-class (frame rdf-class &optional error?)
  (let ((cclass (rdfs-clos-class rdf-class :force? t :error? error?)))
    (if cclass 
	(change-class frame cclass)
	(if error?
	    (error "Can't set class for ~A, no class found" frame)))
    frame))

;;; Make instance

;;; based on gensym-instance-frame, but that's not modularized right
(defun gensym-instance-uri (class &key start (fast? t) (source *default-frame-source*) base)
  (if (eq (frame-source class) *code-source*)
      (setf (frame-source class) source)
      ;; Here we might want to do an initial write of frame to db
      )
  (unless base (setq base (frame-uri class)))
  (acl-compat.mp:with-process-lock (gensym-lock)	
    (unless (and fast?
		 (msv class #$crx:last_used_id))
      (fill-frame class :force? t :inverse? nil))
    (let* ((last (or start (ssv class #$crx:last_used_id)))
	   (next (if last
		     (1+ (coerce-number last))
		     0))
	   (uri (string+ base "/"
			 (if fast? (string+ (frame-label (unique-session)) "/") "")
			 (fast-string next))))
      (if (and (not fast?) (uri-used? source uri))
	  (gensym-instance-frame class :start next :fast? fast?)
	  (progn
	    (add-triple class #$crx:last_used_id next :to-db (and (not fast?) *default-frame-source*) :remove-old t)
	    uri)))))


;;; handle full range of defmethod hair +++
(defmacro defmethod$ (name args &body body)
  (let* ((&pos (position #\& args :key #'(lambda (arg) (and (symbolp arg) (char (symbol-name arg) 0)))))
	 (qualified-args (subseq args 0 &pos))
	 (rest-args (and &pos (subseq args &pos)))
	 (trans-args (mapcar #'(lambda (arg)
				 (if (listp arg)
				     (list (car arg) (class-name (sw::rdfs-clos-class (cadr arg) :force? t)))
				     arg))
			     qualified-args)))
    `(defmethod ,name ,(nconc trans-args rest-args) ,@body)))

(defun make-instance$ (class &rest args)
  (apply #'rdfs-make-instance class args))
