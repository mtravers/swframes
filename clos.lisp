(in-package :sw)

(export '(defmethod$))

#|
Theory:

- Maps RDFS classes to CLOS classes
- Lisp class names are the URI keywordified. 
- $-versions of standard Lisp functions work on URIs


Todo +++:

- rdfs-def-class should be extended (rename defclass$ for consistency)
- metaclass?
- maintain relation between URI/frame/symbol/class in organized way

|#

(defclass rdfs-class (frame)
  ())

(defmethod initialize-instance :after ((f rdfs-class) &rest ignore)
  (with-slots (uri) f
    (unless uri
      (setf uri (gensym-instance-frame (class-frame (class-of f)) :uri-only? t)))))

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

;;; handle full range of defmethod hair +++
(defmacro defmethod$ (name args &body body)
  (let* ((&pos (position #\& args :key #'(lambda (arg) (and (symbolp arg) (char (symbol-name arg) 0)))))
	 (qualified-args (subseq args 0 &pos))
	 (rest-args (and &pos (subseq args &pos)))
	 (trans-args (mapcar #'(lambda (arg)
				 (if (and (listp arg) (frame-p (cadr arg)))
				     (list (car arg) (class-name (sw::rdfs-clos-class (cadr arg) :force? t)))
				     arg))
			     qualified-args)))
    `(defmethod ,name ,(nconc trans-args rest-args) ,@body)))


