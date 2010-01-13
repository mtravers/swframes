(in-package :sw)

#|
Theory:
- need a mapping from URIs to class names.
- rdfs-def-class should be extended

- top goal is to get rid of our crappy method dispatch and use CLOSs

- 2-way connection between CLOS class and RDFS?

- metaclass?
- classes could 

- steal from activerdf

Notes:
- having frames of different classes means that our straightforward uri->frame map could be compromised.

- maintain relations between:
  - URI
  - frame (which is an instance of FRAME and possibly other classes)
  - symbol
  - CLOS class
  

OK -- when a frame is read from a sparql query, we have to check its class before
making a frame (or use change-class when it is determined).  I suppose fill-frame 
could do it.


|#

;;; New CLOS stuff
(defclass rdfs-class (frame)
  ())

(defmethod initialize-instance :after ((f rdfs-class) &rest ignore)
  (with-slots (uri) f
    (unless uri
      (setf uri (gensym-instance-uri (class-frame (class-of f)))))))

(defun class-frame (class)
  (get (class-name class) :frame))

(defun frame-as-symbol (frame)
  (keywordize (frame-name frame)))

(defun defclass-form (frame supertypes)
  `(defclass ,(frame-as-symbol frame)
       ,(append (mapcar #'frame-as-symbol supertypes)
		(list 'rdfs-class))
     ()))

;;; older, doesnt force
'(defun rdfs-clos-class (frame &optional (error? t))
  (let ((sym (frame-as-symbol frame)))
    (find-class sym error?)))

;;; newer
(defun rdfs-clos-class (frame &key force? (error? t))
  (let ((sym (frame-as-symbol frame)))
    (acond ((find-class sym nil)
	    it)
	   (force?
	    (eval (defclass-form frame nil))
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


#|  has to be later
;;; Discovery!

;;; code-created classes are not in the db, so need to this (or write them out)
; (slotv-inverse  #$rdfs:Class #$rdf:type)

(defmethod discover-classes ((ep sparql-endpoint))
  (let ((classes (rdfs-find :all :class #$rdfs:Class :source ep)))
    (mapcar #'frame-class classes)))

|#
