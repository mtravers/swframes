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

(defun rdfs-clos-class (frame &optional (error? t))
  (let ((sym (frame-as-symbol frame)))
    (find-class sym error?)))		
#|
    (unless
      (setf (get sym :frame) frame)
;      (print `(defining ,sym))
;      (eval (defclass-form frame)))
    sym))

|#

;;; Setting the class of a frame

(defun classify-frame (f)
  (when (eq 'frame (type-of f))
    (set-frame-class f nil))
  )

(defun set-frame-class (f &optional error?)
  (let ((rclass (collapse-class-list (slotv f #$rdf:type))))
    (if (= 1 (length rclass))
	(let ((cclass (rdfs-clos-class (car rclass) error?)))
	  (if cclass 
	      (change-class f cclass)
	      (if error?
		  (error "Can't set class for ~A, no class found" f))))
	(if error?
	    (error "Can't set class, no or multiple types for ~A" f)
	    ))))

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
