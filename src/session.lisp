(in-package :sw)

(rdfs-def-class #$sw:session ()
		(#$sw:session/machine))

(defvar *unique-session* nil)

;;; should get called once for a lisp session
(defun make-unique-session ()
  (let* ((*fast-instances?* nil)
	 (session
	  (rdfs-make-instance #$sw:session 
			      #$sw:session/machine (machine-instance))))
    (write-frame session)
    (setf *unique-session* session)))

(defun unique-session ()
  (or *unique-session*
      (make-unique-session)))

;;; This has to be relative to a frame source so you can check for taken ids. 
;;; fast? mode does not go to the database each time, and is suitable for when there is a single lisp server.  
(defvar gensym-lock (acl-compat.mp:make-process-lock))

(defun gensym-instance-frame (class &key start (fast? nil) (source *default-frame-source*) base uri-only?)
  (if (eq (frame-source class) *code-source*) ;+++ this seems rather radical, probably a mistake?
      (setf (frame-source class) source)
      ;; Here we might want to do an initial write of frame to db
      )
  (unless base (setq base (frame-uri class)))
  (acl-compat.mp:with-process-lock (gensym-lock)	;+++ I hope this won't slow down the world too much.
    (unless (and fast?
		 (msv class #$sw:last_used_id))
      (setf (slotv class #$sw:last_used_id) nil) ;in lieu of a full reset
      (fill-frame class :force? t :inverse? nil)
      )
    (let* ((v (slotv class #$sw:last_used_id))
	   (last (or start
		     (if (listp v) (first (last v)) v)))
	   (next (if last
		     (1+ (coerce-number last))
		     0))
	   (uri (string+ base "/"
			 (if fast? (string+ (frame-label (unique-session)) "/") "")
			 (fast-string next))))
      (if (and (not fast?) (uri-used? source uri))
	  (gensym-instance-frame class :start next :fast? fast?)
	  (progn
	    (add-triple class #$sw:last_used_id next :to-db (and (not fast?) *default-frame-source*) :remove-old t)
	    (if uri-only?
		uri
		(intern-uri uri :class class :source source)))))))


(defgeneric uri-used? (source uri))
