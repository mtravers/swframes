(in-package :sw)

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


(export '(defmethod$))

#|
Theory:

- Maps RDFS classes to CLOS classes
- Lisp class names are the URI keywordified. 
- $-versions of standard Lisp functions work on frames (defmethod$, defclass$, make-instance$)

Todos:
- metaclass?
- maintain relation between URI/frame/symbol/class in organized way
|#

(defclass rdfs-class (frame)
  ())

;;; Set up some universals
(setf (get 'rdfs-class :frame) #$rdfs:Resource)
(defun universal-slot (s)
  (setf (ssv s #$rdfs:domain) #$rdfs:Resource))

(universal-slot #$rdfs:label)

(defmethod initialize-instance :after ((f rdfs-class) &rest ignore)
  (with-slots (uri) f
    (unless uri
      (setf uri (gensym-instance-frame (class-frame (class-of f)) :uri-only? t)))))

;;; +++ should do this through metaclass
(defun class-frame (class)
  (get (class-name class) :frame))

(defun frame-as-symbol (frame)
  (keywordize (frame-uri frame)))

(defun frame-as-symbol (frame)
  (keywordize (string+ "%RDF-" (subseq (frame-uri frame) 
				       (1+ (position #\/ (frame-uri frame) :from-end t))
				       ))))

#|  Written because I thought some implementations couldn't hack keyword class names, but now I think I was mistaken and the problem is elsewhere.

'(defun frame-as-symbol (frame)
  (intern (string-replace
	   (string-replace 
	   (string-replace (string-upcase (frame-uri frame))
			   ":" "")
	   "/" "")
	   "." "")
	  :sw)
  )
|#

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
		;; ++ I'm not sure how to do this in an implementation-indepent way. 
		#+:CCL
		(not (typep class 'ccl:forward-referenced-class))
		#+:SBCL			;untested
		(not (type class 'sb-mop:forward-referenced-class))
		#+:ALLEGRO		;untested
		(not (excl::forward-referenced-class-p class))
		)
	   class)
	  (force?
	   (setf (get sym :frame) frame)
	   (let ((supertypes (slotv frame #$rdfs:subClassOf)))
	     (mapcar 'rdfs-clos-class supertypes)
	     (eval (defclass-form frame supertypes)))
	   (find-class sym t))
	  (error?
	   (error "Can't turn ~A into class" frame))
	  (t nil))))

(defun ensure-clos-class (frame)
  (rdfs-clos-class frame :force? t :error? t))

;;; Set class of frame based on its RDF type
(defun classify-frame (f &key error? force?)
  (when (or force? (eq 'frame (type-of f)))
    (let ((rclasses (collapse-class-list (slotv f #$rdf:type))))
      (cond ((= 1 (length rclasses))
	     (set-frame-class f (car rclasses) error?))
	    ((null rclasses)
	     (if error? (error "Can't set class, no type for ~A" f)))
	    (t
	     ;; Multiple types, so heuristicate -- pick one that already is a CLOS class
	     ;; minimal thing is redundnat with collapse-class-list
	     (let ((minimal nil))
	       (dolist (c rclasses)
		 (let ((clos (rdfs-clos-class c :force? nil :error? nil)))
		   (when (and clos
			      (or (null minimal)
				  (member clos (subclasses minimal))))
		   (setf minimal clos))))
	       (when minimal
		 (change-class f minimal)
		 minimal)))))))

(defun set-frame-class (frame rdf-class &optional error?)
  (let ((cclass (rdfs-clos-class rdf-class :force? t :error? error?)))
    (if cclass 
	(change-class frame cclass)
	(if error?
	    (error "Can't set class for ~A, no class found" frame)))
    cclass))

(defmacro defmethod$ (name &rest rargs)
  (let* ((argpos (position 'cons rargs :key #'type-of))
	 (qualifiers (subseq rargs 0 argpos))
	 (args (nth argpos rargs))
	 (body (nthcdr (1+ argpos) rargs))
	 (&pos (position #\& args :key #'(lambda (arg) (and (symbolp arg) (char (symbol-name arg) 0)))))
	 (qualified-args (subseq args 0 &pos))
	 (rest-args (and &pos (subseq args &pos)))
	 (trans-args (mapcar #'(lambda (arg)
				 (if (and (listp arg) (frame-p (cadr arg)))
				     (list (car arg) (class-name (sw::rdfs-clos-class (cadr arg) :force? t)))
				     arg))
			     qualified-args)))
    `(defmethod ,name ,@qualifiers ,(nconc trans-args rest-args) ,@body)))


