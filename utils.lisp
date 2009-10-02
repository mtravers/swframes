(in-package :swframes)

(export '(string+ parse-xml html-string uri-tag coerce-number in-background-thread))

(defun string+ (&rest args)
  (apply #'concatenate 'string args))

;;; try to standardize the call to this, since there are a few random things we need to do often.
(defun parse-xml (source)
  (let* (; (s-xml::*ignore-namespaces* t)
	 (ccl:*make-package-use-defaults* nil)) ;fixes a nasty bug where tags lose their namespaces if their symbol is defined in CL!
    (s-xml:parse-xml-string (adjust-sparql-string source))))

(defun parse-xml (source)
  (let* (; (s-xml::*ignore-namespaces* t)
	 (ccl:*make-package-use-defaults* nil)) ;fixes a nasty bug where tags lose their namespaces if their symbol is defined in CL!
    (s-xml:parse-xml-string (adjust-sparql-string source))))

(defmacro html-string (&body stuff)
  `(with-output-to-string (s)
     (let ((net.aserve::*html-stream* s))
       (net.aserve::html ,@stuff))))

(defun uri-tag (uri)
  (subseq uri (1+ (position #\# uri))))

(defun coerce-number (slotv &key no-error (default slotv))
  (typecase slotv
    (null (if no-error
	      default
	      (error "Can't coerce nil to a number")))
    (list
     (warn "Multiple values ~A" slotv)
     (coerce-number (car slotv)))
    (number slotv)
    (string 
     (let ((n (read-from-string slotv)))
       (if no-error
	   (if (numberp n) n default)
	   (progn
	     (assert (numberp n))
	     n))))
    (t (error "can't coerce ~A to a number" slotv))))

;;; needs some error handling +++
(defmacro in-background-thread (&body body)
  `(acl-compat.mp:process-run-function
    (string (gensym "THREAD"))
    #'(lambda () ,@body)))

;;; Until we figure out something better.
(defmacro doc (&rest string-designators)
  `(bio::one-string-nl ,@string-designators))

;;; Based on bio::one-string-nli
(defmacro doc (&rest string-designators)
  "Inserts newlines after every argument except the last, and calls ONE-STRING"
  (let ((nl (string #\Newline)))
    `(bio::one-string
      ,@(loop for strings on string-designators 
              as s = (first strings)
              nconc 
             (if (cdr strings) (list s nl) (list s))))))
