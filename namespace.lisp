(in-package :swframes)

;;; was colliding with s-xml.  Actually, why don't I just reuse it, it's orbably the same code.  Oh well
(defparameter *sw-namespaces* nil)

(defun sw-register-namespace (abbrev full)
  ;; +++ detect redefinitions
  (push (list abbrev full) *sw-namespaces*))

(defun abbreviate-uri (uri)
  (dolist (namespace *sw-namespaces*)
    (let ((full (cadr namespace)))
      (when (and (>= (length uri) (length full))
		 (string= uri full :end1 (length full)))
	(return-from abbreviate-uri (values (format nil "~A:~A" (car namespace) (subseq uri (length full)))
					    (car namespace)
					    )))))
  (values uri nil))

(defun namespace-lookup (namespace)
  (find namespace *sw-namespaces* :key #'car :test #'string=))

(defun namespace-expand (namespace)
  (cadr (namespace-lookup namespace)))

(defun expand-uri (uri)
  (let ((colonpos (position #\: uri)))
    (if colonpos
	(let ((namespace (namespace-lookup (subseq uri 0 colonpos))))
	  (if namespace
	      (format nil "~A~A" (cadr namespace) (subseq uri (1+ colonpos)))
	      uri))
	uri)))
		      
		   

