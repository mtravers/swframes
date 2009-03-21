(in-package :swframes)

;;; was colliding with s-xml.  Actually, why don't I just reuse it, it's about the same code.  
(defparameter *sw-namespaces* nil)

(defun sw-register-namespace (abbrev full)
  (aif (member abbrev *sw-namespaces* :key #'car :test #'string-equal)
      (unless (equal (cadr it) full)
	(warn "Attempt to redefine namespace ~A from ~A to ~A" abbrev (cadr it) full))
      (push (list abbrev full) *sw-namespaces*)))

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

(defun expand-uri-0 (ns string)
  (let ((namespace (namespace-lookup ns)))
    (if namespace
	(format nil "~A~A" (cadr namespace) string)
	(error "No namespace ~A" ns))))

