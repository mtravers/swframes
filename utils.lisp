(in-package :swframes)

(defun string+ (&rest args)
  (apply #'concatenate 'string args))

;;; try to standardize the call to this, since there are a few random things we need to do often.
(defun parse-xml (source)
  (let* (; (s-xml::*ignore-namespaces* t)
	 (ccl:*make-package-use-defaults* nil)) ;fixes a nasty bug where tags lose their namespaces if their symbol is defined in CL!
    (s-xml:parse-xml-string (knewos::adjust-sparql-string source))))

(defmacro html-string (&body stuff)
  `(with-output-to-string (s)
     (let ((*html-stream* s))
       (html ,@stuff))))
