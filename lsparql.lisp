(in-package :swframes)

(defvar *sparql-namespace-uses*)

;;; might want to register these somewhere
(defun make-sparql-source (uri)
  (make-instance 'sparql-endpoint
		 :uri uri))

;;; +++ promulgate
(defclass* sparql-endpoint (frame-source)
  (uri
   (writeable? nil)
   (write-graph nil))
  :initable-instance-variables
  (:readable-instance-variables uri write-graph)
  )

(defmethod* print-object ((sparql sparql-endpoint) stream)
  (format stream "#<~A ~A>" (type-of sparql) uri))

(defvar *sparql-default-timeout* 30)

;;; Now will set the source of new frames...which is not always right, but better than nothing
(defmethod* do-sparql ((sparql sparql-endpoint) (command string) &key (timeout *sparql-default-timeout*))
  (net.aserve::with-timeout-local (timeout (error "SPARQL timeout from ~A" sparql))
    (knewos::run-sparql uri command :make-uri #'(lambda (u) (intern-uri u sparql)))))

(defmethod do-sparql ((sparql sparql-endpoint) (command list) &key (timeout *sparql-default-timeout*))
  (do-sparql sparql (generate-sparql command) :timeout timeout))

;;; Return a simple list of results.  Query should either have one open variable or you can specify one with the optional argument
(defmethod* do-sparql-one-var ((sparql sparql-endpoint) query &optional var)
  (multiple-value-bind (res vars)
      (do-sparql sparql query)
    (extract-sparql-binding res (or var (car vars)))))

;;; should generate a guaranteed unique new URI (+++ not really doing it yet)
(defmethod genuri ((sparql sparql-endpoint) prefix)
  (let ((frame (intern-uri (format nil "~A/~A" prefix (string (gensym))))))
    (setf (frame-source frame) sparql)
    frame))

;;; being replaced with do-sparql
(defun sparql-query (form)
  (error "replace with do-sparql")
  )

(defun generate-sparql (form)
  (let ((*sparql-namespace-uses* nil)
	(*print-case*  :downcase)
	query)
    (setq query
	  ;; DELETE and INSERT can take WHERE clauses, not supported here yet
	  ;; redone as separate methods on endpoints.
    (cond
#|
      ((eq (car form) :insert)
	   (destructuring-bind ((&key from) &rest clauses) (cdr form)
	     (with-output-to-string (s)
	       (format s "INSERT ~A { "
		     (if from (format nil "INTO GRAPH <~A>" (uri-full from)) ""))
	       (loop for clause in (cddr form)
		     do (emit-sparql-clause clause s))
	       (format s " }"))))
	  ((eq (car form) :delete)
	   (destructuring-bind ((&key from) &rest clauses) (cdr form)
	     (with-output-to-string (s)
	       (format s "DELETE ~A { "
		     (if from (format nil "FROM GRAPH <~A>" (uri-full from)) ""))
	       (loop for clause in (cddr form)
		     do (emit-sparql-clause clause s))
	       (format s " }"))))
|#
      ((eq (car form) :select)
       ;; +++ don't like
       (destructuring-bind (vars (&key limit distinct from) &rest clauses) (cdr form)
	 (with-output-to-string (s) 
	     (format s "SELECT ~a~{~a~^ ~}~a~%WHERE { "
		     (if distinct "DISTINCT " "")
		     vars 
		     (if from (format nil "~{ FROM <~a> ~^~%~}" (mapcar 'sparql-term (if (symbolp from) (list from) from))) "")
		     )
	     (loop for clause in clauses
		do (emit-sparql-clause clause s))
			 (format s "} ~a" (if limit (format nil "LIMIT ~a " limit) "")))))
	  (t (error "Can't handle ~A command yet" (car form)))))
    ;; add prefixes
    (let* ((prefix (with-output-to-string (p)
		     (loop for ns in *sparql-namespace-uses* 
			do (format p "PREFIX ~a <~a>~%" ns (namespace-lookup ns))))))
      (setq query (concatenate 'string prefix query ))
      ;; magic?
      (if (search "reasoning:" query)
	  (format nil "PREFIX reasoning: <http://www.mindswap.org/2005/sparql/reasoning#>~%~a" query)
	  query))
    ))

;;; +++ methodize
(defun sparql-term (thing)
  (typecase thing
    (frame (format nil "<~A>" (frame-uri thing)))
    (symbol
     (string-downcase (string thing)))
    (string (format nil "\"~A\"" thing))	;needs better quoting probably
    (t (mt:fast-string thing))))


#| for later
		 ((frame-p el)
		  (multiple-value-bind (string ns) (abbreviate-uri (frame-uri el) :sparql)
		    (if ns
			(progn 
			  (pushnew ns *sparql-namespace-uses* :test 'equal)
			  string)
			(format nil "<~a>" (frame-uri el)))))


		  (let ((transformed (expand-uri el)))
		    (if (eq el transformed)
			(cond ((stringp el)
			       (format nil "~s" el))
			      ((and (integerp el) (minusp el))
			       (format nil "\"~A\"^^<http://www.w3.org/2001/XMLSchema#integer>" el))
			      (t el))
			(format nil "<~a>" transformed)))))))



|#

(defun emit-sparql-clause (clause s)
  (flet ((maybe-format-uri (el)
	   (cond ((eq el :a)
		  "a")
		 ((eq el '[])
		  (emit-blank-node '[] nil))
		 ((and (keywordp el)
		       (char= (char (string el) 0) #\_))
		  (emit-blank-node el nil))
		 ((equal el "")
		  "\"\"")
		 ((and (stringp el) (char= (char el 0) #\<)
		       (char= (char el (1- (length el))) #\>))
		  el)
		 (t
		  (sparql-term el)))))
    (cond ((eq (car clause) :optional)
	   (format s "~%OPTIONAL { ")
	   (loop for sub in (cdr clause) do (funcall 'emit-sparql-clause sub s))
	   (format s "}."))
	  ((eq (car clause) :union)
	   (loop for (sub more) on (cdr clause) do
		(format s "~% { ")
		(mapcar (lambda(c) (emit-sparql-clause c s)) sub)
		(write-string "}" s)
		(when more (write-string " UNION " s)))
	   (write-string "." s))
	  ((eq (car clause) :filter)
	   (format s "~%FILTER ")
	   (emit-sparql-filter (second clause) s))
	  (t (apply 'format s "~%~a ~a ~a . " (mapcar #'maybe-format-uri clause))))))



;<http://www.mindswap.org/2005/sparql/reasoning#isCanonical>

(defparameter *sparql-function-names*
  '((is-canonical "reasoning:isCanonical")
    ))

;;; should use sparql-term
(defun emit-sparql-filter (expression s)
  (let ((*print-case* :downcase))
    (cond ((and (listp expression)
		(assoc (car expression) '((and "&&")(or "||") (equal "=") (< "<") (> ">"))))
	   (write-char #\( s)
	   (loop for rest on (cdr expression) do 
		(emit-sparql-filter (car rest) s)
		(when (cdr rest) 
		  (format s " ~a " (second (assoc (car expression) '((and "&&")(or "||") (equal "=") (< "<") (> ">")))))))
	   (write-char #\) s))
	  ((and (listp expression) (eq (car expression) 'not))
	   (write-string "(!(" s)
	   (loop for arg in (cdr expression) do (emit-sparql-filter arg s))
	   (write-string "))" s))
	  ((and (keywordp expression)
		(char= (char (string expression) 0) #\_))
	   (emit-blank-node expression s))
	  ((frame-p expression)		;uri
	   (format s " <~a> " (expand-uri (frame-uri expression))))
	  ;;  try to get FILTER clause in right format (still requires Jena)
 	  ((or (stringp expression)
	       (numberp expression))
 	   (format s "~s" expression))
	  (t
	   (if (atom expression)
	       (if (stringp expression)
		   (format s "~s" expression)
		   (princ (string-downcase (string expression)) s))
	       (progn
		 (format s "~a(" (or (second (assoc (car expression) *sparql-function-names*)) (car expression)))
		 (loop for rest on (cdr expression) do 
		      (emit-sparql-filter (car rest) s)
		      (when (cdr rest) 
			(write-char #\, s)))
		 (write-char #\) s)))))))

(defun emit-blank-node (name stream)
  (if (eq name '[])
      (format stream "[]")
      (let ((name (subseq (string name) 1)))
	(if (equal name "")
	    (emit-blank-node '[] stream)
	    (concatenate 'string "_:" name)))))

(defun sparql-binding-elt (bindings name)
  (cadar (member name bindings :key #'car :test #'string-equal)))

;;; ???
(defun sparql-binding-elt (binding v)
  (cadr (find v binding :key #'car :test #'equal)))

(defun extract-sparql-binding (binding-list v)
  (mapcar #'(lambda (binding) (sparql-binding-elt binding v)) binding-list))

;;; sparql clauses define a set using variable ?s, this loads all forward links fro those uris.
;;; +++ should be method on server.
(defmethod bulk-load ((server sparql-endpoint) sparql-clauses)
  (let* ((full-query `(:select (?s ?p ?o) () ,@sparql-clauses (?s ?p ?o)))
	 (res (do-sparql server full-query)))
    (collecting
      (dolist (bind res)
	(let ((s (sparql-binding-elt bind "s"))
	      (p (sparql-binding-elt bind "p"))
	      (o (sparql-binding-elt bind "o")))
	  (add-triple s p o)
	  (collect-new s)
	  (setf (frame-dereferenced? s) t) ;not really, but the equivalent
	  (setf (frame-source s) server)
	  (when (frame-p o)		;not sure about this, but for now
	    (setf (frame-source o) server))
	  )))))

(defmethod sanity-check ((endpoint sparql-endpoint))
  (do-sparql endpoint `(:select (?s ?p ?o) (:limit 10) (?s ?p ?o) )))

(defmethod fill-frame-from ((frame frame) (source sparql-endpoint))
  (fill-frame-sparql frame source)
  (fill-frame-inverse-sparql frame source))

(defmethod fill-frame-sparql ((frame frame) (source sparql-endpoint))
    (let ((*default-frame-source* source)) ;not sure
      (dolist (binding (do-sparql 
			   source
			 (format nil "select ?p ?o where { <~A> ?p ?o . }" (frame-uri frame))))
	(add-triple frame
		    (sparql-binding-elt binding "p")
		    (sparql-binding-elt binding "o"))
	)
      (setf (frame-loaded? frame) t)
      ))

(defmethod fill-frame-inverse-sparql ((frame frame) (source sparql-endpoint))
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-hash-table :test #'eq)))
  (let ((*default-frame-source* source))
    (dolist (binding (do-sparql 
			 source
		       `(:select (?s ?p) () (?s ?p ,frame))))
      (add-triple (sparql-binding-elt binding "s") 
		  (sparql-binding-elt binding "p")
		  frame)
      )))
