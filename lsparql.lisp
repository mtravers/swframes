(in-package :swframes)

(export '(sparql-endpoint make-sparql-source
	  do-sparql do-sparql-one-var
	  post-fill *default-sparql-endpoint*))

(defvar *default-sparql-endpoint* nil)
(defvar *sparql-namespace-uses*)

;;; might want to register these somewhere
(defun make-sparql-source (uri &key writeable?)
  (make-instance 'sparql-endpoint
		 :uri uri
		 :writeable? writeable?))

(defclass* sparql-endpoint (frame-source)
  (uri
   (writeable? nil)
   (read-graph nil)			;if set, SEXP queries are limited to that graph 
   (write-graph nil))
  :initable-instance-variables
  (:readable-instance-variables uri read-graph write-graph)
  )

(defmethod* print-object ((sparql sparql-endpoint) stream)
  (format stream "#<~A ~A ~A ~A>" 
	  (type-of sparql)
	  uri
	  (if writeable? "[w]" "[nw]")
	  (if read-graph (format nil "[rg: ~A]" read-graph))))

(defvar *sparql-default-timeout* 30)

(defmethod do-sparql ((sparql null) (command t) &key (timeout *sparql-default-timeout*))
  (do-sparql *default-sparql-endpoint* command :timeout timeout))

;;; Now will set the source of new frames...which is not always right, but better than nothing
(defmethod* do-sparql ((sparql sparql-endpoint) (command string) &key (timeout *sparql-default-timeout*))
    (knewos::run-sparql uri command 
			:make-uri #'(lambda (u) (intern-uri u sparql))
			;; this suddenly became necessary since I was geting literals back...no idea why 
			:eager-make-uri? t
			:timeout timeout
			))

;;; Handles translation and breaking up query into chunks if result set is too big
(defmethod* do-sparql ((sparql sparql-endpoint) (query list) &key (timeout *sparql-default-timeout*) (chunk-size 1000))
  (flet ((do-it ()
	   (do-sparql sparql (generate-sparql sparql query) :timeout timeout))
	 (modify-query (offset)
	   (setf (third query)
		 (append  `(:offset ,offset :limit ,chunk-size)
			  (if (zerop offset)
			      (third query)
			      (subseq (third query) 4))))))
    ;; if query already has limit or offset, no chunking
    (if (or (member :offset (third query))
	    (member :limit (third query)))
	(do-it)
	(progn 
	  (modify-query 0)
	  (do ((offset 0 (+ offset chunk-size))
	       (vars nil) (result nil)
	       (concat nil))
	      ((and vars (< (length result) chunk-size))
	       (values concat vars))
	    (multiple-value-bind (iresult ivars) 
		(do-it)
	      (setf vars ivars result iresult)
	      (setf concat (nconc concat result))
	      (modify-query offset)))))))
	   

;;; Return a simple list of results.  Query should either have one open variable or you can specify one with the optional argument
(defmethod do-sparql-one-var ((sparql t) query &optional var)
  (multiple-value-bind (res vars)
      (do-sparql sparql query)
    (extract-sparql-binding res (or var (car vars)))))

;;; +++ to be replaced with something better.  Virtuoso indicates bnodes in the XML returned now.
(defun bnode? (frame)
  (string-prefix-equals (frame-uri frame) "nodeID://"))

;;; :order value can be a single element, a 2-list (:desc/:asc ?elt), or a list of such elements
(defmethod* generate-sparql ((sparql sparql-endpoint) form)
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
       (destructuring-bind (vars (&key limit offset distinct (from read-graph) order) &rest clauses) (cdr form)
	 (with-output-to-string (s) 
	     (format s "SELECT ~a ~a ~a~%WHERE { "
		     (if distinct "DISTINCT " "")
		     (cond ((eq vars '*)
			    "*")
			   ((eq vars :count)
			    "count(*)")
			   (t (format nil "~{~a~^ ~}" vars)))
		     (if from (format nil "~{ FROM ~a ~^~%~}" (mapcar #'sparql-term (mapcar #'intern-uri (if (listp from) from (list from))))) " ")
		     )
	     (loop for clause in clauses
		do (emit-sparql-clause clause s))
	     (format s "}")
	     (when order
	       (unless (listp order) (setf order (list order)))
	       (format s " ORDER BY ~{~A ~}"
		       (mapcar #'(lambda (clause)
				   (cond ((symbolp clause)
					  clause)
					 ((eq :asc (car clause))
					  (cadr clause))
					 ((eq :desc (car clause))
					  (format nil "DESC(~A)" (cadr clause)))))
			       order)))
	     ;; Virtuoso complains if LIMIT comes before ORDER, although that's perfectly valid...
	     (when limit
	       (format s "~%LIMIT ~a " limit))
	     (when offset
	       (format s "~%OFFSET ~a " offset))
	     )))
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
;;; see http://www.w3.org/TR/rdf-sparql-query/#QSynLiterals
(defun sparql-term (thing)
  (typecase thing
    (frame (format nil "<~A>" (frame-uri thing)))
    (symbol
     (string-downcase (string thing)))
    (string (if (or (position #\" thing) (position #\Newline thing))
		(format nil "'''~A'''" thing)
		(format nil "\"~A\"" thing))) ;add @en for fun
    ;; SPARQL can't handle 3.0D3
    (double-float (utils:fast-string (coerce thing 'single-float)))
    ;; Newish way to generate language-specific literals (ie Melanoma@en)
    (list
     (cond ((eq (car thing) :uri)
	    (format nil "<~A>" (cadr thing)))	    
	   (t (format nil "~A@~A" (sparql-term (car thing)) (cadr thing)))))
    (t ; (error "Can't translate ~A into a SPARQL term" thing)
       (utils:fast-string thing)
       )))


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
		(mapcar (lambda (c) (emit-sparql-clause c s)) sub)
		(format s "~% } ")
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

(defvar sparql-binary-ops '((and "&&")(or "||") (equal "=") (< "<") (> ">") (>= ">=") (<= "<=")))

;;; should use sparql-term
(defun emit-sparql-filter (expression s)
  (let ((*print-case* :downcase))
    (cond ((and (listp expression)
		(assoc (car expression) sparql-binary-ops))
	   (write-char #\( s)
	   (loop for rest on (cdr expression) do 
		(emit-sparql-filter (car rest) s)
		(when (cdr rest) 
		  (format s " ~a " (second (assoc (car expression) sparql-binary-ops)))))
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
 	  ((or (stringp expression)
	       (numberp expression))
 	   (format s "~s" expression))
	  ((and (listp expression)
		(eq (car expression) :regex))
	   (format s "regex(~A, \"~A\", \"~A\")" (second expression) (third expression) (or (fourth expression) "")))
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

(defun sparql-binding-elt (binding v)
  (if (symbolp v) (setf v (string-downcase (string v))))
  (if (char= #\? (char v 0)) (setf v (subseq v 1)))
  (cadr (find v binding :key #'car :test #'equal)))

(defun extract-sparql-binding (binding-list v)
  (mapcar #'(lambda (binding) (sparql-binding-elt binding v)) binding-list))


(defmethod sanity-check ((endpoint sparql-endpoint))
  (do-sparql endpoint `(:select (?s ?p ?o) (:limit 10) (?s ?p ?o) )))

(defmethod fill-frame-from ((frame frame) (source sparql-endpoint) &key inverse?)
;;; this causes too many problems...needs rethinking
;  (reset-frame frame)	
  (fill-frame-sparql frame source)
  (when inverse?
    (fill-frame-inverse-sparql frame source))
  (rdfs-call-if post-fill frame))

(defmethod fill-frame-sparql ((frame frame) (source sparql-endpoint))
    (let* ((*default-frame-source* source) ;not sure
	   (results  (do-sparql 
			   source
			 (format nil "select ?p ?o where { <~A> ?p ?o . }" (frame-uri frame)))))
      (dolist (binding results)
	(let ((p (sparql-binding-elt binding "p"))
	      (o (sparql-binding-elt binding "o")))
	  (if (%slotv p #$crx:specialhandling)
	      (setf o (rdfs-call deserialize-value p o)))
	  (add-triple frame p o)
	  ))
      (when results
	(set-frame-loaded? frame))
      ))

(rdfs-defmethod deserialize-value ((slot #$crx:slots/LispValueSlot) value)
		(report-and-ignore-errors
		 (read-from-string value)))

;;; +++ this can time out without the limit, but of course it produces incorrect results.  Maybe ths should only be done on demand.
(defmethod fill-frame-inverse-sparql ((frame frame) (source sparql-endpoint))
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-hash-table :test #'eq)))
  (let ((*default-frame-source* source))
    (dolist (binding (do-sparql 
			 source
		       `(:select (?s ?p) (:limit 100) (?s ?p ,frame))))
      (add-triple (sparql-binding-elt binding "s") 
		  (sparql-binding-elt binding "p")
		  frame)
      )))

(defmethod uri-used? ((source sparql-endpoint) uri)
  (do-sparql 
      source
    `(:select (?s ?p ?o) (:limit 1) (:union (((:uri ,uri) ?p ?o)) ((?s ?p (:uri ,uri)))))))

(defun var-p (thing)
  (and (symbolp thing)
       (char= #\? (char (string thing) 0))))

;;; convert all string literal objects into case-insensitive regex searches.
;;; +++ not working in all cases, see drugs-for-genes1
;;; +++ also too slow
(defun case-insensitize (query)
  (setq query (copy-tree query))
  (let ((new-clauses nil))
    (flet ((process-triple (triple)
	     (when (stringp (third triple))	;+++ doesn't play with ("foo" :en) clauses
	       (let ((var (gensym "?v")))
		 (push `(:filter (:regex ,var ,(formatn "^~A$" (third triple)) "i")) new-clauses)
		 (setf (third triple) var)
		 ))))	     
      (dolist (clause (nthcdr 3 query))
	(cond ((eq (car clause) :union)
	       (dolist (group (cdr clause))
		 (dolist (triple group)
		   (process-triple triple))))
	      (t (process-triple clause)))))
    (setf (nthcdr 3 query) (append (nthcdr 3 query) new-clauses)))
  query)

;;; alternate, less powerful but faster
(defun case-insensitize-2 (query)
  (setq query (copy-tree query))
    (flet ((modify-triple (triple)
	     (if (stringp (third triple))	;+++ doesn't play with ("foo" :en) clauses
		 `(:union ((,(car triple) ,(cadr triple) ,(caddr triple)))
			  ((,(car triple) ,(cadr triple) ,(string-downcase (caddr triple))))
			  ((,(car triple) ,(cadr triple) ,(string-upcase (caddr triple))))
			  ((,(car triple) ,(cadr triple) ,(string-capitalize (caddr triple)))))
		 triple
		 )))	     
      `(,(car query) 
	 ,(cadr query)
	 ,(caddr query)
	 ,@(mapcar #'(lambda (clause)
		       (cond ((eq (car clause) :union)
			      (cons :union
				    (mapcar #'(lambda (group)
						(mapcar #'modify-triple group))
					    (cdr clause))))
			     (t (modify-triple clause))))
		   (nthcdr 3 query)))))




;;; +++ could be generalized for other dependent properties
;;; OPTIONAL could be optional
(defun include-labels (vars query)
  ;; this defaulting of vars is almost never the right thing.  Also, won't deal with :optional and other constructs
  (unless vars
    (setq vars
	  (utils:collecting 
	   (dolist (clause (nthcdr 3 query))
	     (when (var-p (third clause))
	       (utils::collect-new (third clause)))
	     (when (var-p (first clause))
	       (utils::collect-new (first clause)))
	     ))))
  (setq query (copy-tree query))
  (dolist (var vars)
    (let ((label-var (intern (string+ (string var) "_label") :keyword)))
      (push-end label-var (second query))
      (push-end `(:optional (,var #$rdfs:label ,label-var)) query)))
  query)

;;; Given a SPARQL query and a var, extend the query to load all slots of var and mark frames as loaded.
(defun bulk-load-query (source query &key (var (car (second query))))
  (setq query (copy-tree query))
  (push-end `(,var ?bl_p ?bl_o) query)
  (push-end '?bl_p (second query))
  (push-end '?bl_o (second query))
  (let ((res (do-sparql source query)))
    (collecting
     (dolist (bind res)
	(let ((s (sparql-binding-elt bind var))
	      (p (sparql-binding-elt bind "bl_p"))
	      (o (sparql-binding-elt bind "bl_o")))
	  (add-triple s p o)
	  (collect-new s)
	  (set-frame-loaded? s)
	  (setf (frame-source s) source)
	  (when (and (frame-p o)		;not sure about this, but for now
		     (null (frame-source o)))
	    (setf (frame-source o) source))
	  )))))

;;; Add some slots to a one-var query (like bulk-load but selective rather than every thing)
(defun augment-query (source query &key (var (car (second query))) slots &aux slot-vars)
  (setq query (copy-tree query))
  (dolist (slot slots)
    (let ((slot-var (intern (format nil "?SLOT~D" (position slot slots)))))
      (push slot-var slot-vars)
      (push-end `(:optional (,var ,slot ,slot-var)) query)
      (push-end slot-var (second query))))
  (setf slot-vars (nreverse slot-vars))
  (let ((res (do-sparql source query)))
    (utils:collecting 
     (dolist (bind res)
       (let ((s (sparql-binding-elt bind var)))
	 (util:collect-new s)
	 (mapc #'(lambda (slot slot-var)
		   (let ((val (sparql-binding-elt bind slot-var)))
		    (when val
		      (add-triple s slot val))))
	      slots slot-vars
	  ))))))
    
