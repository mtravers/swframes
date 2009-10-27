(in-package :swframes)

(export '(sparql-endpoint make-sparql-source
	  do-sparql do-sparql-one-var
	  bulk-load-query augment-query
 	  case-insensitize case-insensitize-2 
	  post-fill
	  *default-sparql-endpoint*
	  *default-sparql-timeout*))

(defvar *default-sparql-endpoint* nil "The SPARQL-ENDPOINT used by default if none is specified.")

;;; might want to register these somewhere
(defun make-sparql-source (url &key writeable?)
  "Make a SPARQL endpoint for a given URL"
  (make-instance 'sparql-endpoint
		 :url url
		 :writeable? writeable?))

(defclass* sparql-endpoint (frame-source)
  (url
   (writeable? nil)
   (read-graph nil)			;if set, SEXP queries are limited to that graph 
   (write-graph nil))
  :initable-instance-variables
  (:readable-instance-variables url read-graph write-graph)
  (:documentation "A FRAME-SOURCE that represents a SPARQL endpoint, optionally with given named graphs for reading and writing.")
  )

(defmethod* print-object ((sparql sparql-endpoint) stream)
  (format stream "#<~A ~A ~A ~A>" 
	  (type-of sparql)
	  url
	  (if writeable? "[w]" "[nw]")
	  (if read-graph (format nil "[rg: ~A]" read-graph))))

(defvar *default-sparql-timeout* 30)

(defgeneric do-sparql (source command &key timeout)
  (:documentation #.(doc "Perform a SPARQL command"
"SOURCE is a SPARQL-ENDPOINT."
""
"COMMAND can be a string in SPARQL syntax, or a sexp in the followng syntax)"
""
"(:select <vars> <options> <clauses>*)"
"(:delete <triple> <options> <clauses>*"
"(:insert <triple> <options> <clauses>*"
""
"Where:"
" <vars> is a list of query variables (?foo, etc) or :all, or :count."
""
"<options> is a key/val list:"
"For :select:"
":limit n"
":distinct <boolean>"
":from:  A frame specifying a named graph"
":offset n "
":order  value can be a single variable (?var) a 2-list (:desc/:asc ?var), or a list of such elements"
""
"For :delete:"
":from <graph>"
"For :insert:"
":into <graph>"

"<triple>s are"
"  (subject predicate object)"
"Where subject, predicate, object are frames or sparql variables"

"Clauses are triples: "
"Or"
"(:filter ...)"
"(:union ...)"

"The value returned from a :SELECT is a list of binding sets; each set is of the form ((?var1 value1) (?var2 value2) ...)"

":insert and :delete operations..."

)))

(defmethod do-sparql ((sparql string) (command t) &key (timeout *default-sparql-timeout*))
  (do-sparql (make-instance 'sparql-endpoint :url sparql) command :timeout timeout))

(defmethod do-sparql ((sparql null) (command t) &key (timeout *default-sparql-timeout*))
  (unless *default-sparql-endpoint*
    (error "No default SPARQL endpoint defined"))
  (do-sparql *default-sparql-endpoint* command :timeout timeout))

;;; Now will set the source of new frames...which is not always right, but better than nothing
(defmethod* do-sparql ((sparql sparql-endpoint) (command string) &key (timeout *default-sparql-timeout*))
;  (print command)
  (run-sparql url command 
		      :make-uri #'(lambda (u) (intern-uri u sparql))
		      ;; this suddenly became necessary since I was geting literals back...no idea why 
		      :eager-make-uri? t
		      :timeout timeout
		      ))

;;; Handles translation and breaking up query into chunks if result set is too big
(defmethod* do-sparql ((sparql sparql-endpoint) (query list) &key (timeout *default-sparql-timeout*) (chunk-size 5000))
  (flet ((do-it ()
	   (do-sparql sparql (generate-sparql sparql query) :timeout timeout))
	 (modify-query (offset)
	     (setf (third query)
		   (append  `(:offset ,offset :limit ,chunk-size)
			    (if (zerop offset)
				(third query)
				(subseq (third query) 4))))))
    ;; if query already has limit or offset, no chunking
    (if (or (not (eq (car query) :select))
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

(defgeneric do-sparql-one-var (sparql query &optional var)
  (:documentation 
   "Perform a SPARQL query and return a simple list of frames.  Query should either have one variable returned; or you can specify one with the optional VAR argument."))

(defmethod do-sparql-one-var ((sparql t) query &optional var)
  (multiple-value-bind (res vars)
      (do-sparql sparql query)
    (extract-sparql-binding res (or var (car vars)))))

;;; +++ to be replaced with something better.  Virtuoso indicates bnodes in the XML returned now.
(defun bnode? (frame)
  (string-prefix-equals (frame-uri frame) "nodeID://"))

(defvar *sparql-namespace-uses*)

;;; :order value can be a single element, a 2-list (:desc/:asc ?elt), or a list of such elements
(defmethod* generate-sparql ((sparql sparql-endpoint) form)
  (let ((*sparql-namespace-uses* nil)
	(*print-case*  :downcase)
	query)
    (setq query
	  ;; DELETE and INSERT can take WHERE clauses, not supported here yet
	  ;; redone as separate methods on endpoints.
	  (case	(car form)
      (:select
       (destructuring-bind (vars (&key limit offset distinct (from read-graph) order) &rest clauses) (cdr form)
	 (with-output-to-string (s) 
	     (format s "SELECT ~a ~a ~a~%WHERE { "
		     (if distinct "DISTINCT " "")
		     (cond ((member vars '(* :all))
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
      (:insert 
       (destructuring-bind (triple (&key (into write-graph)) &rest clauses) (cdr form)
	 (with-output-to-string (s) 
	   (format s
		   "INSERT INTO GRAPH ~A { ~A ~A ~A }"
		   (sparql-term (make-frame into))
		   (sparql-term (first triple))
		   (sparql-term (second triple))
		   (sparql-term (third triple)))
	   (when clauses
	     (write-string "WHERE { " s)
	     (loop for clause in clauses
		do (emit-sparql-clause clause s))
	     (write-string " }" s)))))
      (:delete 
       (destructuring-bind ((s p o) (&key (from write-graph)) &rest clauses) (cdr form)
	 (with-output-to-string (str) 
	   (format str
		   "DELETE FROM GRAPH ~A { ~A ~A ~A }"
		   (sparql-term (make-frame from))
		   (sparql-term s)
		   (sparql-term p)
		   (sparql-term o))
	   (when (or (symbolp s) (symbolp o) (symbolp p))
	     (push (list s p o) clauses)
	     )
	   (when clauses
	     (write-string "WHERE { " str)
	     (loop for clause in clauses
		do (emit-sparql-clause clause str))
	     (write-string " }" str)))))
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
    (null (error "NIL in SPARQL"))
    (frame (format nil "<~A>" (frame-uri thing)))
    (symbol
     (if (var-p thing)
	 (string-downcase (string thing))
	 (error "Can't translate ~A into a SPARQL term" thing)))
    (string (if (or (position #\" thing) (position #\Newline thing))
		(format nil "'''~A'''" (backslash-quote-string thing))
		(format nil "\"~A\"" (backslash-quote-string thing))))
    (fixnum (fast-string thing))
    ;; SPARQL can't handle 3.0D3
    (double-float (fast-string (coerce thing 'single-float)))
    ;; Newish way to generate language-specific literals (ie Melanoma@en) (Melanoma :en)
    ;; or types (Melanoma #$xsd:string)
    (list
     (cond ((eq (car thing) :uri)      
	    (format nil "<~A>" (cadr thing)))	    
	   ((symbolp (cadr thing))
	    (format nil "~A@~A" (sparql-term (car thing)) (cadr thing)))
	   ((frame-p (cadr thing))
	    (format nil "~A^^~A" (sparql-term (car thing)) (abbreviate-uri (frame-uri (cadr thing)))))
	   (t (error "Can't translate ~A into a SPARQL term" thing))
	   ))
    (t  (error "Can't translate ~A into a SPARQL term" thing)
;       (fast-string thing)
       )))

;;; Virtuoso requires this
(defun backslash-quote-string (s)
  (string-replace s "\\" "\\\\"))

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
		  (sparql-term el))))
	 (emit-subclause-list (clauses)
	   (format s " { ")
	   (mapcar (lambda (c) (emit-sparql-clause c s)) clauses)
	   (format s "~% } ")))
    (cond ((eq (car clause) :optional)
	   (format s "~%OPTIONAL ")
	   (emit-subclause-list (cdr clause)))
	  ((eq (car clause) :union)
	   (loop for (sub more) on (cdr clause) do
		(format s "~% { ")
		(mapcar (lambda (c) (emit-sparql-clause c s)) sub)
		(format s "~% } ")
		(when more (write-string " UNION " s)))
	   (write-string "." s))
	  ((eq (car clause) :graph)
	   (format s "~%{ GRAPH ~A " 
		   (sparql-term (cadr clause)))
	   (emit-subclause-list (cddr clause))
	   (format s " }"))
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
  (let ((*fill-by-default?* nil))
    (rdfs-call-if post-fill frame)))

(defmethod fill-frame-sparql ((frame frame) (source sparql-endpoint))
    (let* ((*default-frame-source* source) ;not sure
	   (results  (do-sparql 
			   source
			 (format nil "select ?p ?o where { <~A> ?p ?o . }" (frame-uri frame)))))
      (dolist (binding results)
	(let ((p (sparql-binding-elt binding "p"))
	      (o (sparql-binding-elt binding "o")))
	  (if (%slotv p #$crx:specialhandling)
	      (rdfs-call deserialize-slot p frame o)
	      (add-triple frame p o))
	  ))
      (when results
	(set-frame-loaded? frame))
      ))

(rdfs-defmethod deserialize-slot ((slot #$crx:slots/LispValueSlot) frame value)
		(setf (%slotv frame slot)
		      (if (stringp value)
			  (read-from-string value)
			  value)))

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

;;; +++ not working in all cases, see drugs-for-genes1
;;; +++ also too slow
(defun case-insensitize (query)
  "Given a SPARLQ query, convert all string literal objects into case-insensitive regex searches."
  (Setq query (copy-tree query))
  (let ((new-clauses nil))
    (flet ((process-triple (triple)
	     (when (stringp (third triple))	;+++ doesn't play with ("foo" :en) clauses
	       (let ((var (gensym "?v")))
		 (push `(:filter (:regex ,var ,(format nil "^~A$" (third triple)) "i")) new-clauses)
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

(defun case-insensitize-2 (query)
  #.(doc 
     "Alternate, much faster version of CASE-INSENSITIZE, does not handle all cases though,"
     "Works by searching for upcased, downcased, and capitalized forms of all string literals.")
  (setq query (copy-tree query))
    (labels ((modify-triple (triple)
	     (if (stringp (third triple))	;+++ doesn't play with ("foo" :en) clauses
		 `(:union ((,(car triple) ,(cadr triple) ,(caddr triple)))
			  ((,(car triple) ,(cadr triple) ,(string-downcase (caddr triple))))
			  ((,(car triple) ,(cadr triple) ,(string-upcase (caddr triple))))
			  ((,(car triple) ,(cadr triple) ,(string-capitalize (caddr triple)))))
		 triple
		 ))
	   (modify-clause (clause)
	     (cond ((eq (car clause) :union)
		    (cons :union
			  (mapcar #'(lambda (group)
				      (mapcar #'modify-clause group))
				  (cdr clause))))
		   (t (modify-triple clause))))
	   )	     
      `(,(car query) 
	 ,(cadr query)
	 ,(caddr query)
	 ,@(mapcar #'modify-clause (nthcdr 3 query)))))

;;; +++ could be generalized for other dependent properties
;;; OPTIONAL could be optional
(defun include-labels (vars query)
  ;; this defaulting of vars is almost never the right thing.  Also, won't deal with :optional and other constructs
  (unless vars
    (setq vars
	  (collecting 
	   (dolist (clause (nthcdr 3 query))
	     (when (var-p (third clause))
	       (collect-new (third clause)))
	     (when (var-p (first clause))
	       (collect-new (first clause)))
	     ))))
  (setq query (copy-tree query))
  (dolist (var vars)
    (let ((label-var (intern (string+ (string var) "_label") :keyword)))
      (push-end label-var (second query))
      (push-end `(:optional (,var #$rdfs:label ,label-var)) query)))
  query)

(defun bulk-load-query (source query &key (var (car (second query))))
  #.(doc
     "Given a SPARQL query and a VAR extend the query to load all slots of frames that match VAR, and mark them as loaded."
     "This function actually peforms the query and returns the list of frames matching VAR")
  (setq query (copy-tree query))	;we mutate query, so copy it first
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

(defun augment-query (source query &key (var (car (second query))) slots &aux slot-vars)
  "Add some slots to a one-variable query (like bulk-load-query but selective rather than loading everything)"
  (setq query (copy-tree query))
  (dolist (slot slots)
    (let ((slot-var (intern (format nil "?SLOT~D" (position slot slots)))))
      (push slot-var slot-vars)
      (push-end `(:optional (,var ,slot ,slot-var)) query)
      (push-end slot-var (second query))))
  (setf slot-vars (nreverse slot-vars))
  (let ((res (do-sparql source query)))
    (collecting 
     (dolist (bind res)
       (let ((s (sparql-binding-elt bind var)))
	 (collect-new s)
	 (mapc #'(lambda (slot slot-var)
		   (let ((val (sparql-binding-elt bind slot-var)))
		    (when val
		      (add-triple s slot val))))
	      slots slot-vars
	  ))))))

(defun augment-query-standard (source query &key (var (car (second query))))
  (augment-query source query :var var :slots (list #$rdf:type #$rdfs:label)))
       
    
