(in-package :swframes)

(export '(sparql-endpoint 
	  do-sparql do-sparql-one-var
	  bulk-load-query augment-query
 	  case-insensitize case-insensitize-2 
	  post-fill
	  *default-sparql-timeout*))

;;; bit of a crock -- figure out a good sparql source to use.  Will go to default of from-frame is code-source.
(defun default-sparql-source (from-frame)
  (cond ((null from-frame) *default-frame-source*)
	((typep (frame-source from-frame) 'sparql-endpoint)
	 (frame-source from-frame))
	((typep *default-frame-source* 'sparql-endpoint)
	 *default-frame-source*)
	(t (error "Can't figure out a SPARQL endpoint"))))

(defclass* sparql-endpoint (frame-source)
  (url
   (read-graph nil)			;if set, SEXP queries are limited to that graph 
   (write-graph nil))
  :initable-instance-variables
  (:readable-instance-variables url read-graph write-graph)
  (:documentation "A FRAME-SOURCE that represents a SPARQL endpoint, optionally with given named graphs for reading and writing.")
  )

;;; Coerce strings to frames and otherwise be reasonable.
(defmethod* initialize-instance :after ((sparql-endpoint sparql-endpoint) &rest ignore)
	    (unless name
	      (setf name url))
	    (when (stringp read-graph)
	      (setf read-graph (make-frame read-graph)))
	    (when (stringp write-graph)
	      (setf write-graph (make-frame write-graph)))
	    (when write-graph
	      (setf writeable? t)))

(defvar *default-sparql-timeout* 30)

(defgeneric do-sparql (source command &key timeout result-format)
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

;;; transplanted from mb branch
(defparameter *sparql-performance-monitor* nil)
(defparameter *sparql-heartbeat-monitor* nil)

(defvar *default-sparql-result-format* :json)

(defmethod do-sparql :around ((sparql t) (command t) &key timeout result-format)
  (declare (ignore timeout result-format))
  (when *sparql-heartbeat-monitor*
    (princ "."))
  (if *sparql-performance-monitor*
      #+:ccl (ccl::report-time command #'(lambda () (call-next-method)))
      #-:ccl (error "Don't know how to time monitoring in this Lisp implementation")
      (call-next-method)))

(defmethod do-sparql ((sparql string) (command t) &key (timeout *default-sparql-timeout*)
		      (result-format *default-sparql-result-format*))
  (do-sparql (make-instance 'sparql-endpoint :url sparql) command :timeout timeout :result-format result-format))

(defmethod do-sparql ((sparql null) (command t) &key (timeout *default-sparql-timeout*)
		      (result-format *default-sparql-result-format*))
  (unless (typep *default-frame-source* 'sparql-endpoint)
    (error "No default SPARQL endpoint defined"))
  (do-sparql *default-frame-source* command :timeout timeout :result-format result-format))

;;; Now will set the source of new frames...which is not always right, but better than nothing
(defmethod* do-sparql ((sparql sparql-endpoint) (command string) &key (timeout *default-sparql-timeout*)
		       (result-format *default-sparql-result-format*))
    (run-sparql url command result-format
	     :make-uri #'(lambda (u) (intern-uri u :source sparql))
	     ;; this suddenly became necessary since I was geting literals back...no idea why 
	     :eager-make-uri? t
	     :timeout timeout
	     ))

;;; Handles translation and breaking up query into chunks if result set is too big
(defmethod* do-sparql ((sparql sparql-endpoint) (query list) &key (timeout *default-sparql-timeout*)
		       (chunk-size 5000) (result-format *default-sparql-result-format*))
  ;;(format t "format = ~s~%" result-format)
  (flet ((do-it ()
	   (do-sparql sparql (generate-sparql sparql query) :timeout timeout :result-format result-format))
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
		     (if from (format nil "~{ FROM ~a ~^~%~}" (mapcar #'sparql-term (mapcar #'make-frame (if (listp from) from (list from))))) " ")
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
	 (assert into nil "No write graph")
	 (with-output-to-string (s) 
	   (format s
		   "INSERT INTO GRAPH ~A { ~A ~A ~A }"
		   (sparql-term into)
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
	 (assert from nil "No write graph")
	 (with-output-to-string (str) 
	   (format str
		   "DELETE FROM GRAPH ~A { ~A ~A ~A }"
		   (sparql-term from)
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
;;; symbols are put out as-is, allowing ie |bif:contains|.  But USUALLY a symbol here is a bug.
(defun sparql-term (thing)
  (typecase thing
    (null (error "NIL in SPARQL"))
    (frame (format nil "<~A>" (frame-uri thing)))
    (symbol
     (if (var-p thing)
	 (string-downcase (string thing))
	 (string thing))) 
    (string (if (or (position #\" thing) (position #\Newline thing))
		(format nil "'''~A'''" (backslash-quote-string thing))
		(format nil "\"~A\"" (backslash-quote-string thing))))
    (fixnum (fast-string thing))
    (single-float (fast-string thing))	;+++ in theory these should be tagged with ^^xsd:double
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

(defun backslash-quote-string (s)
  (string-replace s "\\" "\\\\"))

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
    (rdfs-call post-fill frame))	;+++ should be done at higher level for non-sparql sources
  )

;;; Hack +++
(rdfs-defmethod post-fill (frame)
		)

(defmethod fill-frame-sparql ((frame frame) (source sparql-endpoint))
  (let* ((*default-frame-source* source) ;not sure
	 (results (do-sparql 
		       source
		     `(:select (?p ?o) () 
			       (,frame ?p ?o)))))
    (dolist (binding results)
      (let ((p (sparql-binding-elt binding "p"))
	    (o (sparql-binding-elt binding "o")))
	(when (and p o)			;+++ shouldn't be necessary but some SPARQL endpoints have missing results (dbpedia)
	  (add-triple frame p (process-value p o))))
      )
    (when results
      (set-frame-loaded? frame))
    ))

;;; +++ this can time out without the limit, but of course with it, it produces incorrect results.  Maybe ths should only be done on demand.
(defvar *inverse-fill-limit* 100)

(defmethod fill-frame-inverse-sparql ((frame frame) (source sparql-endpoint))
  (unless (frame-inverse-slots frame)
    (setf (frame-inverse-slots frame) (make-hash-table :test #'eq)))
  (let ((*default-frame-source* source))
    (dolist (binding (do-sparql 
			 source
		       `(:select (?s ?p) (:limit ,*inverse-fill-limit*) (?s ?p ,frame))))
      (let ((p (sparql-binding-elt binding "p"))
	    (s (sparql-binding-elt binding "s")))
	(when (and s p)	; shouldn't be necessary but some SPARQL endpoints have missing results (dbpedia)
	  (add-triple s p frame))
      ))))

;;; Special hack to fill tuplesets efficiently.  Fill-frame is not cutting it.
(defun fill-tupleset (frame)
  (let ((tuples (make-hash-table :test #'eq)))
    (dolist (bindingset
	    (do-sparql (frame-source frame)
	      `(:select * ()
			(,frame #$sw:slots/includes-tuple ?tuple)
			(?tuple ?tprop ?value))))
      (setf (gethash (sparql-binding-elt bindingset '?tuple) tuples) t)
      (setf (ssv (sparql-binding-elt bindingset '?tuple)
		 (sparql-binding-elt bindingset '?tprop))
	    (sparql-binding-elt bindingset '?value)))
    (setf (slotv frame  #$sw:slots/includes-tuple)
	  (hash-keys tuples)
	  (frame-loaded? frame)
	  t)))


;;; Maybe this should be folded into add-triple
(defun process-value (slot value)
  (if (%slotv slot #$sw:specialhandling)
      (rdfs-call deserialize-value slot value)
      value))

(defmethod uri-used? ((source sparql-endpoint) uri)
  (do-sparql 
      source
    `(:select (?s ?p ?o) (:limit 1) (:union (((:uri ,uri) ?p ?o)) ((?s ?p (:uri ,uri)))))))

(defun var-p (thing)
  (and (symbolp thing)
       (char= #\? (char (string thing) 0))))

;;; Rather slow
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

(defun include-labels (vars query &key (property #$rdfs:label) required?)
  #.(doc "Add an optional label (or other PROPERTY) clause to QUERY for each var in VARS."
	 "Labels are returned in the variables VAR_label."
	 "If REQUIRED? is T, property must be present for any result to be returned." )
  (setq query (copy-tree query))
  (dolist (var vars)
    (let* ((label-var (intern (string+ (string var) "_label") :keyword))
	   (clause `(,var ,property ,label-var)))
      (push-end label-var (second query))
      (push-end (if required?
		    clause
		    `(:optional ,clause))
		query)))
  query)

;;; An extension to do this to n levels might be useful (++).
;;; Fill-frame is n=0.  
;;; return-all-results? is not currently used
(defun bulk-load-query (source query &key (var (car (second query))) return-all-results? (inverse? t))
  #.(doc
     "Given a SPARQL query and a VAR extend the query to load all slots and inverse-slots of frames that match VAR, and mark them as loaded."
     "This function actually peforms the query and returns the list of frames matching VAR.")
  (setq query (copy-tree query))	;we mutate query, so copy it first
  (push-end `(:union ((,var ?bl_p ?bl_o))
		     ,@(if inverse? `(((?bl_s ?bl_p ,var)))))
	    query)
  (push-end '?bl_p (second query))
  (push-end '?bl_o (second query))
  (if inverse? (push-end '?bl_s (second query)))
  (let* ((res (do-sparql source query))
	 (processed? nil)
	 (frames
	  (collecting
	    (dolist (bind res)
	      (let ((sm (sparql-binding-elt bind var))
		    (s (and inverse? (sparql-binding-elt bind "bl_s")))
		    (p (sparql-binding-elt bind "bl_p"))
		    (o (sparql-binding-elt bind "bl_o")))
		;; do a reset on frames we bring in
		(unless (member sm processed?)
		  (reset-frame sm)
		  (push sm processed?))
		(when o
		  (add-triple sm p (process-value p o)))
		(when s
		  (add-triple s p sm))
		(collect-new sm)
		(set-frame-loaded? sm)
		(setf (frame-source sm) source)
		(when (and (frame-p o)	;not sure about this, but for now
			   (null (frame-source o)))
		  (setf (frame-source o) source))
		(when (and (frame-p s)	;not sure about this, but for now
			   (null (frame-source s)))
		  (setf (frame-source s) source))
		)))))
    (if return-all-results?
	res 
	frames)))

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
		      (add-triple s slot (process-value slot val)))))
	      slots slot-vars
	  ))))))

(defun augment-query-standard (source query &key (var (car (second query))))
  (augment-query source query :var var :slots (list #$rdf:type #$rdfs:label)))
       
; Find which graph a triple is in (can take vars).  Very useful!    
(defun find-named-graph (source s p o)
  (do-sparql-one-var source `(:select (?g) (:distinct t) (:graph ?g (,s ,p ,o)))))

(defun sparql-type-count (type)
  (parse-integer (cadr (car (car (do-sparql nil `(:select :count () (?s #$rdf:type ,type))))))))

(defun sparql-named-graph-count (graph)
  (cadr (car (car (do-sparql nil `(:select :count () (:graph ,graph (?s ?p ?o))))))))

(defun random-instance (type)
  (car (do-sparql-one-var nil  `(:select * (:limit 1 :offset ,(random (sparql-type-count type))) (?s #$rdf:type ,type)))))
