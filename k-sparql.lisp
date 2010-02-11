(in-package :sw)

;;; From Knewos, the lower level sparql parts

;;; Raw SPARQL, return string
(defun run-sparql-0 (endpoint sparql &key timeout)
  (unless timeout (setf timeout 10000))
  (multiple-value-bind (body response headers)
      (net.aserve::with-timeout-local (timeout (error "SPARQL timeout from ~A" endpoint))
	(get-url endpoint
		 ;; some sparql servers only accept GET, we should parameterize this (+++)
		 :method :post
					;       :accept '("application/rdf+xml")
		 :query `(("query" . ,sparql)
                                        ; more efficient probably, but requires a JSON parser
                                        ;            ("format" . "json")
			  )))
    (declare (ignore headers))
    (unless (= response 200)
      (error (format nil "SPARQL Error ~A: ~A" response body)))
    body))

;;; some endpoints are giving me preceding nulls which break things.
;;; inefficient, but this shouldn't be needed at all!
(defun adjust-sparql-string (s &optional (limit 25))
  (cond ((char= #\< (char s 0))
	 s)
	((minusp limit)
	 (error "Bad XML string: ~A" s))
	(t (adjust-sparql-string (subseq s 1) (1- limit)))))

;;; eager-make-uri? causes literals to be converted to URIs if they look like one
(defun run-sparql (endpoint sparql &key (make-uri #'identity) eager-make-uri? timeout)
  (let* ((s-xml:*ignore-namespaces* t)
         (xml (s-xml:parse-xml-string
               (adjust-sparql-string (run-sparql-0 endpoint sparql :timeout timeout))
               :output-type :lxml))
         (vars (mapcar #'(lambda (var-elt)
                           (lxml-attribute var-elt :|name|))
                       (lxml-subelements (lxml-find-element xml ':|head|) t)))
         (results nil))
    (dolist (result (lxml-subelements (lxml-find-element xml ':|results|) t))
      (let ((row-result nil))
        (dolist (binding (lxml-subelements result t))
          (let* ((name (lxml-attribute binding :|name|))
                 (value-elt (car (lxml-subelements binding t)))
                 (value
                  (cond ((or (eq (car value-elt) ':|uri|)
			     (string-prefix-equals (cadr value-elt) "nodeID:")
			     (and eager-make-uri?
				  (string-prefix-equals (cadr value-elt) "http://")))
			 (funcall make-uri (cadr value-elt)))
			((and (eq (lxml-tag value-elt) ':|literal|)
			      (equal (lxml-attribute value-elt :|datatype|)
				     "http://www.w3.org/2001/XMLSchema#integer"))
			 (parse-integer (cadr value-elt)))
			((and (eq (lxml-tag value-elt) ':|literal|)
			      (equal (lxml-attribute value-elt :|datatype|)
				     "http://www.w3.org/2001/XMLSchema#double"))
			 (read-from-string (cadr value-elt)))
			;; +++ other datatypes?
			((eq (car value-elt) ':|bnode|)
			 ;; we do this for bnodes, although it's not really correct -- you could have two colliding. +++
			 (funcall make-uri (string+ "bnode:" (cadr value-elt))))
			(t
			 (cadr value-elt)))))
            (push (list name value) row-result)))
        (push row-result results)))
    (values (nreverse results) vars)))





(defvar *nc-sparql-server* "http://sparql.neurocommons.org:8890/sparql/")
(defvar *cog-sparql-server* "http://knewos:8890/sparql/")
(defvar *ec2-virtuoso-sparql-server* "http://ec2-knewos:8890/sparql/")
(defvar *ec2-agraph-sparql-server* "http://ec2-knewos:8870/sparql") ;NOTE -- trailing slash prohibited.
(defvar *sparql-server* *nc-sparql-server*)

(defun all-servers ()
  (list *nc-sparql-server*
        *cog-sparql-server*
        *ec2-virtuoso-sparql-server*
        *ec2-agraph-sparql-server*))

(defun server-alive (server)
  (run-sparql server "select * where {?s ?p ?o} limit 10"))

(defun servers-alive ()
  (mapcar #'(lambda (server)
              (print server)
              (report-and-ignore-errors
                (print (length (server-alive server)))))
          (all-servers)))

(defun server-statement-count (server)
  (report-and-ignore-errors
    (run-sparql server "select count(*) where {?s ?p ?o}")))

;;;



