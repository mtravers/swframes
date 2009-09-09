(in-package :sw)

;;; From Knewos, the lower level sparql parts

;;; Raw SPARQL, return string
(defun run-sparql-0 (endpoint sparql &key timeout)
  (unless timeout (setf timeout 10000))
  (multiple-value-bind (body response headers)
      (net.aserve::with-timeout-local (timeout (error "SPARQL timeout from ~A" endpoint))
	(utils:get-url endpoint
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
(defun adjust-sparql-string (s)
  (if (char= #\< (char s 0))
      s
      (adjust-sparql-string (subseq s 1))))

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
                  (if (or (eq (car value-elt) ':|uri|)
                          ;; virtuoso hands back these as <literals>, but they act like URIs
                          ;; actually, now they are <bnode>s...should handle those specially (optional arg to uri constructor maybe) +++
                          (utils::string-prefix-equals (cadr value-elt) "nodeID:")
                          (and eager-make-uri?
                               (utils::string-prefix-equals (cadr value-elt) "http://")))
                      (funcall make-uri (cadr value-elt))
                      (cadr value-elt))))
            (push (list name value) row-result)))
        (push-end row-result results)))
    (values results vars)))

;;; Tuplized version
(defun run-sparql-tuples (endpoint sparql &key (make-uri #'identity))
  (let* ((s-xml:*ignore-namespaces* t)
         (xml (s-xml:parse-xml-string
               (run-sparql-0 endpoint sparql)
               :output-type :lxml))
         (vars (mapcar #'(lambda (var-elt)
                           (lxml-attribute var-elt :|name|))
                       (lxml-subelements (lxml-find-element xml ':|head|) t))))
;    (print vars)                       ;+++ pass to tuple set
    (making-tuple-set
     (dolist (result (lxml-subelements (lxml-find-element xml ':|results|) t))
       (let ((row-tuple (make-tuple)))
         (dolist (binding (lxml-subelements result))
           (let* ((name (lxml-attribute binding :|name|))
                  (value-elt (car (lxml-subelements binding)))
                  (value
                   (if (eq (car value-elt) ':|uri|)
                       (funcall make-uri (car (lxml-subelements value-elt)))
                       (car (lxml-subelements value-elt)))))
             (setf (tuple-field row-tuple name) value))))))))



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

;;; New set based

;;; makes use of sw:sparql-endpoints

(defclass* sparql-result-set (query-tuple-set)
  (endpoint
   query
   (fields nil)
   (count nil))
  (:initable-instance-variables endpoint query)
  )

(defmethod* tset-fields ((tset sparql-result-set))
  (or fields
      (multiple-value-bind (result nfields)
          (k-do-sparql tset (modified-query tset :options '(:limit 1)))
        (setf fields nfields))))


(defmethod* k-do-sparql ((tset sparql-result-set) nquery &key (timeout *sparql-default-timeout*))
  (k-do-sparql endpoint nquery :timeout timeout))

(defmethod* maybe-init-query ((tset sparql-result-set))
  (unless count
    (setf count
          (parse-integer
           (cadr (caar
                  (do-sparql tset (modified-query tset :vars '("count(*)")))))))
    (print `(,count records found))))

(defmethod* modified-query ((tset sparql-result-set) &key vars options)
  (let ((nquery (copy-tree query)))
    (when options
      (setf (third nquery)
            (append (third nquery) options)))
    (when vars
      (setf (second nquery) vars))
    nquery))

(defmethod* tset-count ((tset sparql-result-set))
  (maybe-init-query tset)
  count)

(defmethod* tset-subseq ((tset sparql-result-set) start length)
  (maybe-init-query tset)
  (multiple-value-bind (result field-results)
      (do-sparql
          tset
        (modified-query tset :options `(:offset ,start :limit ,length)))
    (unless fields
      (setf fields field-results))
    (sparql-results->tuples result)
    ))

;;; +++ make fields something more appropriate
(defun sparql-results->tuples (results)
  (dolist (r results)
    (dolist (i r)
      (rplacd i (cadr i))))
  results)

