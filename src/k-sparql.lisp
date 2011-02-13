(in-package :sw)

;;; +=========================================================================+
;;; | Copyright (c) 2009, 2010  Mike Travers and CollabRx, Inc                |
;;; |                                                                         |
;;; | Released under the MIT Open Source License                              |
;;; |   http://www.opensource.org/licenses/mit-license.php                    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  Mike Travers

;;; From Knewos, the lower level sparql parts

;;; Raw SPARQL, return string
(defun run-sparql-0 (endpoint sparql &key timeout (result-format "json"))
  (unless timeout (setf timeout 10000))
  (multiple-value-bind (body response headers)
      (net.aserve::with-timeout-local (timeout (error "SPARQL timeout from ~A" endpoint))
	(net.aserve.client:do-http-request
	    endpoint
	  ;; some sparql servers only accept GET, we should parameterize this (++)
	  :method :post
	  :query `(("query" . ,sparql)
		   ("format" . ,result-format)
		   )))
    (declare (ignore headers))
    (unless (= response 200)
      (error (format nil "SPARQL Error ~A: ~A" response body)))
    body))

;;; some endpoints are giving me preceding nulls which break things.
;;; inefficient, but this shouldn't be needed at all!
(defun adjust-sparql-string (s &optional (limit 25))
  (cond ((char= (char s 0) #\Null) (adjust-sparql-string (subseq s 1) (1- limit)))
	((minusp limit) (error "Bad SPARQL string: ~A" s))
	(t s)))

(defgeneric run-sparql (endpoint sparql result-format &key make-uri eager-make-uri? timeout))

;;; eager-make-uri? causes literals to be converted to URIs if they look like one
(defmethod run-sparql (endpoint sparql (result-format (eql :xml))
			&key (make-uri #'identity) eager-make-uri? timeout)
  (unpack-xml-sparql-result (adjust-sparql-string
			     (run-sparql-0 endpoint sparql :timeout timeout :result-format "xml"))
			    make-uri
			    eager-make-uri?))

(defun unpack-xml-sparql-result (s make-uri eager-make-uri?)
  (let* ((s-xml:*ignore-namespaces* t)
         (xml (s-xml:parse-xml-string s :output-type :lxml))
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
			;; +++ other datatypes
			((eq (car value-elt) ':|bnode|)
			 ;; we do this for bnodes, although it's not really correct -- you could have two colliding. +++
			 (funcall make-uri (string+ "bnode:" (cadr value-elt))))
			(t
			 (cadr value-elt)))))
            (push (list name value) row-result)))
        (push row-result results)))
    (values (nreverse results) vars)))

(defmacro geta (key al) `(cdr (assoc ,key ,al)))

(defmethod run-sparql (endpoint sparql (result-format (eql :json))
		       &key (make-uri #'identity) eager-make-uri? timeout)
  (let ((raw-result (adjust-sparql-string (run-sparql-0 endpoint sparql :timeout timeout :result-format "json"))))
    (if (char= (char raw-result 0) #\<)
	;; oops, looks like XML
	(unpack-xml-sparql-result raw-result make-uri eager-make-uri?)
	(let* ((json (json:decode-json-from-string raw-result))
	       (vars (geta :vars (geta :head json)))
	       (results nil))
	  (dolist (bindings (geta :bindings (geta :results json)))
	    (let ((row-result nil))
	      (dolist (binding bindings)
		;;(format t "b=~a, datatype=~a~%" binding (geta :datatype (cdr binding)))
		(let* ((name (string-downcase (symbol-name (car binding))))
		       (brest (cdr binding))
		       (type (intern (string-upcase (geta :type brest)) (find-package :keyword)))
		       (value-elt (geta :value brest))
		       (value
			(cond ((or (eq type :uri)
				   (string-prefix-equals value-elt "nodeID:")
				   (and eager-make-uri?
					(string-prefix-equals value-elt "http://")))
			       (funcall make-uri value-elt))
			      ((and (eq type :typed-literal)
				    (equal (geta :datatype brest)
					   "http://www.w3.org/2001/XMLSchema#integer"))
			       (parse-integer value-elt))
			      ((and (eq type :typed-literal)
				    (equal (geta :datatype brest)
					   "http://www.w3.org/2001/XMLSchema#double"))
			       (read-from-string value-elt))
			      ;; +++ other datatypes?
			      ((eq type :bnode)
			       ;; we do this for bnodes, although it's not really correct -- you could have two colliding. +++
			       (funcall make-uri (string+ "bnode:" value-elt)))
			      (t value-elt))))
		  (push (list name value) row-result)))
	      (push row-result results)))
	  (values (nreverse results) vars)))))



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
  (run-sparql server "select * where {?s ?p ?o} limit 10" :xml))

(defun servers-alive ()
  (mapcar #'(lambda (server)
              (print server)
              (report-and-ignore-errors
                (print (length (server-alive server)))))
          (all-servers)))

(defun server-statement-count (server)
  (report-and-ignore-errors
    (run-sparql server "select count(*) where {?s ?p ?o}" :xml)))

;;;



