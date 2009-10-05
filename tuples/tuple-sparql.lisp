(in-package :sw)

;;; New set based

;;; makes use of sw:sparql-endpoints

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
     (dolist (result (lxml-subelements (lxml-find-element xml ':|results|) t))
       (let ((row-tuple (make-tuple)))
         (dolist (binding (lxml-subelements result))
           (let* ((name (lxml-attribute binding :|name|))
                  (value-elt (car (lxml-subelements binding)))
                  (value
                   (if (eq (car value-elt) ':|uri|)
                       (funcall make-uri (car (lxml-subelements value-elt)))
                       (car (lxml-subelements value-elt)))))
             (setf (tuple-field row-tuple name) value)))))))

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
