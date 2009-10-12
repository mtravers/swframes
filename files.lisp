(in-package :sw)

(export '(parse-owl-file parse-rdf-xml-file))

;;; You can load frames from files (RDF/OWL)

(defclass* file-frame-source (frame-source) 
  (file)
  :initable-instance-variables)

;;; no-op 
(defmethod fill-frame-from ((frame frame) (source file-frame-source) &key inverse?)
  (declare (ignore frame source inverse?))
  )

(defun parse-rdf-xml-file (file)
  "Parse an RDF/XML file into frames"
  (let ((*default-frame-source* (make-instance 'file-frame-source :file file)))
    (process-rdf-xml (s-xml:parse-xml-file file))))

(defun parse-owl-file (file)
  "Parse an OWL file into frames"
  (parse-rdf-xml-file file))		;for now

(defun owl-file-to-virtuoso (file graph)
  (let ((frames (parse-owl-file file))
	(writer (make-instance 'sparql-endpoint
			       :url (sparql-endpoint-url *default-sparql-endpoint*)
			       :writeable? t
			       :write-graph graph)))
    (dolist (f frames)
      (write-frame f :source writer))))


