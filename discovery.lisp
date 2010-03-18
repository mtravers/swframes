(in-package :sw)

(export 'discover-classes)

#|
Still has some problems with the order of class creation.
|#

;;; Automatically generate CLOS classes from SPARQL endpoint.  Too slow.
(defmethod discover-classes ((endpoint sparql-endpoint))
  #.(doc "Query the endpoint for RDFS classes that it contains, and define the corresponding CLOS classes")
  (mapcar #'rdfs-clos-class
	  (do-sparql-one-var endpoint
	    `(:select (?concept) (:distinct t :limit 40)
		      (?x #$rdf:type ?concept)))))





