(in-package :sw)

(export 'discover-classes)

#|
Still has some problems with the order of class creation.
|#

;;; Automatically generate CLOS classes from SPARQL endpoint.  Too slow.
;;; Clauses can be used to restrict the query, necessary in some cases for performance
;;; PPP slow because it fills each frame independently +++ try using bulk-loader
(defmethod discover-classes ((endpoint sparql-endpoint) &key limit clauses from (method :type-object) fill?)
  #.(doc "Query the endpoint for RDFS classes that it contains, and define the corresponding CLOS classes")
  (mapc #'rdfs-clos-class
	(ecase method
	  (:type-object
	   (if fill? (error "not yet"))
	   (do-sparql-one-var endpoint
	     `(:select (?concept) (:distinct t :limit ,limit :from ,from)
		       (?x #$rdf:type ?concept)
		       ,@clauses)))
	  (:class-typed
	   (rdfs-find :all :source endpoint :class #$rdfs:Class :fill? fill?))
	  )))





