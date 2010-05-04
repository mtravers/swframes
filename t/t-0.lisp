(in-package :sw)

;;; Setup code for tests (we are assuming/hoping the name of this file will ensure it runs first)

(setf *default-frame-source*
      (make-instance 'sparql-endpoint 
		     :url "http://sparql.collabrx.com/sparql"
		     :write-graph #$http://collabrx.com/g/test))
