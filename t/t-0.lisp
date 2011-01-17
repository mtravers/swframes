(in-package :sw)

;;; Setup code for tests (we are assuming/hoping the name of this file will ensure it runs first)

;;; Nice if we have a writeable sparql store set up.  To get one, try Open Virtuoso
;;;    http://virtuoso.openlinksw.com/dataspace/dav/wiki/Main/

(print "Note: many of these tests require a SPARQL endpoint, in some cases a writeable one")

;;; in the meantime, try something public.
(setf *default-frame-source*
      (make-instance 'sparql-endpoint 
		     :url "http://sparql.neurocommons.org/sparql"
		     ))


