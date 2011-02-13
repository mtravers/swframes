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

(export 'discover-classes)

#|
Still has some problems with the order of class creation.
|#

;;; Automatically generate CLOS classes from SPARQL endpoint.  Too slow.
;;; Clauses can be used to restrict the query, necessary in some cases for performance
;;; PPP slow because it fills each frame independently 
;;; +++ try using bulk-loader
;;; LIMIT interacts badly with FILL using :TYPE-OBJECT (limits the binding-sets returned, not the classes).
(defmethod discover-classes ((endpoint sparql-endpoint) &key limit clauses from (method :type-object) fill?)
  #.(doc "Query the endpoint for RDFS classes that it contains, and define the corresponding CLOS classes")
  (mapc #'rdfs-clos-class
	(ecase method
	  (:type-object
	   (funcall (if fill? 'bulk-load-query 'do-sparql-one-var) endpoint
	     `(:select (?concept) (:distinct t :limit ,limit :from ,from)
		       (?x #$rdf:type ?concept)
		       ,@clauses)))
	  (:class-typed
	   (rdfs-find :all :source endpoint :class #$rdfs:Class :fill? fill?))
	  )))





