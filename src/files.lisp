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

(export '(parse-owl-file parse-rdf-xml-file))

;;; You can load frames from files (RDF/OWL)

(defclass* file-frame-source (frame-source) 
  (file)
  :initable-instance-variables)

;;; no-op 
(defmethod fill-frame-from ((frame frame) (source file-frame-source))
  (declare (ignore frame source))
  )

(defun parse-rdf-xml-file (file)
  "Parse an RDF/XML file into frames"
  (process-rdf-xml (s-xml:parse-xml-file file) :source (make-instance 'file-frame-source :file file)))

(defun parse-owl-file (file)
  "Parse an OWL file into frames"
  (parse-rdf-xml-file file))		;for now

(defun owl-file-to-virtuoso (file graph &key (source *default-frame-source*))
  (let ((frames (parse-owl-file file))
	(writer (make-instance 'sparql-endpoint
			       :url (sparql-endpoint-url source)
			       :writeable? t
			       :write-graph graph)))
    (dolist (f frames)
      (write-frame f :source writer))))


