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


;;; Some standard namespaces
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *standard-namespaces*
    '(;; SemWeb infrastructure
      ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
      ("xsd" "http://www.w3.org/2001/XMLSchema#")
      ("owl" "http://www.w3.org/2002/07/owl#")

      ;; Common schemas
      ("dc" "http://purl.org/dc/terms/")
      ("foaf" "http://xmlns.com/foaf/0.1/")
      ("skos" "http://www.w3.org/2004/02/skos/core#")

      ;; Local
      ("sw" "http://swframes.org/rdf/")	;+++ well, who should own this?
      ))

(dolist (n *standard-namespaces*)
  (register-namespace (car n) (cadr n) t))

)
