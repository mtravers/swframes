(in-package :swframes)

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

;;; was colliding with s-xml.  Actually, why don't I just reuse it, it's about the same code.  
(defvar *sw-namespaces* nil)

(defun register-namespace (abbrev full &optional force?)
  "Register ABBREV as a namespace definition for FULL.  Ie, (register-namespace \"dbpedia\" \"http://dbpedia.org/property/\")"
  (assert (stringp abbrev))
  (assert (or (stringp full)
	      (keywordp full)))
  (aif (member abbrev *sw-namespaces* :key #'car :test #'string-equal)
       (unless (equal (cadr (car it)) full)
	 (if force?
	     (progn (deletef abbrev *sw-namespaces* :key #'car :test #'string-equal)
		    (push (list abbrev full) *sw-namespaces*))
	     (error "Attempt to redefine namespace ~A from ~A to ~A" abbrev (cadr (car it)) full)
	     ))
       (push (list abbrev full) *sw-namespaces*)))

(defun unregister-namespace (abbrev)
  (deletef abbrev *sw-namespaces* :test #'equal :key #'car))

(defmacro def-namespace (abbrev full)
  "A version of register-namespace for use in code files; does an eval-when so frame names will be read correctly."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-namespace ,abbrev ,full)))

;;; Return namespace and remainder
(defun namespacify (uri)
  (dolist (namespace *sw-namespaces*)
    (let ((full (cadr namespace)))
      (when (and (not (eq full :uri-scheme))
		 (>= (length uri) (length full))
		 (string= uri full :end1 (length full)))
	(return-from namespacify (values (car namespace)
					 (subseq uri (length full)))))))
  (values nil uri))
  

(defun abbreviate-uri (uri)
  "Given a full URI as a string, attempt to abbreviate it using known namespaces."
  (multiple-value-bind (namespace rest)
      (namespacify uri)
    (if namespace
	(values (format nil "~A:~A" namespace rest)
		namespace)
	(values uri nil))))

;;; takes care of a corner case that arises in practice, not sure if 
(defun namespace-splice (prefix suffix)
  (if (and (> (length suffix) 0)
	   (char= #\# (char suffix 0))
	   (char= #\# (char prefix (1- (length prefix)))))
      (string+ prefix (subseq suffix 1))
      (string+ prefix suffix)))

(defvar *namespace-lenient* nil)

(defun frame-namespace (frame)
  (namespacify (frame-uri frame)))

(defun expand-uri (uri &optional (no-error? *namespace-lenient*))
  "Given an abbreviated URI as a string, expand it using known namespaces.  An error is signalled if the namespace is unknown unless NO-ERROR? is true."
  (let* ((colonpos (position #\: uri))
	 (prefix (and colonpos
		      (subseq uri 0 colonpos)))
	 (namespace (namespace-expand prefix))
	 (suffix (if prefix (subseq uri (1+ colonpos)) uri)))
    (cond 
      ((null prefix) uri)
      ((null namespace)
       (if no-error?
	   uri
	   (error "Unknown namespace ~A in ~A" prefix uri)))
      ((eq namespace :uri-scheme)
       uri)
      (t (namespace-splice namespace suffix)))))

(defun namespace-lookup (namespace)
  (find namespace *sw-namespaces* :key #'car :test #'string-equal))

(defun namespace-expand (namespace)
  (cadr (namespace-lookup namespace)))

(defun expand-uri-0 (ns string)
  (let ((namespace (namespace-lookup ns)))
    (if namespace
	(string+ (cadr namespace) string)
	(error "No namespace ~A" ns))))

;;; Define known real (non-abbreviated) schemas
(register-namespace "http" :uri-scheme)
(register-namespace "https" :uri-scheme)
(register-namespace "urn" :uri-scheme)
(register-namespace "bnode" :uri-scheme) ;for rdf-file parsing
(register-namespace "nodeID" :uri-scheme) ;+++ temp: for Virtuoso blank nodes

;;; Generate headers for SPARQL (not currently used)
(defun sparql-namespace-prefix ()
  (format nil "~:{~%PREFIX ~A: <~A>~}" *sw-namespaces*))
