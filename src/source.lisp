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



#|
A frame source is a sparql endpoint or other source of frames and content.

Dereferencing is a "frame source" of sorts...

|#

(export '(frame-source writeable? with-frame-source *default-frame-source*))

;;; set later on
(defvar *default-frame-source* nil
  "A FRAME-SOURCE used by default when frames are created.  Can by dynamically bound.")	

(defclass* frame-source ()
  ((name nil)
   (writeable? nil))
;  (:abstract t)
  :initable-instance-variables
  (:documentation "A CLOS object that represents a source of frame information; possibly writeable as well.  This is an abstract class"))

(defmethod* print-object ((sparql frame-source) stream)
  (format stream "#<~A ~A ~A>" 
	  (type-of sparql)
	  (or name "?")
	  (if writeable? "[w]" "[nw]")
	  ))

(defgeneric writeable? (frame-source)
  (:documentation "True if the source is capable of being written to."))
  
(defmethod uri-used? ((source frame-source) uri)
  (frame-named (expand-uri uri)))

(defmethod do-write-group ((source null) async? proc)
  (do-write-group *default-frame-source* async? proc))

(defmethod do-write-group ((source frame-source) async? proc)
  (if async?
      (background-funcall proc)
      (funcall proc)))

(defmacro with-frame-source ((source) &body body)
  "Execute BODY with *default-frame-source* set to SOURCE"
  `(let ((*default-frame-source* ,source))
     ,@body))
	 
(defmacro with-write-group ((&optional (endpoint '*default-frame-source*) &key async?) &body body)
  `(do-write-group ,endpoint ,async?
     #'(lambda ()
	 ,@body)))

#|
;;; Not used (but probably should be +++) 

(defclass frame-generation-mixin (frame-source)
  ((uri-base :initarg :uri-base :initform nil)))

;;; Generate a guaranteed unique new URI
(defgeneric gensym-uri (frame-source &optional prefix))
|#
