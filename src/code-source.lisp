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

;;; Singleton class to represent frames defined in code

(defclass code-source (frame-source) 
  ())

(defvar *code-source* (make-instance 'code-source))

;;; Done in memory, so nothing more to do
(defmethod delete-triple ((source code-source) s p o &key write-graph)
  (declare (ignore s p o write-graph))
  )

;;; +++ perhaps not necessary now that we specialize %write-triple?
(defmethod write-triple ((source code-source) s p o &key write-graph)
  (declare (ignore s p o write-graph))
  )

(defmethod %write-triple ((source code-source) s p o &key write-graph)
  (declare (ignore s p o write-graph))
  )

(defmethod make-load-form ((source code-source) &optional env)
  `(or *code-source*
       (setf *code-source* (make-instance 'code-source))))

;;; class FRAME not defined yet.
(defmethod fill-frame-from (frame (source code-source))
  (declare (ignore frame source))
  )
