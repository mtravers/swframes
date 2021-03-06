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


(export '(dump-frames
	  dump-frames-to-fasl
	  nt-writer))

;;; Not a source really, a sink.  Maybe need to refine these classes (and/or integrate with tuplesets) ++
(defclass* bulk-out (frame-source)
  ())

(defclass* file-bulk-out (bulk-out)
  (file stream)
  (:initable-instance-variables file))

(defgeneric dump-frames (source frames)
  (:documentation "Write out a set of frames to the SOURCE (actually a sink)."))

(defmethod* dump-frames ((source file-bulk-out) frames)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (setf stream s)
    (dolist (f frames)
      (frame-dump source f))))

(defmethod frame-dump ((source bulk-out) frame)
  (for-frame-slots (frame slot value)
		   (dolist (elt value)
		     (write-triple source frame slot elt))))

;;; NTriples writer 
(defclass* nt-writer (file-bulk-out) ()
	   (:documentation "Source (sink) for writing out frames in Ntriples format (not to be confused with n3)"))

(defmethod* write-triple ((out nt-writer) s p o &key write-graph)
  (declare (ignore write-graph))
  (terpri stream)
  (write-entity out s)
  (write-char #\Space stream)
  (write-entity out p)
  (write-char #\Space stream)
  (write-entity out o)
  (write-char #\Space stream)
  (write-char #\. stream))
	  
(defmethod* write-entity ((out nt-writer) (thing frame))
  (format stream "<~A>" (frame-uri thing)))

;;; +++ prob inadequate type handling
(defmethod* write-entity ((out nt-writer) (thing t))
  (unless (or (typep thing 'string) (typep thing 'number))
    (prin1 (fast-string thing) stream)))

;;; given a set of frames, finds references to frames outside the set
;;; for now, does not process predicates or inverse links (++ add those as options)
;;; Not called at present
(defun frameset-external-refs (frames)
  (collecting
    (dolist (f frames)
      (for-frame-slots (f slot value)
		       (dolist (elt value)
			 (if (and (frame-p elt)
				  (not (member elt frames)))
			     (collect-new elt)))))))
			      
;;; Fasl dump
(defvar *fasl-dump-temp*)
(defun dump-frames-to-fasl (frames file &key (variable '*fasl-dump-temp*))
  (set variable frames)
  (dump-vars-to-file (list variable) file))

(defun big-dump-frames-to-fasl (frames dir &key (limit 500) (variable '*fasl-dump-temp*))
  (let ((counter 0))
    (dolist (sublist (break-list frames limit))
      (dump-frames-to-fasl sublist (make-pathname :directory dir :name (fast-string (incf counter))) :variable variable)
      (print `(dump ,counter)))))
    
;;; Supports fasl dump -- see swframes-0 for the make-load-form machinery

(defmethod$ slot-load-form (frame (slot #$sw:slots/TransientSlot) value)
  (declare (ignore frame value))
  nil)
