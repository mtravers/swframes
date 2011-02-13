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


;;; Note: this has been pulled out of sparql machinery, some further cleanup may be required.

(defclass$ #$rdf:Property ())

(defclass$ #$sw:slots/specialSlot (#$rdf:Property))

;;; special write behaviors:  don't write, serialize/deserialize lisp, list handling...

(defun declare-special-slot (slot type)
  #.(doc 
     "Declares SLOT to have special behavior defined by TYPE.  Current TYPEs are:"
     "#$sw:slots/LispValueSlot:"
     "   Slots of this class can hold any printable Lisp object."
     "#$sw:slots/TransientSlot:"
     "  Slots of this class never write their values to the database.")
  (setf (ssv slot #$rdf:type) type
        (ssv slot #$sw:specialhandling) t) ;CCC +++ if we methodize slot functions this flag may be able to go away
  (classify-frame slot))

;;; debugging only
(defun undeclare-special-slot (slot)
  (setf (slotv slot #$rdf:type) nil
        (slotv slot #$sw:specialhandling) nil)  )

;;; LispValueSlot

(defclass$ #$sw:slots/LispValueSlot (#$sw:slots/specialSlot))

(defmethod$ deserialize-value ((slot #$sw:slots/LispValueSlot) value)
		(if (stringp value)
		    (read-from-string value)
		    value))

;;; TransientSlot

(defclass$ #$sw:slots/TransientSlot (#$sw:slots/specialSlot))

;;; Sometimes these unserializable slots get serialized, so ignore them
(defmethod$ deserialize-slot ((p #$sw:slots/TransientSlot) frame value)
		(declare (ignore frame value))
		nil)

(defmethod$ deserialize-value ((p #$sw:slots/TransientSlot) value)
		(declare (ignore value))
		nil)


