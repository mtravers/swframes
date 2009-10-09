(in-package :sw)

(defun d-then-e (s)
  (let* ((decoded (json->lisp s))
	 (encoded (string-replace  (lisp->json decoded) "\"" "'")))
    (print decoded)
    (unless (equal s encoded)
      (print `(differs ,s ,encoded)))))
	

from ~/.sbcl/site/cl-json_0.3.1/src/encoder.lisp   -- the real one seems broken
;;; exp
(defmethod encode-json ((s list) stream)
  (if (listp (car s))
      (encode-json-alist s stream)
      (call-next-method s stream)))

This case loses
(d-then-e "{'a': {'b': 'fred'}}")

But this is OK..
(d-then-e "{'a': {'b': 23}}")

;;; +++ Should test out json extensions :empty-list and :empty-dict

;;; Try to understand their weird encoding...
(defun json->lisp (s)
  (json:decode-json-from-string
   (string-replace 
    (string-replace s "'" "\"")
    "None" "null")))

