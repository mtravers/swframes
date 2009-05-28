;;; Tests
#|


(defvar *collabrx-sparql-writeable*
  (make-instance 'sparql-endpoint
		 :uri "http://sparql.collabrx.com/sparql/"
		 :writeable? t
		 :write-graph "http://collabrx.com/my_graph"))

(test2 *collabrx-sparql-writeable*)

(defmethod test3 ((sparql sparql-endpoint))
  (let* ((frame (genuri sparql "http://collabrx.com/testing/"))
	 (uri (frame-uri frame)))
    (setf (slotv frame #$foo) '(23))
    (setf (slotv frame #$bar) '(#$foo))
    (write-frame sparql frame)
    (print `(frame ,frame written))
    (delete-frame frame)			;clear it out
    (setf frame (intern-uri uri))
    (assert (null (slotv frame #$foo)))
    (fill-frame-sparql frame sparql)	;+++ this API should change
    (print (slotv frame #$foo))
    (assert (member "23" (slotv frame #$foo) :test #'equal))
    (print `(frame ,frame read back))
    ))

|#
