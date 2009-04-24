(in-package :sw)

;;; Useful functions and snippets for demos

(defun local-term-query (term)
  (utils:filter-out 
   #'bnode?
   (do-sparql-one-var *collabrx-sparql*
     `(:select (?s) (:distinct t) (?s ?p ,term)))))

;;;
(lambda (trial) (sw::msv (#^linkedct:reference trial) #$linkedct:citation))

(lambda (trial) (#^linkedct:citation (#^linkedct:reference trial) ))
