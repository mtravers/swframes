(in-package :sw)

(define-test basic-mql
    (assert-true 
     (mql-read  '(("/common/topic/alias" . "Gleevec") ("*" . nil) (:type . "/medicine/drug")))))

