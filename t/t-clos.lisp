(in-package :sw)

(define-test basic-clos
    (rdfs-def-class #$crx:BigClass ())
  (rdfs-def-class #$crx:SmallClass  (#$crx:BigClass))
  (let* ((ib (rdfs-make-instance #$crx:BigClass))
	 (is (rdfs-make-instance #$crx:SmallClass))
	 (random-frame (gen-test-frame)))
    ;; test that frames have appropriate clos classes and the classes have the right relationship
    (assert-true (not (eq (type-of ib) (type-of random-frame))))
    (assert-true (not (eq (type-of ib) (type-of is))))
    (assert-true (typep is (type-of ib)))
    (assert-true (typep is (type-of random-frame)))))

;;; depends on CRX database.
(define-test rdfs-find-classify 
    (let ((patients (rdfs-find :all :class #$crx:Patient)))
      (assert-true (typep (car patients) ':|http://collabrx.com/rdf/Patient|))))

;;; depends on drugbank database
(define-test discover-basic
  (let ((classes (discover-classes *drugbank-frame-source*))
	(a-frame (car (do-sparql-one-var *drugbank-frame-source* '(:select (?s) () (?s ?p "Aspirin"))))))
    (fill-frame a-frame)
    (assert-true (member (class-of a-frame) classes))))

;;;  test that subclass relations are set up properly on discover
;;; depends on rdfabout
;;; +++ not working yet because of class finalization issues.  Works if you do
;;;  (defclass :|http://xmlns.com/foaf/0.1/Organization| () ())
(define-test discover-subclasses
  (let* ((*namespace-leniant* t)	;necessary because there are some hinky results
	 (source (make-instance 'sparql-endpoint :url "http://www.rdfabout.com/sparql"))
	 (classes (discover-classes source))
	 (instance
	  (car (rdfs-find :all :class #$http://www.rdfabout.com/rdf/schema/usgovt/Village :source source :limit 1))))
    ;; Check that instance is a member not only of Village but also one of its superclasses
    (assert-true (typep instance (class-name (rdfs-clos-class #$http://www.rdfabout.com/rdf/schema/politico/Organization))))))
    
    
    


