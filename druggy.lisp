(in-package :swframes)

;;; Enough to get drug grid going.

(setq *default-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/drugbank/sparql"))
(setq *drugbank-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/drugbank/sparql"))
(sw-register-namespace "drugbank" "http://www4.wiwiss.fu-berlin.de/drugbank/")

(defun db-target (gene-name)
 (swframes::sparql-query
  `(:select (?target) ()
    (?target #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/geneName ,gene-name)
    )
  :server *drugbank-frame-source*))


(defun db-drugs (gene-name)
 (swframes::sparql-query
  `(:select (?drug ?name ?target) ()
	    (?drug #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/target ?target)
	    (?drug #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/genericName ?name)
	    (?target #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/geneName ,gene-name)
    )
  :server *drugbank-frame-source*))

(defun drug-grid (target-gene)
  (wb::frame-grid (mapcar #'(lambda (bindings)
			      (sparql-binding-elt bindings "drug"))
			  (db-drugs target-gene))
		  nil
;;	      (list #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/genericName
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/synonym
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/description
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/target
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/drugType
		    ))
