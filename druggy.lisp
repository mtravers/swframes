(in-package :swframes)

;;; Enough to get drug grid going, and other domain-specific code

(setq *default-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/drugbank/sparql"))
(defvar *drugbank-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/drugbank/sparql"))
(defvar *linkedct-frame-source* (make-sparql-source "http://data.linkedct.org/sparql"))
(defvar *diseasome-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/diseasome/sparql"))
(defvar *collabrx-sparql* (make-sparql-source "http://virtuoso.collabrx.com/sparql/"))

(defun trials-for-condition (condition)
  (do-sparql *linkedct-frame-source*
    `(:select (?trial ?title) ()
      (?trial #$http://data.linkedct.org/resource/linkedct/condition ?condition)
      (?condition #$linkedct:condition_name ,condition)
      (?trial #$http://data.linkedct.org/resource/linkedct/brief_title ?title))))

(defun sparql-result->grid (results binding)
  (wb::frame-grid
   (extract-sparql-binding results binding)))


;;; find trials for a drug 
;;; times out now, not sure why...this should be a fairly straightforward query.
(defun trials-for-drug (drugname)
  (do-sparql *linkedct-frame-source* ; *collabrx-sparql* ; 
   `(:select (?trial ?title ?condname) ()
	     (?intervention #$linkedct:intervention_type "Drug" )
	     (?intervention #$linkedct:intervention_name ,drugname )
	     (?trial #$linkedct:intervention ?intervention )
	     (?trial #$linkedct:brief_title ?title)
	     (?trial #$linkedct:condition ?condition)
	     (?condition #$linkedct:condition_name ?condname)
	     )))

;;; stripped down (doesn't help, still times out)
(defun trials-for-drug (drugname)
  (do-sparql *collabrx-sparql* ; *linkedct-frame-source* 
   `(:select (?trial) ()
	     (?intervention #$linkedct:intervention_type "Drug" )
	     (?intervention #$linkedct:intervention_name ,drugname )
	     (?trial #$linkedct:intervention ?intervention )
	     )))

(defun db-target (gene-name)
 (do-sparql *drugbank-frame-source*
  `(:select (?target) ()
    (?target #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/geneName ,gene-name)
    )))

;;; warning: case-sensitive.
(defun db-drugs (gene-name)
 (do-sparql *drugbank-frame-source*
  `(:select (?drug ?name ?target) ()
	    (?drug #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/target ?target)
	    (?drug #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/genericName ?name)
	    (?target #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/geneName ,gene-name)
    )))

(defun drug-grid (target-gene)
  (wb::frame-grid (mapcar #'(lambda (bindings)
			      (sparql-binding-elt bindings "drug"))
			  (db-drugs target-gene))
		  nil
		  ;; better to put these in through the UI?
;;	      (list #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/genericName
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/synonym
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/description
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/target
;; 		    #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/drugType
		    ))
 
