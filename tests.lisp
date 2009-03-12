(in-package :swframes)


(sw-register-namespace "linkedct" "http://data.linkedct.org/resource/linkedct/")
(sw-register-namespace "drugbank" "http://www4.wiwiss.fu-berlin.de/drugbank/")

(setq *default-frame-source* (make-sparql-source "http://data.linkedct.org/sparql"))

(pprint
 (setq xx (sparql-query `(:select (?s ?p ?o) (:limit 10) (?s ?p ?o) ))))


;;; print trials for Myopia
(pprint
 (swframes::sparql-query
  '(:select (?trial ?title) ()
    (?trial #$http://data.linkedct.org/resource/linkedct/condition #$http://data.linkedct.org/resource/condition/8512)
    (?trial #$http://data.linkedct.org/resource/linkedct/brief_title ?title))))


;;; all classes


;;; find some trials with interventions
(run-sparql 
 *default-frame-source*
 (generate-sparql
  `(:select (?s ?o) (:limit 10) (?s ,(make-frame :uri (expand-uri "linkedct:intervention")) ?o ))))

(generate-sparql
  `(:select (?s ?p ?o) (:limit 30) (?s ?p ?o )))


;;; find some drug trials
 (sparql-query 
  `(:select (?trial ?intervention ?drug)
	    (:limit 50)
	    (?trial ,(intern-uri (expand-uri "linkedct:intervention")) ?intervention )
	    (?intervention ,(intern-uri (expand-uri "linkedct:intervention_type")) "Drug" )
	    (?intervention ,(intern-uri (expand-uri "linkedct:intervention_name")) ?drug )
	    ))

 (sparql-query 
  `(:select (?trial ?intervention ?drug)
	    (:limit 50)
	    (?trial #$linkedct:intervention ?intervention )
	    (?intervention #$linkedct:intervention_type "Drug" )
	    (?intervention #$linkedct:intervention_name ?drug )
	    ))

(sparql-query 
  `(:select (?trial ?intervention ?drug)
	    (:limit 50)
	    (?trial #$linkedct:intervention ?intervention )
	    (?intervention #$linkedct:intervention_type "Drug" )
	    (?intervention #$linkedct:intervention_name ?drug )
	    ))

;;; find trials for a drug (not too smart)
(defun trials-for-drug (drugname)
  (sparql-query
   `(:select (?trial ?title ?condname) ()
	     (?trial #$linkedct:intervention ?intervention )
	     (?trial #$linkedct:brief_title ?title)
	     (?intervention #$linkedct:intervention_type "Drug" )
	     (?intervention #$linkedct:intervention_name ,drugname )
	     (?trial #$linkedct:condition ?condition)
	     (?condition #$linkedct:condition_name ?condname)
	     )))

;;; looking for synonyms through drugbank link
;;; Whoops, looks like drugbank refs are in here, but not data...perhaps a different sparql endpoint?

ie:
(describe-sframe (intern-uri "http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugs/DB00243"))

;;; Weirdly try to find drug synonyms through interventions...
;;; seems to take forever, damn it.  Doesn't seem like it would be that hard a query to run...
;;; even if you give it a nonexistant drugname.  Damn, I don't understand how a query planner could be that dumb.
;;; seems to work somewhat ebetter if you leave off distinct
;;; Works OK on "Lepriuden", times out on "Gleevec".  Sigh.
(defun synonyms-for-drug (drugname)
  (run-sparql *default-frame-source*
	      (generate-sparql 
	       `(:select (?nname) ()
			 (?i1 ,(intern-uri (expand-uri "linkedct:intervention_name")) ,drugname)
			 (?i1 ,(intern-uri "http://www.w3.org/2002/07/owl#sameAs") ?syn)
			 (?i2 ,(intern-uri "http://www.w3.org/2002/07/owl#sameAs") ?syn)
			 (?i2 ,(intern-uri (expand-uri "linkedct:intervention_name")) ?nname)))))


;;; This works, b
(defun synonyms-for-drug2 (drugname)
  (let ((synonyms
	 (mt:collecting
	   (dolist (trial-res (trials-for-drug drugname))
	     (let ((intervention (sparql-binding-elt trial-res "intervention")))
	       (fill-sframe intervention) ;should get done automagically
	       (dolist (syn (slotv intervention (intern-uri "http://www.w3.org/2002/07/owl#sameAs")))
		 (mt::collect-new syn)))))))
    (let ((result nil))
      (dolist (syn synonyms)
	(let ((intervention2 (mapcar #'cadar (sparql-query `(:select (?other) () (?other ,(intern-uri "http://www.w3.org/2002/07/owl#sameAs") ,syn)))))) 
	  (dolist (i intervention2)
	    (fill-sframe i)
	    (dolist (elt (slotv i (intern-uri (expand-uri "linkedct:intervention_name"))))
	      (if (not (find elt result :test #'string-equal))
		  (push elt result)))))))))


