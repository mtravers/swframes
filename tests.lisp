(in-package :swframes)


(sw-register-namespace "linkedct" "http://data.linkedct.org/resource/linkedct/")
(sw-register-namespace "drugbank" "http://www4.wiwiss.fu-berlin.de/drugbank/")

(defvar *linkedct-sparql* (make-sparql-source "http://data.linkedct.org/sparql"))

(setq *default-frame-source* *linkedct-sparql*)

(pprint
f (setq xx (do-sparql *default-frame-source* `(:select (?s ?p ?o) (:limit 10) (?s ?p ?o) ))))


;;; print trials for Myopia
(pprint
 (do-sparql *default-frame-source*
  '(:select (?trial ?title) ()
    (?trial #$http://data.linkedct.org/resource/linkedct/condition #$http://data.linkedct.org/resource/condition/8512)
    (?trial #$http://data.linkedct.org/resource/linkedct/brief_title ?title))))


;;; find some trials with interventions
(do-sparql 
 *default-frame-source*
 `(:select (?s ?o) (:limit 10) (?s ,(make-frame :uri (expand-uri "linkedct:intervention")) ?o )))


;;; find some drug trials (returns
(do-sparql *linkedct-sparql* 
  `(:select (?trial ?intervention ?drug)
	    (:limit 50)
	    (?trial ,(intern-uri (expand-uri "linkedct:intervention")) ?intervention )
	    (?intervention ,(intern-uri (expand-uri "linkedct:intervention_type")) "Drug" )
	    (?intervention ,(intern-uri (expand-uri "linkedct:intervention_name")) ?drug )
	    ))

(do-sparql *linkedct-sparql*
  `(:select (?trial ?intervention ?drug)
	    (:limit 50)
	    (?trial #$linkedct:intervention ?intervention )
	    (?intervention #$linkedct:intervention_type "Drug" )
	    (?intervention #$linkedct:intervention_name ?drug )
	    ))

;;; find trials for a drug (not too smart) (seems to have stoped working?)
(defun trials-for-drug (drugname)
  (do-sparql *collabrx-sparql* ; *linkedct-sparql*
   `(:select (?trial ?title ?condname) ()
	     (?trial #$linkedct:intervention ?intervention )
	     (?trial #$linkedct:brief_title ?title)
	     (?intervention #$linkedct:intervention_type "Drug" )
	     (?intervention #$linkedct:intervention_name ,drugname )
	     (?trial #$linkedct:condition ?condition)
	     (?condition #$linkedct:condition_name ?condname)
	     )))


(lambda (d)
  (mapcar #'(lambda (b)
	      (list (sw::sparql-binding-elt b "trial")
		    (sw::sparql-binding-elt b "title")))
	  (sw::trials-for-drug (car (slotv d #$drugbank:genericName)))))




ie:
(describe-frame (intern-uri "http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugs/DB00243"))

;;; Weirdly try to find drug synonyms through interventions...
;;; seems to take forever, damn it.  Doesn't seem like it would be that hard a query to run...
;;; even if you give it a nonexistant drugname.  Damn, I don't understand how a query planner could be that dumb.
;;; seems to work somewhat ebetter if you leave off distinct
;;; Works OK on "Lepriuden", times out on "Gleevec".  Sigh.
(defun synonyms-for-drug (drugname)
  (do-sparql *default-frame-source*
	       `(:select (?nname) ()
			 (?i1 ,(intern-uri (expand-uri "linkedct:intervention_name")) ,drugname)
			 (?i1 ,(intern-uri "http://www.w3.org/2002/07/owl#sameAs") ?syn)
			 (?i2 ,(intern-uri "http://www.w3.org/2002/07/owl#sameAs") ?syn)
			 (?i2 ,(intern-uri (expand-uri "linkedct:intervention_name")) ?nname))))


(defun synonyms-for-drug2 (drugname)
  (let ((synonyms
	 (mt:collecting
	   (dolist (trial-res (trials-for-drug drugname))
	     (let ((intervention (sparql-binding-elt trial-res "intervention")))
	       (fill-frame intervention) ;should get done automagically
	       (dolist (syn (slotv intervention (intern-uri "http://www.w3.org/2002/07/owl#sameAs")))
		 (mt::collect-new syn)))))))
    (let ((result nil))
      (dolist (syn synonyms)
	(let ((intervention2 (mapcar #'cadar (sparql-query `(:select (?other) () (?other ,(intern-uri "http://www.w3.org/2002/07/owl#sameAs") ,syn)))))) 
	  (dolist (i intervention2)
	    (fill-frame i)
	    (dolist (elt (slotv i (intern-uri (expand-uri "linkedct:intervention_name"))))
	      (if (not (find elt result :test #'string-equal))
		  (push elt result)))))))))

;;; Diseasome
(setq *default-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/diseasome/sparql"))

;;; Fails due to SQL errror!  
(pprint
 (setq xx (sparql-query `(:select (?s ?p ?o) (:limit 10) (?s ?p ?o) ))))

;;; Drugbank
(setq *default-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/drugbank/sparql"))
(setq *drugbank-frame-source* (make-sparql-source "http://www4.wiwiss.fu-berlin.de/drugbank/sparql"))


;;; ah, this works


;;; Let's replicate some stuff
(defun db-target (gene-name)
 (do-sparql *drugbank-frame-source*
  `(:select (?target) ()
    (?target #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/geneName ,gene-name)
    )
  :server *drugbank-frame-source*))


(defun db-drugs (gene-name)
 (do-sparql *drugbank-frame-source*
  `(:select (?drug ?name ?target) ()
	    (?drug #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/target ?target)
	    (?drug #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/genericName ?name)
	    (?target #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugbank/geneName ,gene-name)
    )
  :server *drugbank-frame-source*))

(run-sparql "http://quebec.bio2rdf.org/sparql"
"SELECT ?s1, ?type1, ?label1, count(*)
WHERE {
?s1 ?p1 ?o1 .
?o1 bif:contains \"HK1\" .
?s1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?type1 .
?s1 <http://www.w3.org/2000/01/rdf-schema#label> ?label1 .
}" 
:make-uri #'intern-uri
:eager-make-uri? t)

;;; Works except results are tagged wrong, so URIs don't get built
(run-sparql "http://quebec.bio2rdf.org/sparql"
"SELECT ?type1, ?label1, count(*)
WHERE {
?s1 ?p1 ?o1 .
?o1 bif:contains \"HK1\" .
?s1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?type1 .
?s1 <http://www.w3.org/2000/01/rdf-schema#label> ?label1 .
}" :make-uri #'intern-uri)



;;; To get error handling: patch this in BioLisp/Weblisten/special-output.lisp
(defmethod out-record-to-html :around ((form t) (string string) &rest ignore)
  (declare (ignore ignore))
  (call-next-method))


;;; test the grid
(let ((wb::*sessionid* :blither)
      (wb::*html-stream* *standard-output*))
  (wb::out-record-to-html (drug-grid "EGFR") "foo"))

;;; test the browser
(let ((wb::*sessionid* :blither)
      (wb::*html-stream* *standard-output*))
  (html-for-browse-frame "http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugs/DB00002"))





;;;; Dereference worker.
(defun wail-on-dereferencer ()
  (for-all-frames (f)
    (print `(dereferencing ,f))
    (report-and-ignore-errors
      (dereference f))))

;;; Get trials for a condition (dereferencing also works)
(swframes::sparql-query '(:select (?s ?p) () (?s ?p #$db:condition/749)))

;;; all experimental drugs (hm, exactly 600, that's suspicious)
(swframes::sparql-query '(:select (?d ?name) () (?d ?p #$drugbank:resource/drugtype/experimental) (?d #$rdfs:label ?name)) :server  *drugbank-frame-source*)

;;; test our local virtuoso
;; fast!
(defvar *collabrx-sparql* (make-sparql-source "http://virtuoso.collabrx.com/sparql/"))

(do-sparql *collabrx-sparql* `(:select (?s ?p ?o) (:limit 10) (?s ?p ?o) ) )
;; not as fast!
(do-sparql *collabrx-sparql* '(:select (?s ?p) () (?s ?p "Melanoma")))

;;; new feature!
(do-sparql *collabrx-sparql* '(:select (?s) () (?s ?p "Melanoma")) :one-var? t)

;;; trials about Myopia
(do-sparql *collabrx-sparql* '(:select (?s) () (?s #$db:linkedct/condition #$db:condition/8512)) :one-var? t)


(do-sparql *collabrx-sparql* '(:select (?s ?p) () (?s ?p "Ranolazine")))

;;; 





(bulk-load *linkedct-sparql* '((?s #$db:linkedct/condition #$db:condition/8512)) )

I'm always doing this, so

(lambda (trial)
  (mapcar (svf #$db:linkedct/intervention_name)
	  (slotv trial #$db:linkedct/intervention)))


(funcall (svf* #$db:linkedct/intervention_name #$db:linkedct/intervention)
	 #$db:trials/NCT00727558)


;;; This is cute...
(wb::frame-grid
 (swframes::bulk-load *collabrx-sparql* '((?s ?pp "Melanoma"))))


