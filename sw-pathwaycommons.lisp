(in-package :sw)

;;; uses the older code base, eventually it should be unified here

(defclass pathwaycommons-frame-source (frame-source) ())

;;; a singleton, although there could in theory be other sites running the PC software
(defvar *pathwaycommons-frame-source* (make-instance 'pathwaycommons-frame-source))

(defun pc-gene-id (gene-name)
; (or (car (slot-lookup gene-name #$PC.gene_symbol.s))
  (let ((rec (block exit
	       (dolist (hit (mt:lxml-all-subelements (bio::pc-search gene-name)))
		 (dolist (syn (mt:lxml-subelements hit :|synonym|))
		   ;; these equality tests are overly restrictive, ie a search for GAGE2 only returns GAGE-2 so no hit comes back +++s
		   (when (string-equal gene-name (cadr syn))
		     (return-from exit hit)))
		 (dolist (id (mt:lxml-find-elements-with-tag hit ':|id|))
		   (when (string-equal gene-name (cadr id))
		     (return-from exit hit)))))))
       (when rec
	 (caddr (car rec)))))

;;; nope, some URIs use CPATH-LOCAL-n, some use CPATH-n.  Fuck!
;;; they are different, so this is wrong.
(defmethod fill-frame-from ((frame frame) (source pathwaycommons-frame-source))
  (let* ((tag (uri-tag (frame-uri frame)))
	 (cpath-id (car (last (utils::string-split (frame-uri frame) #\-))))
	 (filled (pc-entity-frame cpath-id)))
    (print `(,frame ,filled))
    (assert (eq frame filled))
    frame))

;;; always does a retrieval, this should go into a fill-frame method
(defun pc-gene-frame (gene-name)
  (let* ((cpath-id (pc-gene-id gene-name)))
    (pc-entity-frame cpath-id)))
	 
(defun pc-entity-frame (cpath-id)
  (let ((biopax (pc-get-record (bio::coerce-int cpath-id)))
	(*default-frame-source* *pathwaycommons-frame-source*))
    (car (process-rdf-xml biopax))))

(defun pc-get-record (cpath-id)
  (parse-xml
   (bio::pc-query
    :cmd "get_record_by_cpath_id"
    :q cpath-id
    :output :biopax)))
	
;;; often doesn't work, times out
(defun pc-get-neighbors (cpath-id &optional (format :biopax))
  (ecase format
    ;; Detailed Biopax XML
    (:biopax
     (s-xml:parse-xml-string
      (bio::pc-query
       :cmd "get_neighbors"
       :q cpath-id
       :output :biopax)
      :handle-errors :no-result ))
    (:ids
     (mapcar #'second
             (pc-lispify-tab-results
              (pc-query
               :cmd "get_neighbors"
               :q cpath-id
               :output :id_list)
              )))
    ;; list of triples (cpath_id interaction_type cpath_id)
    (:sif
     (lispify-sif
      (pc-query
       :cmd "get_neighbors"
       :q cpath-id
       :output :binary_sif)))))

(defun pc-get-neighbors (cpath-id)
  (mapcar #'pc-get-record
	  (bio::pc-get-neighbors cpath-id)))



#|
Tests

(describe-frame (pc-gene-frame "EGFR"))
|#
