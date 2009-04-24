;;; Code to parse drugbank files into frames

;;;; MODIFIED (lightly) FOR SW

#|
Import works up until GO threading, where it craps out.

To do:
- more URI-like names (done)
- set loaded flag  (done)
- deal with inverses (needs thinking)
- write out (waiting on virtuoso)
- use in grid (working)
- get rid of temp frames! (done?)
|#

(in-package :bio)

(export '(show-drugs-for-target))

(defvar *drugbank-drugcards-file* (cl-user:translate-simple-lp "bioetc:data;drugbank;drugcards.txt"))
(defvar *drugbank-frames* nil)          ;KKK flush
(sw:sw-register-namespace "crxdb" "http://collabrx.com/drugbank/")

; (parse-drugcards *drugbank-drugcards-file*)

;;; Links
;;; DrugCard main page: http://www.drugbank.ca/drugs/DB06288
;;; structure image: http://129.128.185.122/drugbank2/drugs/DB06288/structure_image

#|
Todo:
- collapse identical targets (DONE)
  - but at cost of losing the refs to pubmed evidence, might want to reintroduce that.
- deal with links (ie wikipedia) (done, but not working)
|#

(def-inverse-slot #$DB.drugs.s #$DB.targets.s)

;;; not working? I'm not sure how slot inhertance works, and I wrote it.
(defslot #$DB.Wikipedia_Link.s :base #$fbrowser.URLvalue)
(defslot #$DB.RxList_Link.s :base #$fbrowser.URLvalue)

(defun create-db-frame-name (string &optional cat)
  (sw::string+ "http://collabrx.com/drugbank/" cat (frames::create-valid-frame-name string :space-char-action #\_)))

(defun db-uri (name &optional (cat ""))
  (sw::uri (create-db-frame-name name cat)))

(defun db-slot-uri (name)
  (db-uri name "slot/"))

(defun parse-drugcards (file &optional (limit nil))
  (let ((sw:*default-frame-source* :drugbank-file-import) ;+++ object for this, like kdb objects
	(sw:*mark-new-frames-loaded?* t))
    (dolist (f *drugbank-frames*)
      (sw:delete-frame f))
    (setq *drugbank-frames* nil)
    (with-open-file (in file :external-format :latin1)
      (do ((line (read-line in nil :eof) (read-line in nil :eof))
	   (frame nil) slot-name slot (good-name nil) target-frames)
	  ((or (eq line :eof)
	       (and limit (> (length *drugbank-frames*) limit))))
	(cond ((= 0 (length line)))
	      ((eql (aref line 0) #\#)
	       (cond ((eql (aref line 1) #\Space)
		      (setq slot-name (subseq line 2 (1- (length line))))
		      (setq slot (db-slot-uri slot-name)))
		     ((equal (aref line 1) #\B)
		      ;; finalize old frame?
		      (let ((name (car (last (string-split line)))))
			(setq frame (db-uri name)
			      good-name nil)
;;; causing problems?
					;                     (delete-frame-contents frame) ;clear out any old stuff
					;                      (setf (#^isA frame) (list (db-uri "Drug"))
			(setf (#^rdfs:subtypeOf frame) (list (db-uri "Drug")))
			(setf target-frames (make-hash-table))
			(pushnew frame *drugbank-frames*)
			))))
	      ;; omit boring
	      ((equal line "Not Available"))
	      ((and (not good-name) (eq slot (db-slot-uri "Generic_Name")))
	       (report-and-ignore-errors ;there are some apparent duplicates, this will deal with them if not well.
		(rename-frame frame (create-db-frame-name line))
		(setq good-name t))
	       ;; do normal add as well.
	       (push line (frame-slot-value frame slot))
	       )

	      ((equal slot (db-slot-uri "Drug_Category"))
	       (let ((cat (db-uri line)))
		 (sw:add-triple frame #$rdfs:subtypeOf cat)
		 ;; make the hierarchy neater (I think this leaves the inverse relationship in place, or something like that)
		 (delete-element frame #$rdfs:subtypeOf (db-uri "Drug"))
		 (sw:add-triple cat #$rdfs:subtypeOf (db-uri "Drug"))
		 (pushnew cat *drugbank-frames*)))

	      ;; unpack targets
	      ((equal (subseq slot-name 0 (min 11 (length slot-name))) "Drug_Target")
	       (let* ((substrings (string-split slot-name #\_))
		      (index (read-from-string (nth 2 substrings)))
		      (target-frame (gethash index target-frames))
		      (sub-slot-name (string-join (subseq substrings 3) #\_))
		      (sub-slot (db-slot-uri sub-slot-name))
		      (idling nil))
		 (when (null target-frame)
		   (setf (gethash index target-frames)
			 (setf target-frame (db-uri (string (gensym "temp")))))
		   ;; causing problems
		   ;; (delete-frame-contents target-frame)
		   (sw:add-triple target-frame #$rdfs:subtypeOf (db-uri "Target"))
		   (push target-frame *drugbank-frames*)
		   (sw:add-triple frame (db-slot-uri "targets") target-frame))
		 ;; this logic assumes that when a target is mentioned in multiple places the description is really the same (that is, that 
		 ;; drugcards.txt is a denormalized view of a normalized database).
		 (unless idling
		   (if (eq sub-slot (db-slot-uri "Name"))
		       (let ((name (create-db-frame-name line)))
			 (if (sw::frame-named name)
			     (progn (delete-element frame (db-slot-uri "targets") target-frame)
				    (sw::delete-frame target-frame)
				    (deletef target-frame *drugbank-frames*)
				    (setf (gethash index target-frames)
					  (setf target-frame (sw::frame-named name)))
				    (sw:add-triple frame (db-slot-uri "targets") target-frame)
				    (setf idling t))
			     (rename-frame target-frame name))))
		   (sw:add-triple target-frame sub-slot line :test #'equal))))

	      ;; thread KEGG
	      ((eq slot (db-slot-uri "KEGG_Drug_ID"))
	       (setf (frame-slot-value frame (db-slot-uri "KEGG_Drug"))
		     (frame-fnamed (create-valid-frame-name line :prefix "KEGG."))))
	      ((eq slot (db-slot-uri "KEGG_Compound_ID.s"))
	       (setf (frame-slot-value frame (db-slot-uri "KEGG_Compound"))
		     (frame-fnamed (create-valid-frame-name line :prefix "KEGG."))))

	      (t
	       (push line (frame-slot-value frame slot))))))
; temporarily not working +++
;;;    (thread-go)
    ))


;;; This makes thread-go fast
(def-indexed-slot #$GO.prettyname :test #'equal)

(defun thread-go ()
  (for-all-frames (f)
    (let* ((v (frame-slot-value f (db-slot-uri "GO_Classification")))
           (nv (mapcar #'(lambda (s)
                           (let* ((colon (and (stringp s) (position #\: s)))
                                  (name (and colon (wb::space-trim (subseq s (1+ colon)))))
                                  (frame (and name (slot-lookup name #$GO.prettyname))))
                             (or frame s)))
                       v)))
      (when nv
	(setf (frame-slot-value f (db-slot-uri "GO_Classification"))
	      nv))
      )))

;;; Hook into kdb system

(defclass drugbank-kdb (kdb)
  ())

(defvar *drugbank-kdb* (make-instance 'drugbank-kdb :name :drugbank))

(defmethod kdb-toplevel-frames ((kdb drugbank-kdb))
  (list (db-uri "Drug") (db-uri "Target")))

;;; KDB generates these symbols. Someone doesn't understand OOP.
;;; Not getting called on load. Blah.
;;; Also, slow.  Needs to be profiled.
(defmethod kdb-make ((kdb drugbank-kdb) &key redo? verbose?)
  (declare (ignore redo? verbose?))     ;+++
  (parse-drugcards *drugbank-drugcards-file*)
  (setf (kdb-frames kdb) (all-drugbank-frames))
)

(defmethod kdb-instantiate ((kdb drugbank-kdb))
  (setq *drugbank-frames*
        (all-drugbank-frames)))


(defun all-drugbank-frames ()
  *drugbank-frames*)

;;; KKK use KB object
(defvar *db-drugs* nil)


(defun db-drugs ()
  (or *db-drugs*
      (setf *db-drugs*
            (mt:filter #'(lambda (f)
			   (slotv f (db-slot-uri "Generic_Name")))
		    (all-drugbank-frames)))))

(defun experimental-db-drugs ()
  (filter #'(lambda (f)
                 (let ((types (slotv f (db-slot-uri "Drug_Type") f)))
                   (and (or (member "Investigational" types :test #'string-equal)
                            (member "Experimental" types :test #'string-equal))
                        (not (member "Approved" types :test #'string-equal)))))
             *db-drugs*))

(def-indexed-slot (db-slot-uri "Gene_Name"))
(def-indexed-slot (db-slot-uri "Synonyms"))
(def-indexed-slot (db-slot-uri "Generic_Name"))
(def-indexed-slot (db-slot-uri "Brand_Names"))

(defun db-lookup-target (target-name)
  (union (slot-lookup target-name (db-slot-uri "Synonyms"))
         (slot-lookup target-name (db-slot-uri "Gene_Name"))))

(defun db-drugs-for-target (target)
  (sw::slotv-inverse target (db-slot-uri "targets")))

(defun drugs-for-target (target-name)
  (mapunion #'db-drugs-for-target (db-lookup-target target-name)))


(defun db-lookup-drug (drug-name)
  (union (slot-lookup drug-name (db-slot-uri "Synonyms"))
         (union (slot-lookup drug-name (db-slot-uri "Generic_Name"))
                (slot-lookup drug-name (db-slot-uri "Brand_Names")))))

;;; inverse of the above
(defun db-drug-names (frame)
  (remove-duplicates
   (append (frame-slot-value frame (db-slot-uri "Synonyms"))
           (frame-slot-value frame (db-slot-uri "Generic_Name"))
           (frame-slot-value frame (db-slot-uri "Brand_Names")))
   :test #'string-equal))


#|
Apparently target descriptions are identical across drugs EXCEPT for the
 #$DB.Drug_References
Which makes sense...those are effectively aassociation properties, which for the moment we are ignoring,
Although maybe it would make sense to include them...so the value of targets would be a list of (target pubmed-1 pubmed-2...) lists

;;; this is never multi-valued -- but does it exist for all targets?
(for-all-frames (f)
  (if (> (length (frame-slot-value f #$DB.Gene_Name.s)) 1)
      (print f)))

(dolist (f (#^subClasses #$DB.Target))
  (unless (= (length (frame-slot-value f #$DB.Gene_Name.s)) 1)
      (print f)))

;;; all types.  I suppose these should be classes.
;("Investigational" "Illicit" "Nutraceutical" "Withdrawn" "Experimental" "Biotech" "Small Molecule" "Approved")

;;; good call to generate a table


 CDH1 is a gene from the melanoma demo



does not appear in DB or KEGG (our version, but
  http://www.genome.ad.jp/dbget-bin/www_bget?hsa:999


(dolist (e (experimental-db-drugs))
    (when (and (member #$DB.AntineoplasticAgents (#^isA e)))
      (print (list e (mapcar #'(lambda (target) (#^DB.Gene_Name target)) (#^DB.targets e))))))


|#
