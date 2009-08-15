(in-package :sw)

;;; Freebase doesn't do SPARQL, so here's a start at an MQL interface
;;; based on metaweb.py (/Volumes/revenant/a/projects/freebase/metaweb.py)

;;; Plumbing

(require :cl-json)

(defvar *freebase-host* "www.freebase.com") ; The Metaweb host
(defvar *freebase-readservice* "/api/service/mqlread")   ; Path to mqlread service
;;; cookie = 'metaweb-user=A|u_mt|g_#9202a8c04000641f800000000432d26a|4.q96D6ZQ1xp7Wm+Vmo8cdfA'

(defvar *mql-debug* nil)

(defun mql-read (q &optional credentials)
  (let* ((env2 `((:query . ,(list q)))) ; )   `((:query . ,q))
	 (json (json:encode-json-to-string env2))
	 (args (net.aserve:uriencode-string json))
	 (url (format nil "http://~A~A?query=~A" *freebase-host* *freebase-readservice* args))
	 response)
    (when *mql-debug*
      (terpri)
      (princ json))
    (setq response
	  (json:decode-json-from-string 
					;     (util:get-url url)
					;     (net.aserve.client::do-http-request url)
	   (get-via-curl url)
	   ))
    (unless (equal "/api/status/ok" (utils::assocdr :code response))
      (error "MQL error ~A" response))
    (when *mql-debug*
      (terpri)
      (print response))
    (utils::assocdr :result response)))

(defun mql-read-raw (q &optional credentials)
  (let* ((args (net.aserve:uriencode-string q))
	 (url (format nil "http://~A~A?query=~A" *freebase-host* *freebase-readservice* args)))
    (json:decode-json-from-string
;     (net.aserve.client::do-http-request url :protocol :http/1.0)
     (get-via-curl url)
     )))

;;; there's some damn bug in the client code
(defun get-via-curl (uri)
  (trim-first-line			;argh
  (with-output-to-string (s)
    (let ((*standard-output* s)
	  (asdf::*verbose-out* s))
      (asdf::run-shell-command "curl -s \"~A\"" uri)))))

(defun trim-first-line (s)
  (subseq s (1+ (position #\Newline s))))

;;; Utility queries 

(defun name-types (name)
  (mql-name-property-lookup name "type" nil))

;;; this should be memoized.
(defun type-properties (type)
  (mql-read `((:id . ,type)
	      ("properties" . :empty-list)
	      (:type . "/type/type"))))  

;;; Given a GUID, return everything we can find
;;; I think this is isomorphic to what you get from dereferencing the RDF?
(defun id->everything (id)
  (let ((types
	 (utils:assocdr 
	  :type 
	  (car (mql-read `((:id . ,id)
			   (:type . :empty-list))))))
	(result nil))
    (dolist (type types)
      (setf result 
	    (append result
		    (ignore-errors 	;+++ some types give errors, just ignore
		    (mql-read `((:id . ,id)
				(:type . ,type)
				("*" . (:empty-dict)))))))) ; or ("*" . nil) to get values only
    result))
      
(defun mql-result->frame (id)
  (let ((f (make-frame (expand-uri (string+ "fb:" (substitute #\. #\/ (subseq id 1)))))))
    (setf (frame-source f) nil)		;+++ or have a MQL-specific source object
    f))

(defun mql-term (term)
  (mql-read `(("*" .  ,term))))

(defun links (id)
  (mql-read `((:type . "/type/link")
	      (:source . ((:id . ,id)))
	      (:master_property . nil)
	      (:target . :empty-dict)
	      (:target_value . nil))))

;;; Generalized
;;; this doesn't work, because you need to provide a type or you get errors
;;; Solution: do one query to get all the types, then do the query for each type, with an error handler.  Yuck!
;;; MQL has no OR operator, so this takes 2 separate queries
(defun mql-name-property-lookup (name property &optional type)
  (flet ((do-query (nproperty)
	   (let* ((mql (mql-read
			`((,nproperty . ,name)
			  ,@(if type `((:type . ,type)))
;			  ("*" . (:empty-dict))
			  ;; this info isn't used here, but it's interesting.
			  ("a:name" . nil)
			  ("a:type" . :empty-list)
			  (,property  . :empty-list))))
;; gets the full object, might be useful
;			  ("/base/bioventurist/product/developed_by"  . (:empty-dict)))))
		  ;; Aigh, bad car
		  (dev (mapunion #'(lambda (result)
				     (utils::assocdr (keywordize property) result))
				 mql
				 :test #'equal))
		  )
	     dev)))
    (or (do-query "name")
	(do-query "/common/topic/alias"))))

(defun mql-name-lookup (name &optional type)
  (flet ((do-query (nproperty)
	   (let* ((mql (mql-read
			`((,nproperty . ,name)
			  ,@(if type `((:type . ,type)))
			  (:id . nil)
			  ("a:name" . nil)
			  ("a:type" . :empty-list)
			  ))))
	     mql)))
    (append (do-query "name")
	    (do-query "/common/topic/alias"))))

;;; almost the same as above
(defun mql-name-lookup-wild (name &optional type)
  (flet ((do-query (nproperty)
	   (let* ((mql (mql-read
			;; THIS LINE IS DFFERENT
			`((,(string+ (string nproperty) "~=") . ,name)
			  ,@(if type `((:type . ,type)))
			  (:id . nil)
			  ("a:name" . nil)
			  ("a:type" . :empty-list)
			  ))))
	     mql)))
    (append (do-query "name")
	    (do-query "/common/topic/alias"))))

;;; RDF/Frame support


;;; Turns a Freebase ID into a frame name (ie, duplicating what they do to go to RDF)
(sw-register-namespace "fb" "http://rdf.freebase.com/ns/")




;;;; Bio specific
(defun mql-gene (gene-id)
  (let* ((raw (mql-read `(("/biology/gene/symbol" . ,gene-id) (:id  . nil))))
	 (id (assocdr :id (car raw)))
	 (frame (and id (mql-result->frame id))))
    (when frame
      (fill-frame frame)			;optional
      frame)))

(defun mql-drug-mfr (drugname)
  (assert drugname)			;nil causes problems
  (mql-name-property-lookup 
   drugname
   "/base/bioventurist/product/developed_by"
   "/medicine/drug"))

#|
for demo:


(mapcar #'(lambda (res) 
	    (sw::mql-result->frame (cdr (assoc :id res))))
	(sw::mql-name-lookup (#^drugbank:drugbank/genericName it)))







|#



