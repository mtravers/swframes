(in-package :sw)

;;; Freebase doesn't do SPARQL, so here's a start at an MQL interface
;;; based on metaweb.py (/Volumes/revenant/a/projects/freebase/metaweb.py)

;;; get the darcs version of cl-json, which has better bugs
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


;;; there's some damn bug in the client code
(defun get-via-curl (uri)
  (trim-first-line			;argh
  (with-output-to-string (s)
    (let ((*standard-output* s)
	  (asdf::*verbose-out* s))
      (asdf::run-shell-command "curl -s \"~A\"" uri)))))

(defun trim-first-line (s)
  (subseq s (1+ (position #\Newline s))))

(defun mql-read-raw (q &optional credentials)
  (let* (
	 (args (net.aserve:uriencode-string q))
	 (url (format nil "http://~A~A?query=~A" *freebase-host* *freebase-readservice* args)))
    (json:decode-json-from-string
;     (net.aserve.client::do-http-request url :protocol :http/1.0)
     (get-via-curl url)
     )))

;;; Try to understand their weird encoding...
(defun json->lisp (s)
  (json:decode-json-from-string
   (mt:string-replace 
    (mt:string-replace s "'" "\"")
    "None" "null")))

(defun lisp->json (l)
  (json:encode-json-to-string l))

#|

Tests and experimentation

(defun d-then-e (s)
  (let* ((decoded (json->lisp s))
	 (encoded     (mt:string-replace  (lisp->json decoded) "\"" "'")))
    (print decoded)
    (unless (equal s encoded)
      (print `(differs ,s ,encoded)))))
	

from ~/.sbcl/site/cl-json_0.3.1/src/encoder.lisp   -- the real one seems broken
;;; exp
(defmethod encode-json((s list) stream)
  (if (listp (car s))
      (encode-json-alist s stream)
      (call-next-method s stream)))

This case loses
(d-then-e "{'a': {'b': 'fred'}}")

But this is OK..
(d-then-e "{'a': {'b': 23}}")


(setq album-query "{ 'type': '/music/artist',
          'name': 'Pink Floyd',
          'album': [{ 'name': None,         
                      'release_date': None,
                      'sort': 'release_date' }]}}")


;;; THIS ACTUALLY WORKS DO NOT CHANGE IT (from curl, not lisp at the moment)
;;; looks like bug in client, NET.ASERVE.CLIENT::GET-HEADER-LINE-BUFFER is showing garbage chars..
;;; 
(setq album-query-env "{'query': [{ 'type': '/music/artist',
          'name': 'Pink Floyd',
          'album': [{ 'name': null,         
                      'release_date': null,
                      'sort': 'release_date' }]}]}")

(mql-read-raw (mt:string-replace album-query-env "'" "\""))



;;; NOTE: need to have null


(setq pf-query
      '((:TYPE . "/music/artist")
	(:NAME . "Pink Floyd")
	(:ALBUM ((:NAME) 
		 (:RELEASE_DATE)
		 (:SORT . "release_date")))))

(mql-read pf-query)

(setq pf-query-w-envelope
      '((:QUERY ((:TYPE . "/music/artist") (:NAME . "Pink Floyd") (:ALBUM ((:NAME) (:RELEASE_DATE) (:SORT . "release_date")))))))


(mql-read  '((:name . "Retuximab")))

;;; everything about something
(mql-read  '((:name . "Pink Floyd") ("*" . nil)))

;;; works
(mql-read  '((:name . "Pink Floyd") ("*" . nil) (:type . "/music/musical_group")))

;;; try to expand property...doesn't work, I have no idea 
(mql-read  '((:name . "Pink Floyd") ("*" . (nil)) (:type . "/music/musical_group")))

;;; Works with yet another patch to cl-json
(mql-read  '((:name . "Pink Floyd") ("*" . (:empty-dict)) (:type . "/music/musical_group")))

;;; this is better (er no, just seems to return admin stuff)
(mql-read  '(("*" . "Gleevec") ("*" . nil)))

;;; an error, not sure why.
(mql-read  '(("*" . "Gleevec")))
;;; this too
(mql-read  '(("*" . "Gleevec") (:type . "/medicine/drug")))

;;; this works nice...er no returns 100 random drugs
(mql-read  '(("*" . "Gleevec") ("*" . nil) (:type . "/medicine/drug")))



;;; returns nothing
(mql-read  '((:name . "Gleevec") ("*" . nil) (:type . "/medicine/drug")))

;;; this works well...
(mql-read  '(("/common/topic/alias" . "Gleevec") ("*" . nil) (:type . "/medicine/drug")))

;;; uses an extr property on a different type (the only way I could get the name of this was by looking at the RDF!)
(mql-read  '(("/common/topic/alias" . "Gleevec") ("*" . nil) (:type . "/medicine/drug") ("/base/bioventurist/product/developed_by"  . nil)))


;;; nothing
(mql-read  '(("/common/topic/alias" . "Asprin") ("*" . nil) (:type . "/medicine/drug")))

;;; have to do it this way (there must be an OR)
(mql-read  '(("name" . "Asprin") ("*" . nil) (:type . "/medicine/drug")))


New version of cl-json has different, smaller set of bugs.
(in /misc/sourceforge/cl-json/ )

|#

(defun mql-drug-mfr (drugname)
  (mql-name-property-lookup 
   drugname
   "/base/bioventurist/product/developed_by"
   "/medicine/drug"))

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

;;; examples
#|
(name-property-lookup "2001" "directed_by" "/film/film")
(name-property-lookup "Lisp" "type" nil) ;type can be nil because "type" is a common property?

;;; get ids for everything called 2001 (77!)
(name-property-lookup "2001" "id")
(name-property-lookup "Stanley Kubrick" "id") ;more reasonable

 |#
(defun name-types (name)
  (name-property-lookup name "type" nil))

;;; this should be memoized.
(defun type-properties (type)
  (mql-read `((:id . ,type)
	      ("properties" . :empty-list)
	      (:type . "/type/type"))))  

;;; Given a GUID, return everything we can find
;;; 
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
		    (mql-read `((:id . ,id)
				(:type . ,type)
				("*" . nil))))))
    result))
      
