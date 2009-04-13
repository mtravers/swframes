;;; Freebase doesn't do SPARQL, so here's a start at an MQL interface
;;; based on metaweb.py (/Volumes/revenant/a/projects/freebase/metaweb.py)

(require :cl-json)

(defvar *freebase-host* "www.freebase.com") ; The Metaweb host
(defvar *freebase-readservice* "/api/service/mqlread")   ; Path to mqlread service
;;; cookie = 'metaweb-user=A|u_mt|g_#9202a8c04000641f800000000432d26a|4.q96D6ZQ1xp7Wm+Vmo8cdfA'

(defun mql-read (q &optional credentials)
  (let* ((env2 `((:query . (,q))))
	 (json (json:encode-json-to-string env2))
	 (args (net.aserve:uriencode-string json))
	 (url (format nil "http://~A~A?query=~A" *freebase-host* *freebase-readservice* args))
	 results)
    (terpri)
    (princ json)
    (setq results
	  (json:decode-json-from-string 
					;     (util:get-url url)
					;     (net.aserve.client::do-http-request url)
	   (get-via-curl url)
	   ))
    results))

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

OK, utterly broken:

? (json->lisp "{'query': {'name': 'fred'}}")
((:QUERY (:NAME . "fred")))
? (lisp->json *)
"[[\"query\",[\"name\",\"r\",\"e\",\"d\"]]]"
? 


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

;;; this is better (er no, just seems to return admin stuff)
(mql-read  '(("*" . "Gleevec") ("*" . nil)))

;;; an error, not sure why.
(mql-read  '(("*" . "Gleevec")))

;;; this works nice...er no returns 100 random drugs
(mql-read  '(("*" . "Gleevec") ("*" . nil) (:type . "/medicine/drug")))

;;; returns nothing
(mql-read  '((:name . "Gleevec") ("*" . nil) (:type . "/medicine/drug")))

;;; this works well...
(mql-read  '(("/common/topic/alias" . "Gleevec") ("*" . nil) (:type . "/medicine/drug")))

;;;;

New version of cl-json has different, smaller set of bugs.
(in /misc/sourceforge/cl-json/ )

|#
