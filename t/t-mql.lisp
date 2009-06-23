#|

Tests and experimentation

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
   "/medicine/drug")


 
;;; Suitablt for spreadsheet
;;; Frame version would be nice.
  (sw::mql-name-property-lookup 
   (#^crxdb:slot/Generic_Name it)
   "/base/bioventurist/product/developed_by"
   "/medicine/drug")

(reverse (sw::frame-label it))


;;; examples
#|

(name-property-lookup "2001" "directed_by" "/film/film")
(name-property-lookup "Lisp" "type" nil) ;type can be nil because "type" is a common property?

;;; get ids for everything called 2001 (77!)
(name-property-lookup "2001" "id")
(name-property-lookup "Stanley Kubrick" "id") ;more reasonable

(mql-name-lookup-wild "Marx")
(mql-name-lookup-wild "Marx" "/book/author")

|#
