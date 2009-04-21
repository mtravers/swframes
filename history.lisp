(in-package :sw)

;;; An example of how to write something to Virtuoso.


(rdfs-def-class #$crx:bioblog/Entry ()
		(#$crx:bioblog/in)
		(#$crx:bioblog/out)
		(#$crx:bioblog/user :range #$crx:bioblog/Entry)
		(#$crx:bioblog/time)
		)
	
    
(rdfs-def-class #$crs:bioblog/User ()
		(#$crx:bioblog/uname)
		(#$crx:bioblog/fullname)
		(#$crx:bioblog/password))

(defun make-history-entry (user in out)
  (rdfs-make-instance #$crx:bioblog/Entry
		      #$crx:bioblog/user user
		      #$crx:bioblog/in in
		      #$crx:bioblog/out out))
		    


#|

(defvar *collabrx-main*
  (make-instance 'sparql-endpoint
		 :uri "http://sparql.collabrx.com/sparql/"
		 :writeable? t
		 :write-graph "http://collabrx.com/main"))

(setq mt (rdfs-make-instance #$crs:bioblog/User 
			     #$crx:bioblog/uname "mt"
			     #$crx:bioblog/fullname "Michael Travers"
			     #$crx:bioblog/password "mumblefrotz"))

(write-frame mt *collabrx-main*)

(setq e1 (make-history-entry mt "(+ 2 2)" 4))

(write-frame e1 *collabrx-main*)

			     

|#
