(in-package :sw)

;;; An example of how to write something to Virtuoso.

(defun save-history (user session in out)
  (let ((frame (genuri *collabrx-sparql-writeable* (format nil "crx:/bioblog/~A/~A/history" user session))))
    (add-triple frame #$rdfs:type #$crx:bioblog/Entry)
    (add-triple frame #$crx:bioblog/in in)
    (add-triple frame #$crx:bioblog/out out)
    (add-triple frame #$crx:bioblog/user user)
    (write-frame frame)
    frame))



(rdfs-def-class #$crx:bioblog/Entry ()
		(#$crx:bioblog/in)
		(#$crx:bioblog/out)
		(#$crx:bioblog/user :range #$crx:bioblog/Entry))
	
    
(rdfs-make-instance #$crx:bioblog/Entry
		    #$crx:bioblog/in in
		     #$crx:bioblog/out out
		     #$crx:bioblog/user user)
		    


