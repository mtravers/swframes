(in-package :sw)

;;; An example of how to write something to Virtuoso.

(defun save-history (user session in out)
  (let ((frame (genuri *collabrx-sparql-writeable* (format nil "crx:/bioblog/user/~A/session/~A/history" user session))))
    (add-triple frame #$rdfs:type #$crx:bioblog/Entry)
    (add-triple frame #$crx:bioblog/in in)
    (add-triple frame #$crx:bioblog/out out)
    (add-triple frame #$crx:bioblog/user user)
    (write-frame frame)
    frame))
    
