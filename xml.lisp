(in-package :swframes)

;;; For Laszlo (so not currently used)

(net.aserve:publish :path "/frame.xml"
		    :function 'frame-xml)

(defun frame-xml (req ent)
  (wb::with-http-response-and-body (req ent)
    (let* ((uri (net.aserve::request-query-value "uri" req))
	   (frame (frame-named uri))
	   (xml (xml-dump frame)))
      (s-xml:print-xml xml :stream wb::*html-stream*))))

;; Test
;; curl 'http://localhost:8002/frame.xml?uri=http://data.linkedct.org/resource/trials/NCT00000102'

(defmethod xml-dump ((frame frame))
  (fill-frame frame)
  `(:|frame-xml|
     ,@(collecting
	(maphash #'(lambda (key val)
		     (if (listp val) (setq val (car val))) ;+++ not dealing with multiple value properly yet
		     (collect
			 `(:|property|
			    (:|predicate| ,(frame-uri key))
			    (:|predicateLabel| ,(frame-label key))
			    (:|uri| ,(if (frame-p val) (frame-uri val) ""))
			    (:|value| ,(stringy val))))) 
		 (frame-slots frame)))))

(defun stringy (val)
  (typecase val
    (frame (frame-label val))
    (t (utils:fast-string val))))

(net.aserve:publish :path "frame.xml"
		    :function 'frame-xml)

      
  
