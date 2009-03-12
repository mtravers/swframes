;;; Not yet working (or loaded)

(in-package :wb)

(defun frame-grid (frames &optional slots row-limit)
  #.(one-string-nl
     "Output a table where the rows are FRAMES and columns are SLOTS."
     "If SLOTS is missing, use the slots of the first frame.")
  ;; Include error checks because otherwise you can get errors in the weblistener redisplay, which fucks things up rather badly.
  (dolist (frame frames)
    (assert (swframes::frame-p frame)))
  (dolist (slot slots)
    (assert (or (swframes::frame-p slot)
		(functionp slot)
		(and (symbolp slot) (fboundp slot))
		(and (listp slot)
		     (or (framep (car slot))
			 (functionp (car slot))
			 (and (symbolp (car slot)) (fboundp (car slot)))))
		)))
  ;; Could do something smarter like take the union or intersection of all slots
  (if (null slots)
      (setq slots (delete #$fname (frame-slots (car frames)))))
  (make-frame-grid :frames frames :slots slots :row-limit row-limit))

(defmethod out-record-to-html ((grid frame-grid) (string string) &rest ignore)
  (declare (ignore ignore))
  (let ((slots (frame-grid-slots grid))
        (frames (frame-grid-frames grid))
	(id (session-persist-object grid)))
    (with-frame-printing-context
      (labels ((slot-property (slotd key)
		 (and (listp slotd)
		      (cadr (member key (cdr slotd)))))
	       (real-slot (slotdef)
		 (if (listp slotdef) (car slotdef) slotdef))
	       (column-header (slot &optional label?)
		 (let ((rslot (real-slot slot))
		       (header (slot-property slot :header))
		       (sortable? (slot-property slot :sortable?)))
		   (html (:th (unless label?
				(if header
				    (html (:b (:princ header)) :br)
				    (html (:b (frame::emit-value rslot)) :br)))
			      (if (and (frame-grid-sorting? grid) ;was commented out, not sure why
				       (column-sortable? grid rslot))
				  (html ((:a :href (eval-link `(frame-grid-sort ,(ref-to-current-output) ,rslot :up)) 
					     :title "Sort up")
					 "U")
					"&nbsp;"
					((:a :href (eval-link `(frame-grid-sort ,(ref-to-current-output) ,rslot :down))
					     :title "Sort down")
					 "D")
					"&nbsp;"
					))
			      (html ((:a :href (eval-link `(frame-grid-export-column (session-persisted-object ,id) ,rslot))
					 :title "Export column to list")
				     "E")
				    (unless label?
				      (html
					"&nbsp;"
					((:a :href (eval-link `(frame-grid-delete-column ,(ref-to-current-output) ,rslot))
					     :title "Delete column")
					 "X")))
				    (when label?
				      (html "&nbsp;"
					    ((:a :href (format nil "/frame-grid-export.txt?id=~A"
							       id)
						 :title "Export entire table")
					     "Export")))
				    ))))))

	(html
	  ((:table :border 1 :cellpadding 3 :cellspacing 0  :rules :all)
	   (:tr
	    (column-header 'identity t)	
	    (dolist (slot slots)
	      (column-header slot)))
	   ;; add slot column?
	   (dolist (frame (if (frame-grid-row-limit grid)
			      (first-n (frame-grid-row-limit grid) frames)
			      frames))
	     ;; More links will go to the regular frame browser, which is better than nothing.
	     (let ((frames::*current-object* frame))
	       (html 
		 (:tr
		  (:td (:b (frame::emit-value frame)))
		  (dolist (slotdef slots)
		    (let ((slot (real-slot slotdef)))
		      (html (:td 
			     (if (slot-property slotdef :async?)
				 (let ((realframe frame))
				   (out-record-to-html
				    (async
				      (with-frame-printing-context
					(let ((frames::*current-object* frame)
					      ;; this is not working, not sure why
					      (frame::*elements-per-row* (or (slot-property slotdef :elements-per-row) frame::*elements-per-row*)))
					  (with-output-to-string (out)
					    (let ((*html-stream* out))
					      (if (swframes::frame-p slot)
						  (frame::emit-slot-value slot (slotv realframe slot))
						  (frame::emit-value (funcall slot realframe)))
					      )))))
				    "foo"))
				 ;; synchronous
				 (if (swframes::frame-p slot)
				     (frame::emit-slot-value slot (slotv frame slot))
				     (frame::emit-value (funcall slot frame)))
				 )))))))))))))))
