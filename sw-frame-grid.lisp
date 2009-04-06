(in-package :wb)

(defun frame-grid (frames &optional slots row-limit)
  #.(one-string-nl
     "Output a table where the rows are FRAMES and columns are SLOTS."
     "If SLOTS is missing, use the slots of the first frame.")
  ;; Include error checks because otherwise you can get errors in the weblistener redisplay, which fucks things up rather badly.
  (dolist (frame frames)
    (assert (swframes::frame-p frame))
    (swframes::fill-frame frame)
    )
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
  (make-frame-grid :frames frames :slots slots :row-limit row-limit))

(defstruct frame-grid 
  frames
  slots 
  (row-limit nil)
  (sorting? nil)
  )

(defmethod potential-slots ((grid frame-grid))
  (sort (mapunion #'swframes::%frame-slots (frame-grid-frames grid))
	#'string-lessp
	:key #'swframes::frame-label))
  

(defmethod add-column-ui ((grid frame-grid))
  (let* ((potential-slots (potential-slots grid))
	 (id (session-persist-object grid)))
    (html
     ((:form
	:action "add-function"
	:method "POST")
       ((:input :type "hidden" :name "grid-id" :value id))
       "Def:" ((:textarea :name "sexp" :cols 80 :rows 5)) :br
       "Name:" ((:input :name "name"))
       ((:input :type "submit" :value "Add function")))
     ((:form
	:action "add-column"
	:method "POST")
       ((:input :type "hidden" :name "grid-id" :value id))
       ((:select :name "slot")
	(dolist (pslot potential-slots)
	  (html
	    ((:option :value (swframes::frame-uri pslot))
	     (:princ (swframes::frame-label pslot))))))
       ((:input :type "submit" :value "Add slot")))

      )))
    

(publish :path "/add-column"
	 :function 'do-add-column)

(defun do-add-column (req ent)
  (with-http-response (req ent)
    (with-session (req ent)
      (let* ((grid (session-persisted-object (net.aserve::request-query-value "grid-id" req)))
	     (slot (swframes::frame-named  (net.aserve::request-query-value "slot" req))))
	(setf (frame-grid-slots grid)
	      (append (frame-grid-slots grid) (list slot)))
	(net.aserve::redirect-to req ent "/redisplay.html")))))

(publish :path "/add-function"
	 :function 'do-add-function)

(defun do-add-function (req ent)
  (with-http-response (req ent)
    (with-session (req ent)
      (let* ((grid (session-persisted-object (net.aserve::request-query-value "grid-id" req)))
	     (name (net.aserve::request-query-value "name" req))
	     (name-sym (intern name (find-package *username*)))
	     (fun-text (net.aserve::request-query-value "sexp" req))
	     (fun (compile name-sym (read-from-string fun-text))))
	(setf (get name-sym :text) fun-text)
	(setf (frame-grid-slots grid)
;	      (append (frame-grid-slots grid) (list fun)))
	      (append (frame-grid-slots grid) (list name-sym)))
	(net.aserve::redirect-to req ent "/redisplay.html")))))


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
				    (if (symbolp rslot)
					(html (:b (frame::emit-value rslot) )
					      :br
					      (:div (:pre (:princ (get rslot :text)))))
					;; frame slot
					(html (:b (frame::emit-value rslot)) :br))))
			      (if (and (frame-grid-sorting? grid) ;was commented out, not sure why
				       (column-sortable? grid rslot))
				  (html ((:a :href (eval-link `(frame-grid-sort ,(ref-to-current-output) ',rslot :up)) 
					     :title "Sort up")
					 "U")
					"&nbsp;"
					((:a :href (eval-link `(frame-grid-sort ,(ref-to-current-output) ',rslot :down))
					     :title "Sort down")
					 "D")
					"&nbsp;"
					))
			      (html ((:a :href (eval-link `(frame-grid-export-column (session-persisted-object ,id) ',rslot))
					 :title "Export column to list")
				     "E")
				    (unless label?
				      (html
					"&nbsp;"
					((:a :href (eval-link `(frame-grid-delete-column ,(ref-to-current-output) ',rslot))
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
	      (column-header slot))
	    (:th
	     (add-column-ui grid)
	     ))
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
				    (async ()
				      (with-frame-printing-context
					(let ((frames::*current-object* frame)
					      ;; this is not working, not sure why
					      (frame::*elements-per-row* (or (slot-property slotdef :elements-per-row) frame::*elements-per-row*)))
					  (with-output-to-string (out)
					    (let ((*html-stream* out))
					      (if (swframes::frame-p slot)
						  (frames::emit-slot-value slot (swframes::slotv realframe slot))
						  (frame::emit-value (funcall slot realframe)))
					      )))))
				    "foo"))
				 ;; synchronous
				 (handler-case 
				     (if (swframes::frame-p slot)
					 (frames::emit-slot-value slot (swframes::slotv frame slot))
					 (frame::emit-value (funcall slot frame)))
				   (error (e)
				     (html (:i (:princ-safe e)))))
				 )))))))))))))))



(defun frames::emit-slot-value (slot-frame slot-value)
;;   (vif (html-generator
;; 	(and (not *print-lispy*) 
;; 	     (slotv slot-frame #$HTMLGenerator)))
;;        (funcall html-generator slot-value)
       (frames::emit-value slot-value)
       )

;;; +++ exp, not working...
(defmethod frames::emit-value 
    ((object swframes::frame) &optional (print-limit nil))
  (declare (ignore print-limit))
  (html
   ((:a :href (frames::wob-url object))
    (if (sw::frame-loaded? object)
	(html (:princ-safe (sw::frame-label object))
	      :newline)
	(async-html (:pre-text (sw::frame-label object))
		    (sw:fill-frame object)
		    (html (:princ-safe (sw::frame-label object))
			  :newline))
	))))


(defmethod frames::wob-url ((object swframes::frame))
  (formatn
   (one-string
    "/frame"
;    (wob-state-variable-values-url-argstring)
    "?name=~A")
   (url-safe-string (swframes::frame-name object))
   ))

;;

(defun frame-grid-value (grid row column)
  (declare (ignore grid))
  (if (swframes::frame-p column)
      (swframes::slotv row column)
    (funcall column row)))
