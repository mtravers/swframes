(in-package :knewos)

;;; UNFINISHED parser for refseq data

;;; would be nice to have this and drugcard share a common abstraction.

;;; +++ for utils
(defmacro dolines ((var stream) &body body)
  `(do ((,var (read-line ,stream nil :eof) (read-line ,stream nil :eof)))
       ((eq ,var :eof))
     ,@body))

;(parse-gpp "/misc/kbs/refseq/human.protein.gpff")

(defun parse-gpp (file)
  (with-open-file (s file)
    (let ((current-tuple (make-tuple))
	  (subtuple nil)
	  (current-value nil)
	  (current-field nil)
	  (result nil))
      (labels ((gpp-property (command)
		 (keywordize command))	;for now, maybe uri later
	     (ensure-subtuple (prop)
	       (unless (and subtuple (eq current-field prop))
		 (when subtuple
		   (setf (tuple-field current-tuple current-field) subtuple))
		 (setf subtuple (make-tuple)
		       current-field prop)))
	     (collect-current-tuple ()
	       (end-field)
	       (pprint current-tuple)
	       (push current-tuple result)
	       (setf current-tuple (make-tuple)
		     current-field nil))
	     (end-field ()
	       (print `(,current-field ,current-value))
	       (setf (tuple-field current-tuple current-field) current-value
		     current-field nil
		     current-value nil)))
	(dolines (line s)
;		 (print line)
	     (let ((command (read-from-string line nil :eof :end (min 12 (length line))))
		   (contents (and (> (length line) 12) (subseq line 12)))
		   (subprop? (char= #\Space (char line 0))))
	       (cond
		 ((equal command '//)
		  (collect-current-tuple))
		 ;; blank command
		 ((eq command :eof)
		  (push-end contents current-value))
		 ;; ORIGIN field has this weird format, hoepfully this detcts it and nothing else
		 ((numberp command)
		  (if (eq current-field :origin)
		      (push-end line current-value)
		      (error "blah")))
		 (subprop?
		  (ensure-subtuple (gpp-property command))
		  (push-end contents current-value))
		 (t
		  (setf current-field (gpp-property command)
			current-value (list contents))))))))))
		 
	       
	       
	       
