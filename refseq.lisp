(in-package :knewos)

#|
Todo:
- REFERENCE can occur multiple times in a record and has content on the line (in addition to subfields

- FEATURES has a complex structure
  - subfields don't obey the same rule
  - subfields can occur multiple times.
  - sub-subfields with / (qualifiers)
    - this is actually cool, it xrefs dbSNP and other databases.
      - if we are really RDFizing, probably need to capture these sanely

- ORIGIN has yet another format.

- SOURCE/ORGANISM -- I think the ORGANISM part can be ignored, it's just taxonomy?
|#


;;; would be nice to have this and drugcard share a common abstraction.

;;; +++ for utils
(defmacro dolines ((var stream) &body body)
  `(do ((,var (read-line ,stream nil :eof) (read-line ,stream nil :eof)))
       ((eq ,var :eof))
     ,@body))

;(parse-gpp "/misc/kbs/refseq/human.protein.gpff")

(defun parse-gpp (file &key limit)
  (with-open-file (s file)
    (let ((current-tuple (make-tuple))
	  (subtuple nil)
	  (current-value nil)
	  (current-field nil)
	  (result nil))
      (labels ((gpp-property (command)
		 (keywordize command))	;for now, maybe uri later
	       (ensure-subtuple ()
		 (unless subtuple
		   (setf subtuple (make-tuple)
			 (tuple-field current-tuple current-field) subtuple)))
	       (collect-current-tuple ()
		 (end-field)
		 (pprint `(TUPLE  ,current-tuple))
		 (push current-tuple result)
		 (setf current-tuple (make-tuple)
		       current-field nil)
		 (when (and limit (> (length result) limit))
		   (return-from parse-gpp)))
	       (end-subtuple ()
		 (when subtuple
		   (setf subtuple nil)))
	       (end-field ()
		 (when current-field
		   (print `(FIELD ,current-field ,current-value))
		   (setf (tuple-field (or subtuple current-tuple) current-field) current-value
			 current-field nil
			 current-value nil))))
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
		      (when subtuple
			(end-field))
		      (ensure-subtuple)
		      (setf current-field (gpp-property command)
			    current-value (list contents)))
		     (t
		      (end-field)
		      (end-subtuple)
		      (setf current-field (gpp-property command)
			    current-value (list contents))))))))))
		 
	       
	       
	       
