(in-package :swframes)

;;; Needs to be after code-source.lisp but before source is used.
(eval-when  (:compile-toplevel :load-toplevel :execute)
  (unless *default-frame-source*
    (setq *default-frame-source* *code-source*)))

#|
This file has the minimum needed to get the frame system working (esp. the reader)
|#

defclass* frame ();  (:print-function frame-printer) (:constructor %make-frame))
  (uri
   (slots nil)
   (inverse-slots nil)
   source				;Source 
   (loaded? nil)				;T if slots have been loaded (+++ should be called FILLED?)
   (inverse-loaded? nil))
  (:initable-instance-variables uri source)
  :writable-instance-variables		
  :readable-instance-variables)

(defmethod print-object ((frame frame) stream)
  (report-and-ignore-errors
   (format stream "#$~A" (frame-name frame))))

;;; gets redefined in swframes.lisp
(defun frame-name (frame)
  (frame-uri frame))

(defun frame-p (f)
  (typep f 'frame))

(setf (documentation #'frame-loaded? 'function)
      "T if frame has been completely loaded from its source")

(setf (documentation #'frame-source 'function)
      "An object that represents the source of this frame's contents (ie, a SPARQL-ENDPOINT)")

(setf (documentation #'frame-uri 'function)
      "The URI of the frame as a string")

;;; We set this globally.
(set-dispatch-macro-character #\# #\$ 'pound-dollar-frame-reader)
(set-dispatch-macro-character #\# #\^ 'pound-carat-frame-reader)
(set-dispatch-macro-character #\# #\v 'pound-inverse-frame-reader)

(defun make-reader-frame (s)
  (make-frame s :source *default-frame-source*)) 

(defun pound-dollar-frame-reader (stream char arg)
  (declare (ignore char arg))
  (make-reader-frame (read-fname stream)))

;;; ++ would be good to allow #$"sdasdad" for hard to parse names
(defun read-fname (stream)
  (let ((name
	 (read-until 
	  stream
	  (lambda (char)
	    (or (member char *whitespace*)
		(member char '(#\( #\)))))
	  (new-string)
	  t)))
    (assert (not (char= #\# (char name 0)))) ;catch this common error
    name))

;;; Works with setf through blisp magic -- see swframes/blisp
(defun pound-carat-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((slot (make-reader-frame (read-fname stream))))
    `(lambda (f) (msv f ,slot))))

;;; See above (+++ blisp doesn't deal with this yet, it's not hard)
(defun pound-inverse-frame-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((slot (make-reader-frame (read-fname stream))))
    `(lambda (f) (msv-inverse f ,slot))))

(defun make-frame (thing &key (source *default-frame-source*))
  #.(doc
     "Coerce THING (typically a URI as a string) into a frame, creating it if necessary."
     "SOURCE specifies a source, argument is ignored if frame already exists."
     "Synonymous (more or less) with INTERN-URI")
  (etypecase thing
    (frame thing)
    (string (intern-uri thing :source source))))

(defvar *force-source?* nil)

(defun intern-uri (uri &key source (class 'frame) (force-source? *force-source?*))
  #.(doc
     "Coerce THING (typically a URI as a string) into a frame, creating it if necessary."
     "SOURCE specifies a source, argument is ignored if frame already exists.")
  (if (frame-p uri) (return-from intern-uri uri))
  (if (frame-p class)
      (setf class (rdfs-clos-class class :force? t)))
  (assert (stringp uri))
  (setf uri (expand-uri uri))	
  (assert (> (length uri) 0))
  (aif (frame-named uri)
       (progn
	 (when (and source (or (null (frame-source it))
			       force-source?))
	   (setf (frame-source it) source))
	 it)
       (intern-frame
	(make-instance class
		       :uri uri 
		       :source source
		       ))))

;;; Would be nice if this were weak, but only EQ hashtables support that in CCL.
;;; Change equal to equalp for case-insensitve URLs (won't work with SPARQL though)
(defvar *uri->frame-ht* (make-hash-table :test 'equal))

;;; here for tracability.
(defun set-frame-loaded? (frame &optional (loaded? t) source)
  (setf (frame-loaded? frame) loaded?)
  (when source
    (setf (frame-source frame) source))
  frame)

(defun intern-frame (frame)
  (setf (gethash (frame-uri frame) *uri->frame-ht*) frame)
  frame)  

(defun frame-named (uri)
  "The frame named URI or nil if none is known.  Inverse of FRAME-URI"
  (gethash uri *uri->frame-ht*))

(defun unintern-uri (uri)
  (remhash uri *uri->frame-ht*))

(defun unintern-frame (f)
  (unintern-uri (frame-uri f)))

(defun rename-frame (f new-name)
  "Rename frame to a new URI"
  (if (frame-named new-name) 
      (error "There is already a frame named ~A" new-name))
  (unintern-uri (frame-uri f))
  (setf (frame-uri f) new-name)
  (intern-frame f))

;;; Reuse some biobike machinery
(defun clean-string (string)
  (create-valid-frame-name 
   string
   :space-char-action #\_
   :from-chars "()$&+,/:;=?@<>#%'^"
   :to-chars   "[].............._."))
				   
;;; redo this for urls.  Source http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
;;; note that chars like : and / are legal for URIs, but only in a certain way...
;;; Parens aren't allowed in frame names because it fucks up lisp syntax (but they are legal so could come from an outside source).
;;; +++ need to distinguish between legal chars and "unsafe" chars and syntax chars.  Ugh.
(defparameter *illegal-frame-chars*
  (coerce 
   (string+ "()$&+:;,/=?<>#%'^" *whitespace*) 
   'simple-string)
  "Characters that are not allowed in strings representing frame names")

;;; Following borrowed from BioLisp more or less verbaitm.

(defun create-valid-frame-name 
       (string 
        &key
        (prefix nil)
        (suffix nil)
        (case-action :none)
        (space-char-action :remove)
        (from-chars "()")
        (to-chars "[]")
        (verify? t)
        )
  #.(doc
     "Create a frame name from an arbitrary string STRING.  By default:"
     "  -- No case conversion is done."
     "  -- Spaces in the string are removed."
     "  -- left/right parentheses are converted to left/right brackets."
     "  -- the resulting string is scanned for illegal frame characters."
     "STRING itself is not modified, a freshly minted string is returned."
     "CASE-ACTION determines what Lisp 'case' function to call on STRING:"
     "  -- :UPPERCASE - STRING-UPCASE"
     "  -- :LOWERCASE - STRING-DOWNCASE"
     "  -- :CAPITALIZE - STRING-CAPITALIZE"
     "  -- :NONE (or NIL) - no case conversion."
     "SPACE-CHAR-ACTION determines what happens to space characters in STRING."
     "If the value is :REMOVE or :DELETE (the default) all spaces (but not"
     "other whitespace) are removed from STRING.  If the value is a character"
     "object, that character replaces all occurences of spaces in STRING."
     "Any other value is erroneous."
     "FROM-CHARS and TO-CHARS define a substitution mapping. STRING is scanned"
     "and any char in STRING which is found in FROM-CHARS is replaced by the"
     "corresponding (indexwise) char in TO-CHARS."
     "If PREFIX and/or SUFFIX are non-nil they are assumed to be strings and"
     "are concatenated to STRING before and/or after."
     "VERIFY determines whether the result string is finally scanned for"
     "characters that are not legal in frame names."
     )
  (let ((sstring (coerce (copy-seq string) 'simple-string)))
    (setq sstring
          (case case-action
            ((nil :none) sstring)
            (:uppercase (nstring-upcase sstring))
            (:lowercase (nstring-downcase sstring))
            (:capitalize (nstring-capitalize sstring))
            ))
    (when from-chars
      (unless to-chars (error "FROM-CHARS provided but not TO-CHARS!"))
      (unless (= (length to-chars) (length from-chars))
        (error "FROM-CHARS and TO-CHARS must be same length!"))
      (ntranslate-string sstring from-chars to-chars))
    (case space-char-action
      ((:remove :delete) (setq sstring (delete #\Space sstring)))
      (t
       (unless (characterp space-char-action)
         (error "SPACE-CHAR-ACTION neither a valid action nor a character!"))
       (setq sstring (nsubstitute space-char-action #\Space sstring))
       ))
    (setq sstring
          (cond
           ((and (null prefix) (null suffix)) sstring)
           ((and prefix suffix) (one-string prefix sstring suffix))
           (prefix (one-string prefix sstring))
           (suffix (one-string sstring suffix))
           ))
    (when verify?
      (let ((bad-char? nil))
        (loop for ch across sstring do
              (unless (valid-frame-char? ch)
                (error "Invalid character: ~S" ch)
                (setq bad-char? t)))
        (when bad-char? 
          (error "CONCOCT-VALID-FRAME-NAME: Illegal characters found!"))))
    sstring
    ))

(defun valid-frame-char? (x) (null (find x *illegal-frame-chars*)))

;;; These more logically belong in swframes, but are here so they can be used by make-load-form

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro for-frame-slots ((frame slot value) &body body)
  "Iterate BODY over all slots of a frame, successively binding SLOT and VALUE vars"
  `(and (frame-slots ,frame)
	(maphash #'(lambda (,slot ,value)
		     #+CCL (declare (ccl::ignore-if-unused ,slot ,value))
		     ,@body)
		 (frame-slots ,frame))))

(defmacro for-frame-inverse-slots ((frame slot value) &body body)
  "Iterate BODY over all inverse slots of a frame, successively binding SLOT and VALUE vars"
  `(and (frame-inverse-slots ,frame)
	(maphash #'(lambda (,slot ,value)
		     ,@body)
		 (frame-inverse-slots ,frame)))))

;;; Supports fasl dump.  This has to come early to allow later files to compile.  
;;; Other slot-load-forms are defined later once the defmethod$ machinery exists.
(defmethod make-load-form ((frame frame) &optional ignore)
  (declare (ignore ignore))
  (values
   `(intern-uri ,(frame-uri frame))
   (let ((forms
	  `(,@(collecting
	       (for-frame-slots (frame slot value)
		 (collect-if (slot-load-form frame slot value))))
	      ,@(when (frame-loaded? frame)
		      `((set-frame-loaded? ,frame)) ))))
     (if forms
	 `(progn ,@forms)
	 nil)
     )))

(defmethod slot-load-form (frame slot value)
;;; +++ temp patch for experimenting -- try doing a full-blown set so inverses get tracked
;  `(setf (%slotv ,frame ,slot) ',value)
 `(setf (slotv ,frame ,slot) ',value))	
  
