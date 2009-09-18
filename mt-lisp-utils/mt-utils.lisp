(in-package :mt)

#| ######################################################################

 Random Lisp utilities

Copyright � 1994-2008 Michael Travers
Permission is given to use and modify this code as long
as the copyright notice is preserved.

Send questions, comments, and fixes to mt@media.mit.edu.

-------------------------------------------------------------------------

This file contains a large miscellany of useful hacks that I have come
to depend on.  

Originally written for MCL; this library now has been used with SBCL,
ACL, and ABCL as well.

Many items here are borrowed from others. In most cases the source
is acknowledged.

###################################################################### |#

;;; Generalized variables, lists, binding, etc.

(defmacro deletef (thing place &rest delete-args)
  `(setf ,place (delete ,thing ,place ,@delete-args)))

(defmacro push-end (thing place)
  `(setf ,place
         (nconc ,place
                (list ,thing))))

(defmacro pushnew-end (item place)
  `(if (member ,item ,place)
     ,place
     (push-end ,item ,place)))

(defun list-insert (list item n)
  (let ((cdr (nthcdr (1- n) list)))
    (rplacd cdr (cons item (cdr cdr)))
    list))

; destructive
(defun insert-before (list new before)
  (if (null before)
      (nconc list (list new))
      (do ((rest list (cdr rest))
	   (last nil rest))
	  ((null rest)
	   (error "insert-before: ~A not in ~A" before list))
	(if (eq before (car rest))
	    (if last
		(progn (rplacd last (cons new rest))
		       (return list))
		(return (cons new list)))))))

;;; or subseq-safe
(defun firstn (list n)
  (if (>= n (length list))
    list
    (subseq list 0 n)))

;;; If the lists are not sets (have duplicated items) result is undefined
(defun set-equal (l1 l2 &optional (test #'eq))
  (and (= (length l1) (length l2))
       (dolist (elt l1 t)
         (unless (find elt l2 :test test)
           (return nil)))))

;;; for descending down alist type structures, esp. those in JSON like form
(defun findprop (structure prop)
  (cadr (member prop structure :test #'equal)))

;;; recursive version of above
(defun findprops (structure &rest props)
  (if (null props) 
      structure
      (apply #'findprops (findprop structure (car props)) (cdr props))))

;;; mv-let*: lets the car of a let binding form be a list
;;; elements of which get bound to multiple values.

(defmacro mv-let* (forms &body body)
  (cond ((null forms)
	 `(progn ,@body))
	((or (symbolp (car forms))
	     (symbolp (caar forms)))

	 `(let (,(car forms))
	    (mv-let* ,(cdr forms)
	      ,@body)))
	(t
	 `(multiple-value-bind ,(caar forms) ,(cadar forms)
	    (mv-let* ,(cdr forms)
	      ,@body)))))

;;; Lifted from PCL.  Ensure that a macro variable is only expanded once.
(defmacro once-only (vars &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (dolist (var vars)
      (push `(if (or (symbolp ,var)
                     (numberp ,var)
                     (and (listp ,var)
			  (member (car ,var) '(quote function))))
                 ,var
                 (let ((,gensym-var (gensym)))
                   (push ,gensym-var ,run-time-vars)
                   (push ,var ,run-time-vals)
                   ,gensym-var))
            expand-time-val-forms))    
    `(let* (,run-time-vars
            ,run-time-vals
            (wrapped-body
              ((lambda ,vars . ,body) . ,(reverse expand-time-val-forms))))
       `((lambda ,(nreverse ,run-time-vars)  ,wrapped-body)
         . ,(nreverse ,run-time-vals)))))

#-MCL
(defmacro let-globally (clauses &body body)
  (let ((temp-vars (mapcar #'(lambda (s) (gensym (symbol-name (car s)))) clauses)))
    `(let ,(mapcar #'(lambda (temp clause) `(,temp ,(car clause))) temp-vars clauses)
       (unwind-protect 
         (progn
           ,@(mapcar #'(lambda (clause)
                         (cons 'setf clause))
                     clauses)
           ,@body)
         ,@(mapcar #'(lambda (temp clause)
                       `(setf ,(car clause) ,temp))
                   temp-vars clauses)))))

;;; destructuring-let

;;; Random small aids to expression

(defmacro non-nil (var)
  `(and (boundp ',var)
	,var))

(declaim (ignore ignore))               ; So sue me

(defmacro return-if (val)
  (once-only (val)
    `(if ,val (return ,val))))

(defmacro return-from-if (block val)
  (once-only (val)
    `(if ,val (return-from ,block ,val))))

#-MCL
(defmacro neq (a b)
  `(not (eq ,a ,b)))

(defun circular-list (&rest elements)
  (rplacd (last elements) elements))


;;; Iteration and Mapping

;;; Like dolist, but works with any sequence
(defmacro dosequence ((var sequence &optional result) &body body)
  (let ((index (gensym))
	(len (gensym)))
    `(do ((,index 0 (1+ ,index))
	  (,len (length ,sequence))
	  ,var)
      ((= ,index ,len)
       ,result)
      (setq ,var (elt ,sequence ,index))
      ,@body)))

(defmacro do-for-array-elements (array vars &body body)
  `(let ((array-dimensions (array-dimensions ,array)))
     (do-for-array-elements-1 ,array ,vars 0 ,@body)))

(defmacro do-for-array-elements-1 (array vars dim &body body)
  (if vars
      `(dotimes (,(car vars) (nth ,dim array-dimensions))
	 (do-for-array-elements-1 ,array ,(cdr vars) ,(1+ dim)
	   ,@body))
      `(progn ,@body)))

;;; do-collect
(defmacro collecting (&body body)
  `(let ((%results nil))
     (flet ((collect (thing) (push thing %results))
	    (collect-new (thing &optional (test #'eql)) (pushnew thing %results :test test)))
       ,@body)
     (nreverse %results)))

(defmacro summing (&body body)
  `(let ((%result 0))
     (flet ((sum (thing) (incf %result thing)))
       ,@body)
     %result))
  

;;; generalized good iterator.

;;; Maximum/minimums

;;; +++ flush return-max, use multiple values instead
(defun extreme (list test &key (key #'identity) (return-max nil))
  (and list
       (let* ((best (car list))
              (max (funcall key best)))
         (dolist (other (cdr list) (if return-max max best))
           (let ((score (funcall key other)))
	     (when (funcall test score max)
               (setq best other max score)))))))

; +++ key arguments are slow
(defun extremes (list test &key (key #'identity))
  (if list
    (let* ((best (list (car list)))
           (max (funcall key (car best))))
      (dolist (other (cdr list) (values best max))
        (let ((score (funcall key other)))
          (if (funcall test score max)
            (setq best (list other) max score)
            (if (funcall test max score)
              nil
              (push other best))))))
    (values nil most-negative-fixnum)))

(defun maximize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'> :key key :return-max return-max))

(defun minimize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'< :key key :return-max return-max))

(defun maximums (list &key (key #'identity))
  (declare (inline extremes))            ; not that this does anything
  (extremes list #'> :key key))

(defun minimums (list &key (key #'identity))
  (declare (inline extremes))            ; not that this does anything
  (extremes list #'< :key key))

(defun closest (value list key)
  (minimize list :key #'(lambda (elt) (abs (- value (funcall key elt))))))

;;; Various mapping functions
;;; Most of these borrowed from Ken Haase

#-ABCL  ;;; has a collect fn built in, need to change name
(defun collect (fcn list)
  "Applies FCN to each element of LIST returning all the non-nil values as a list."
  (let* ((head (list 'HEAD)) 
         (tail head))
    (dolist (elt list (cdr head))
      (let ((value (funcall fcn elt)))
	(when value
	  (push value (cdr tail))
	  (setf tail (cdr tail)))))))



(defun mapappend (fcn list)
  "Applies FCN to every element of LIST, appending the results together.
Order is maintained as one might expect."
  (let* ((head (list '())) (tail head))
    (dolist (elt list (cdr head))
      (dolist (result-elt (funcall fcn elt))
	(setf (cdr tail) (list result-elt))
	(setf tail (cdr tail))))))

(defun mapunion (fcn list)
  "Applies FCN to every element of LIST, unioning the results together.
Except for removal of EQL occurences, order is maintained as one might expect."
  (let* ((head (list '())) (tail head))
    (dolist (elt list (cdr head))
      (dolist (result-elt (funcall fcn elt))
	(unless (member result-elt head)
	  (setf (cdr tail) (list result-elt))
	  (setf tail (cdr tail)))))))

(defun maptree (fcn tree)
  (if (listp tree)
      (mapcar #'(lambda (elt) (maptree fcn elt)) tree)
    (funcall fcn tree)))

;;; works on structure with dotted lists
(defun maptree-dots (fcn tree)
  (cond ((null tree) nil)
	((listp tree)
	 (cons (maptree-dots fcn (car Tree))
	       (maptree-dots fcn (cdr Tree))))
	(t (funcall fcn tree))))


(defun mapsum (fcn list)
  (let ((result 0))
    (dolist (elt list result)
      (incf result (funcall fcn elt)))))

(defun mapcross (fcn list1 list2)
  "Applies FCN to every combination of elements from LIST1 and LIST2,
returning the list of results.  Order is maintained as one might expect."
  (let* ((head (list '())) (tail head))
    (dolist (e1 list1 (cdr head))
      (dolist (e2 list2)
	(push (funcall fcn e1 e2) (cdr tail))
	(setf tail (cdr tail))))))

(defun split-list (predicate list)
  "Returns two lists extracted from list based on PREDICATE."
  (let ((wheat '()) (chaff '()))
    (dolist (elt list (values wheat chaff))
      (if (funcall predicate elt)
	  (push elt wheat) (push elt chaff)))))

; +++ key args are slow
(defun filter (predicate list &key key &aux wheat)
  "Return only the elements of list meeting PREDICATE"
  (dolist (elt list (nreverse wheat))
    (when (funcall predicate (if key (funcall key elt) elt))
      (push elt wheat))))

(defun filter-out (predicate list &key key &aux wheat)
  "Return only the elements of list not meeting PREDICATE"
  (dolist (elt list (nreverse wheat))
    (unless (funcall predicate (if key (funcall key elt) elt))
      (push elt wheat))))

;;; String Utilities

;;; this is the same as the CL function SUBSTITUTE, so let's flush it...
#|
(defun string-replace-char (string char0 char1 &key (start 0) (end nil))
  (do ((from start)
       (new-string (concatenate 'string string)))
      ((null from) new-string)
    (setq from (position char0 string :start (1+ from) :end end))
    (when from
      (setf (char new-string from) char1))))
|#

;;; Return substrings separated by separator character.  
(defun parse-substrings (string separator)
  (do ((result nil (cons (subseq string finger0 finger1) result))
       (finger0 0 (and finger1 (1+ finger1)))
       (finger1 (position separator string) (and finger1
                                                 (position separator string :start (1+ finger1)))))
      ((null finger0)
       (nreverse result))))

;;; a much-needed function. this version conses rather more than it should.
(defun string-replace (string find replace &key (start 0) (end nil) (sequence-type 'string) (test #'char-equal))
  (do ((from start)
       (substrings nil)
       (subst-start t))
      ((null subst-start)
       (apply #'concatenate
              sequence-type
              (nreverse substrings)))
    (setq subst-start (search find string :start2 from :end2 end :test test))
    (push (subseq string from subst-start) substrings)
    (when subst-start
      (setf from (+ subst-start (length find)))
      (push replace substrings))
    ))

(defun first-line (string)
  (car (parse-substrings string #\Newline)))

(defun fast-string (obj)
  (typecase obj
    (null "")
    (string obj)
    (symbol (symbol-name obj))          ;what about package?
    (t (fast-princ-to-string obj))))

(defun fast-temp-string (obj)
  (typecase obj
    (null "")
    (string obj)
    (symbol (symbol-name obj))          ;what about package?
    (t (fast-princ-to-temp-string obj))))

;;; +++ this stuff is MCL dependent.

#|  Class is in Clozure, but not working the same way apparently
#+MCL
(defvar *fast-princ-to-string-stream* 
  (make-instance 'ccl::string-output-stream
    :string (make-array 100
                        :element-type 'character
                                      :adjustable t
                                      :fill-pointer 0)))

#+MCL
(defun fast-princ-to-string (obj)
  (princ obj *fast-princ-to-string-stream*)
  (ccl::get-output-stream-string *fast-princ-to-string-stream*))

#-MCL
|#
(defun fast-princ-to-string (obj)
  (princ-to-string obj))

;;; This version returns a string that will change on the next call!
#+MCL
(defun fast-princ-to-temp-string (obj)
  (let ((string (slot-value *fast-princ-to-string-stream* 'ccl::my-string)))
    (setf (fill-pointer string) 0)
    (princ obj *fast-princ-to-string-stream*)
    string))

#+MCL
(defun fast-prin1-to-temp-string (obj)
  (let ((string (slot-value *fast-princ-to-string-stream* 'ccl::my-string)))
    (setf (fill-pointer string) 0)
    (prin1 obj *fast-princ-to-string-stream*)
    string))

#+MCL
(defun fast-pprint-to-temp-string (obj)
  (let ((string (slot-value *fast-princ-to-string-stream* 'ccl::my-string)))
    (setf (fill-pointer string) 0)
    (pprint obj *fast-princ-to-string-stream*)
    string))

#+MCL
(defun fast-format-to-temp-string (control &rest args)
  (let ((string (slot-value *fast-princ-to-string-stream* 'ccl::my-string)))
    (setf (fill-pointer string) 0)
    (apply #'format *fast-princ-to-string-stream* control args)
    string))


(defun string-truncate (string length)
  (if (<= (length string) length)
      string
      (format nil "~A..." (subseq string 0 length))))

;;; inefficient, if the purpose is to make long lists manageable.  Easy to fix.
(defun list-truncate (list length)
  (if (> (length list) length)
      (subseq list 0 length)
      list))

;;; Function definition

;;; Define an inline function
#-ABCL ; already built in, hopefully compatible
(defmacro defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))       
     (defun ,name ,args
       ,@body)))

#+ABCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'extensions::defsubst))

#|  This has vanished from Clozure
#+MCL (pushnew "subst" 
               (cdr (assoc 'function ccl::*define-type-alist*)))
|#

; Define a memoized function.  The function should be a function in the mathematical sense
; (a mapping with no state dependencies).  It can't take &rest, &optional, or &key args.
; Comparision of new args to old is by EQUAL.  Redefining the function resets
; the cache.  
; +++ handle declarations in body
; +++ destructuring-bind is not CL
(defmacro def-cached-function (name arglist &body body)
  (let ((ht (make-hash-table :test #'equal)))
    `(defun ,name (&rest args)
       (declare (dynamic-extent args))
       (multiple-value-bind (val found)
	   (gethash args ,ht)
	 (if found 
           val
           (setf (gethash (copy-list args) ,ht)
                 (block ,name
                   (destructuring-bind ,arglist args
                     ,@body))))))))


;;; Randomness

(defun random-element (list)
  (and list
       (nth (random (length list)) list)))

(defun arand (center range)
  (+ center (random (* 2.0 range)) (- range)))

;;; Numbers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant single-pi (coerce pi 'single-float)))	;Avoid introducing double-floats
(defconstant pi/2 (/ single-pi 2.0))
(defconstant pi/4 (/ single-pi 4.0))
(defconstant 2pi (* single-pi 2))
(defconstant degrees-to-radians (/ 2pi 360))
(defconstant radians-to-degrees (/ degrees-to-radians))
(defmacro d2r (deg)
  `(* degrees-to-radians ,deg))
(defmacro d2ri (deg)
  (* degrees-to-radians deg))
(defmacro r2d (rad)
  `(* radians-to-degrees ,rad))

;;; Fast fixnum arithmetic  

;;; Define functions such as +& that work only for fixnum arguments and results, and 
;;; omit typechecking if the compiler is smart enough (in MCL, it often is -- use DISASM
;;; to check what the compiler is putting out).

(defmacro int (x) `(the fixnum ,x))

(defmacro def-fixnum-op (int-name reg-name &optional (result-type 'fixnum))
  `(defmacro ,int-name (&rest args)
     `(the ,',result-type (,',reg-name ,@(mapcar #'(lambda (arg) `(the fixnum ,arg)) args)))))


(def-fixnum-op +& +)                     ; Only addition-type ops actually get any boost in MCL2.0
(def-fixnum-op -& -)
(def-fixnum-op incf& incf)
(def-fixnum-op decf& decf)
(def-fixnum-op 1+& 1+)
(def-fixnum-op 1-& 1-)
(def-fixnum-op =& = atom)
(def-fixnum-op <& < atom)
(def-fixnum-op <=& <= atom)
(def-fixnum-op >& > atom)
(def-fixnum-op >=& >= atom)
(def-fixnum-op zerop& zerop atom)
(def-fixnum-op plusp& plusp atom)
(def-fixnum-op minusp& minusp atom)

(def-fixnum-op logand& logand)
(def-fixnum-op logior& logior)
(def-fixnum-op lognot& lognot)
(def-fixnum-op logandc1& logandc1)
(def-fixnum-op logxor& logxor)

(def-fixnum-op *& *)                     ; these only work in a few cases (like (* 2 <var>))
(def-fixnum-op /& /)

(defsubst max& (a b)
  (if (>=& a b)
    a b))

(defsubst min& (a b)
  (if (<=& a b)
    a b))

(defmacro ^ (x y)
 `(expt ,x ,y))

(defsubst sign (num)
  (cond ((plusp num) 1)
	((minusp num) -1)
	(t 0)))

;;; ???
(defun absmin (n min)
  (if (plusp n)
    (min n min)
    (max n (- min))))

(defsubst abs-max (max num)
  (if (<= (abs num) max)
      num
      (* max (sign num))))

(defun integers (from to)
  (if (equal from to) (list from)
      (cons from (integers (+ from (sign (- to from))) to))))

(defun log2 (x)
  (/ (log x) #.(log 2)))

(defun number-of-bits (n)   ;;; smallest k st 2**k>=n
  (ceiling (log2 n)))

(defun bits (n &aux result)
  (dotimes (bit (number-of-bits n) (nreverse result))
    (unless (zerop (logand (ash 1 bit) n) )
      (push bit result))))

;;; lousy, won't work on large lists
(defun average (list)
  (if (null list) 0
      (/ (apply #'+ list) (length list))))

(defun average (list)
  (if (null list) 0
      (/ (summing (dolist (elt list) (sum elt))) (length list))))

(defun std-dev (list &aux (average (average list)))
  (sqrt (/ (apply #'+ 
                   (mapcar #'(lambda (x) (expt (- x average) 2)) 
                           list))
            (length list))))

(defun geo-mean (list)
  (nth-root (apply #'* list) (length list)))

(defun nth-root (x n)
  (if (> x 0) (exp (/ (log x) n))
      (error "In NTH-ROOT, X=~S is not positive." x)))


;;; Fast versions of non-arithmetic functions

(defsubst car& (cons)
  (car (the cons cons)))

(defsubst cdr& (cons)
  (cdr (the cons cons)))

;;; Assumes args are the right type and does no bounds checking
(defsubst svref& (vector index)
  (declare (optimize (speed 3) (safety 0)))
  (svref vector (the fixnum index)))

(defsetf svref& (vector index) (new-val)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (svref ,vector (the fixnum ,index)) ,new-val)))

(defsubst schar& (string index)
  (declare (optimize (speed 3) (safety 0)))
  (schar (the simple-string string) (the fixnum index)))

(defsetf schar& (string index) (new-val)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (schar (the simple-string ,string) (the fixnum ,index)) ,new-val)))

;;; Note:  this has no speedup effect in MCL 2.0
;;; +++ not setfable
(defmacro aref& (array &rest indicies)
  `(locally
     (declare (optimize (speed 3) (safety 0)))
     (aref ,array ,@(mapcar #'(lambda (index) `(the fixnum ,index)) indicies))))

;;; Symbols and packages

;;; package is the package of the first argument. Er, no, that doesn't work well, fuck.
(defun symbol-conc (&rest parts)
  (intern (apply #'concatenate 'string (mapcar 'string parts)) (symbol-package (car parts))))

;;; +++ causing package problems
'(defun keyword (symbol)
  (intern (string symbol) (find-package :keyword)))

(defun keywordize (symbol)
  (intern (string symbol) (find-package :keyword)))

;;; CL provides no externalp function, and neither does MCL (although it keeps this info with the symbol).
(defun externalp (symbol)
  (multiple-value-bind (ignore type) 
      (find-symbol (symbol-name symbol) (symbol-package symbol))
    (eq type :external)))

(defun add-nickname (package nickname)
  (rename-package package
                  (package-name package)
                  (adjoin nickname (package-nicknames package) :test #'string-equal)))

;;; Time

;;; these are slightly mistitled, since they don't necessarily return strings anymore

(defun date-time-string (universal-time &optional (include-time t) (stream nil))
  (multiple-value-bind (second minute hour date month year day-of-week) 
                       (decode-universal-time universal-time)
    (declare (ignore second))
    (let ((months '(January February March April May June 
                    July August September October November December))
          (days '(monday tuesday wednesday thursday friday saturday sunday)))
      (format stream "~A ~A ~A, ~A~:[~; ~A:~2,'0D~A~]" 
              (string-capitalize (string (nth day-of-week days)))
              (string-capitalize (string (nth (- month 1) months)))
              date year 
              include-time
              (mod hour 12) minute 
              (if (>= hour 12) "pm" "am")))))

(defun short-date-time-string (universal-time &optional (include-time t) (stream nil))
  (multiple-value-bind (second minute hour date month year day-of-week) 
                       (decode-universal-time universal-time)
    (declare (ignore second day-of-week))
    (format stream "~A/~A/~A~:[~; ~A:~2,'0D~]" 
            month date
            (- year (* 100 (floor year 100)))
            include-time
            hour minute)))

; : means use short format
; @ means omit the time
(defun format-time (stream ut colon-flag at-flag)
  (funcall (if colon-flag
             #'short-date-time-string
             #'date-time-string)
           ut 
           (not at-flag)
           stream))

;;; Streams and strings

(defun new-string (&optional (initial-length 10))
  (make-array initial-length :element-type 'character 
              :adjustable t :fill-pointer 0))


;;; Limitation: these only work with character streams; could be generalized
(defun stream-copy (in out)
  (do (char) (())
    (setq char (read-char in nil :eof))
    (if (eq char :eof)
      (return)
      (write-char char out))))

(defun file-copy (in out)
  (with-open-file (ins in)
    (with-open-file (outs out :direction :output)
      (stream-copy ins outs))))

(defun pprint-to-string (struct &optional (right-margin *print-right-margin*))
  (with-output-to-string (stream)
    (let ((*print-pretty* t)
          (*print-right-margin* right-margin))
      (write struct :stream stream))))

(defun read-until (stream end-char-or-pred &optional (string (new-string)) untyi-end?)
  (do ((char (read-char stream) (read-char stream) ))
      ((if (characterp end-char-or-pred)
         (char= char end-char-or-pred)
         (funcall end-char-or-pred char))
       (when untyi-end?
         (stream-untyi stream char))
       (values string char))
    (when string 
      (vector-push-extend char string))))

(defun read-until-string (stream end-string &optional (string (new-string)))
  (let* ((end-string-length (length end-string))
         (last-char (aref end-string (1- end-string-length))))
    (do ()
        (())
      (read-until stream last-char string)
      (when (string= end-string
                     string
                     :end1 (1- end-string-length)
                     :start2 (- (length string) end-string-length -1))
        (return string)))))

(defvar *whitespace* '(#\Space #\Tab #\Return #\Newline #\Page #\Null #\Linefeed #+MCL #\312))

(defun string-split (str char &key count)
  ;; given a string return a list of the strings between occurances
  ;; of the given character.
  ;; If the character isn't present then the list will contain just
  ;; the given string.
  (let ((loc (position char str))
	(start 0)
	(res))
    (if (null loc)
	;; doesn't appear anywhere, just the original string
	(list str)
	;; must do some work
	(loop
	   (push (subseq str start loc) res)
	   (setq start (1+ loc))
	   (if count (decf count))
	   (setq loc (position char str :start start))
	   (when (or (null loc)
		     (eql 0 count))
	     (if (< start (length str))
		 (push (subseq str start) res)
		 (push "" res))
	     (return (nreverse res)))))))

(defun string-split-words (str)
  ;; split the given string into words (items separated by white space)
  ;;
  (let ((state 0)
	(i 0)
	(len (length str))
	(start nil)
	(res)
	(ch)
	(spacep))
    (loop
      (if (>= i len)
	  (setq ch #\space)
	  (setq ch (char str i)))
      (setq spacep (fast-whitespacep ch))
      (case state
	(0  ; looking for non-space
	 (if (not spacep)
	     (setq start i
		   state 1)))
	(1  ; have left anchor, looking for space
	 (when spacep
	     (push (subseq str start i) res)
	     (setq state 0))))
      (when (>= i len) (return))
      (incf i))
    (nreverse res)))

(defun fast-whitespacep (char)
  (declare (optimize (speed 3) (safety 0)))
  (member char *whitespace* :test #'eql))

(defun string-trim-whitespace (string)
  (string-trim *whitespace* string))


;;; Hash tables

(defun dump-ht (ht)
  (maphash #'(lambda (key value)
               (format t "~%~A: ~A" key value))
           ht))

(defun ht-contents (ht)
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (push (list key value) result))
           ht)
    result))

;;; CLOS

(defmethod subclasses ((c class))
  (remove-duplicates
   (cons c (mapcan #'subclasses (class-direct-subclasses c)))))

(defmethod superclasses ((c class))
  (remove-duplicates
   (cons c (mapcan #'superclasses (class-direct-superclasses c)))))

(defclass plist-mixin () ((plist :initform nil)))

(defmethod oget ((o plist-mixin) property &optional (default nil))
  (getf (slot-value o 'plist) property default))

(defmethod oput ((o plist-mixin) property value)
  (setf (getf (slot-value o 'plist) property)
        value))

;;; call a generic function iff it is implemented for the args
; note: this won't work when a gf gets encapsulated (ie, by trace or metering)
(defun call-if (gf &rest args)
  (when (apply #'method-exists-p gf args)
    (apply gf args)))

;;; Anaphoric macros (from _On Lisp_ by Paul Graham)

(defmacro aif (if then &optional else)
  `(let ((it ,if))
     (if it
       ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test
        (progn ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (when clauses
    (let ((clause (car clauses))
          (sym (gensym)))               ; used so that it is bound only around rhs of clause
      `(let ((,sym ,(car clause)))
         (if ,sym
           (let ((it ,sym)) ,@(cdr clause))
           (acond ,@(cdr clauses)))))))

(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body))
     #'self))

;;; Higher order procedures

;;; Curried functions.

(defun curry (function &rest curried-args)
  #'(lambda (&rest rest-args)
      (apply function (nconc curried-args rest-args))))

(defun rcurry (function &rest curried-args)
  #'(lambda (&rest rest-args)
      (apply function (nconc rest-args curried-args))))

#|
example: (defun numsort (l) (funcall (rcurry #'sort #'<) l)) 

Why is this easier than writing a lambda expression? Well, if it was scheme we could write simply:
  (define numsort (rcurry sort <))
which is nice and compact. But, this isn't Scheme. Still, these are occasionally useful.
 |#

;;; Saving and restoring

;;; Dump an arbitrary structure to a file. If CLOS objects are part of the structure,
;;; make-load-form will be called.  See clos-dumper.lisp for a slightly more sophisticated
;;; version of this.

(defun dump-vars-to-file (vars file)
  (with-open-file (s (merge-pathnames ".lisp" file) :direction :output :if-exists :supersede)
    (format s "(in-package :~A)~%" (package-name *package*))
    (dolist (var vars)
      (format s "~%(setf ~S '#.~S)"
              var var)))
  (compile-file file)
  (delete-file file))                   ; get rid of lisp file

;;; older function, here for compatibility
(defun dump-var-to-file (var file)
  (dump-vars-to-file (list var) file))

;;; Bit manipulation

;;; Define bit-fields in any setf'able place
;; Example: (defbit node-expanded? 'node-bits 1)
;; uses fixnum declarations!  Better initialize field to zero, not nil!
(defmacro defbit (name field pos)
  (let ((setter (symbol-conc 'set- name)))
    `(progn
       (defmacro ,name (obj)
         `(not (zerop& (logand& (,,field ,obj) ,,(lsh 1 pos)))))
       (defmacro ,setter (obj new-val)
         (once-only (obj new-val)
           `(progn
              (setf (,,field ,obj)
                  (if ,new-val
                    (logior& ,,(lsh 1 pos) (,,field ,obj))
                    (logandc1& ,,(lsh 1 pos) (,,field ,obj))))          ; +++ not inlined in MCL3?
              ,new-val)))
       (defsetf ,name ,setter))))

;;; Debugging

;;; I don't use this much anymore: see CTRACE.LISP for a more flexible way to do this.

(defvar *debug-level* 0)
(defvar *debug-indent-level* 0)
(defvar *debug-stream* *trace-output*)

(defun debug-line-out (string &rest args)
  (terpri *debug-stream*)
  (dotimes (n (* *debug-indent-level* 2)) (write-char #\space *debug-stream*))
  (apply #'format *debug-stream* string args))

(defmacro debug-trace (level string &rest args)
  `(when (<= ,level *debug-level*)
     (debug-line-out ,string ,@args)))

(defmacro with-debug-trace ((level string &rest args) &body body)
  `(let ((*debug-indent-level* *debug-indent-level*))
     (when (<= ,level *debug-level*)
       (debug-line-out ,string ,@args)
       (incf *debug-indent-level*))
     ,@body))

;;; Error handling -- returns condition if error happens
(defmacro report-and-ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition) 
            (format *debug-stream* "~%Error: ~A~%" condition)
	    condition)))

          
;;; Proc is a procedure of one arg that returns a list
(defun transitive-closure (thing proc)
  (do ((done nil)
       (fringe (list thing)))
      ((null fringe) done)
    (let ((new (pop fringe)))
      (push new done)
      (dolist (obj (funcall proc new))
	(unless (member obj done)
	  (push obj fringe))))))	

(defmacro let*-debug (forms &body body)
  `(let* ,(mapcar #'(lambda (form)
		      `(,(car form)
			 (progn (format t "~%~A: " ',(car form))
				(print ,(cadr form)))))
		  forms)
     ,@body))

      

(provide :mt-utils)
