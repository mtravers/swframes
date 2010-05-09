(in-package :sw)

(export '(find-frame-by-homology))

;;; Word homology 
;;; Warning: setting fill? to t without limiting the target set will be pretty slow!
(defun find-frame-by-homology (string &key (targets (all-frames)) (slot #$rdfs:label) (limit 0.6) (fill? nil))
  (let ((cstring (compile-word string)))
    (sort (collecting
	    (mapcar #'(lambda (frame)
			(let ((score (score-homology-fast cstring 
								 (ssv frame slot fill?))))
			  (when (> score limit) 
			    (collect (list frame score (ssv frame slot nil))))))
		    targets))
	  #'>
	  :key #'cadr)))

;;; Word Homology package. Orignally Jeff's; Side-graded by Mike.
;;; 'Fast' versions by JP.
       
;;; Now prepends a null so that single char names work (and start of term 
;;; is more significant.)

(defconstant null-char (code-char 0) "#\Null, except it's not portable")

(defun compile-word (word)
  "Turns a word into a useful representation for word homology"
  (if (simple-string-p word)
      (compile-word-fast word)
    (loop for p from 0 to (- (length word) 1)
	  as l1 = (char-upcase (aref word p))
	  as l2 =  (if (zerop p) null-char (char-upcase (aref word (1- p))))
	  as ln1 = (char-code l1)
	  as ln2 = (char-code l2)
	  collect (+ ln1 (* ln2 128))
	  )))

(defun compile-word-fast (word)
  (unless (simple-string-p word)
    (error "COMPILE-WORD-FAST only accepts *SIMPLE* strings!"))
  (let ((word word))
    #.(optimization-declaration)
    (declare (simple-string word))
    (let ((prev-cc (char-code null-char)))
      (declare (fixnum prev-cc))
      (loop for p fixnum from 0 to (the fixnum (1- (length word)))
	    as next-cc fixnum = (char-code (char-upcase (schar word p)))
	    as result = (the fixnum (+ next-cc (the fixnum (ash prev-cc 7))))
	    do (setq prev-cc next-cc)
	    collect result
	    ))))

#+test
(defun time-cw (w n)
  (time (dotimes (j n) (compile-word w)));
  (time (dotimes (j n) (compile-word-fast w))))

;;; Takes two words or compiled words, returns the score of how close
;;; they matched based on co-occuring pairs.

(defun score-homology (word1 word2)
  "Ranks two strings as to how similar they are according to a homology metric"
  (if (or (and (simple-string-p word1) (simple-string-p word2))
          (and (not (stringp word1)) (not (stringp word2))))
      (score-homology-fast word1 word2)
    (let* ((w1 (if (listp word1) word1 (compile-word word1)))
	   (w2 (if (listp word2) word2 (compile-word word2)))
	   (o1 (loop for l1 in w1 if (member l1 w2) sum 1))
	   (o2 (loop for l2 in w2 if (member l2 w1) sum 1))
	   (l1 (length w1))
	   (l2 (length w2)))
      (if (and (> l1 1) (> l2 1))
	  (/ (+ (/ o1 l1) (/ o2 l2)) 2.0)
        0.0				;if either word is of length <2, 
        ))))

(defvar *word-homology-array* nil)

(defun score-homology-fast (word1 word2)
  #.(optimization-declaration)
  (unless (and (or (listp word1) (typep word1 'simple-string))
               (or (listp word2) (typep word2 'simple-string)))
    (error "SCORE-HOMOLOGY-FAST only accepts simple strings."))
  (when (null *word-homology-array*)
    (setq *word-homology-array* 
          (make-array (list #.(expt 2 14))
            :element-type '(unsigned-byte 8)
            :initial-element 0
            )))
  (let ((wha *word-homology-array*) 
        (x (if (listp word1) word1 (compile-word-fast word1)))
        (y (if (listp word2) word2 (compile-word-fast word2)))
        (cx 0) (cy 0) (lx 0) (ly 0))
    (declare (type (simple-array (unsigned-byte 8) 1) wha))
    (declare (fixnum cx cy lx ly) (list x y))
    (dolist (xe x) 
      (declare (fixnum xe)) 
      (setf (aref wha xe) 1)
      (setq lx (the fixnum (1+ lx))))
    (dolist (ye y) 
      (declare (fixnum ye))
      (setq ly (the fixnum (1+ ly)))
      (if (plusp (aref wha ye)) (setq cy (the fixnum (1+ cy)))))
    (dolist (xe x) (declare (fixnum xe)) (setf (aref wha xe) 0))
    (dolist (ye y) (declare (fixnum ye)) (setf (aref wha ye) 1))
    (dolist (xe x)
      (declare (fixnum xe))
      (if (plusp (aref wha xe)) (setq cx (the fixnum (1+ cx)))))
    (dolist (ye y) (declare (fixnum ye)) (setf (aref wha ye) 0))
    (if (or (< lx 2) (< ly 2))
        0.0
      (/ (+ (/ (float cx 0.0) (float lx 0.0))
           (/ (float cy 0.0) (float ly 0.0)))
        2.0))
    ))

#+test
(defun time-sh (w1 w2 n)
  (let ((l1 (compile-word-fast w1)) (l2 (compile-word-fast w2)))
    (time (dotimes (j n) (score-homology l1 l2)))
    (time (dotimes (j n) (score-homology-fast l1 l2)))))

;;; More flexible version using an accessor into frames that
;;; is a function on the elements of list that yields a word.

(defun word-homology (word list &optional (n 3) (accessor #'identity))
  #.(doc
     "Score how WORD compares with the words in LIST using character-pair "
     "homology and return the top N words in LIST along with their scores")
  ;; list of (score word) elements, smallest first
  (let* ((topn '())			
	 (compiled-word (compile-word word)))
    ;; If the new word is more than the lowest of the ones in the set, 
    ;; replace that one with the new one.
    (dolist (entry list)
      (let* ((score (score-homology compiled-word (funcall accessor entry))))
	(cond 
         ((< (length topn) n)
          (push (list score entry) topn)
          (setf topn (sort (cons (list score entry) (cdr topn)) 
                           #'< :key #'car)))
         ((> score (car (car topn)))
          (setf topn (sort (cons (list score entry) (cdr topn)) 
                           #'< :key #'car))))))
    (nreverse topn)))


(defun word-homology-fast (word list &optional (n 3) (accessor #'identity))
  #.(doc
     "Score how WORD compares with the words in LIST using character-pair "
     "homology and return the top N words in LIST along with their scores")
  #.(optimization-declaration)
  (declare (fixnum n))
  (let ((topn nil) 
        (topn-length 0)
        (compiled-word (if (listp word) word (compile-word word))))
    (declare (fixnum topn-length))
    (dolist (entry list)
      (let ((score 
             (score-homology-fast compiled-word (funcall accessor entry))))
        (cond
         ((< topn-length n) 
          (incf topn-length)
          (setq topn (insert-into-ordered-list 
                      (list score entry) #'< topn :key #'first)))
         ((> score (caar topn))
          (setq topn (insert-into-ordered-list
                      (list score entry) #'< (cdr topn) :key #'first))))))
    (nreverse topn)))



(defun insert-into-ordered-list (elem test list &key (key #'identity))
  #.(doc
     "A destructively modified LIST with ELEM inserted is returned. "
     "It is assumed that LIST is already ordered wrt TEST and KEY.")
  (flet ((compare (list-element) 
           (if (eq key #'identity)
               (funcall test elem list-element)
             (funcall test (funcall key elem) (funcall key list-element)))))
    (block exit
      (cond
       ((null list) (list elem))
       ((null (cdr list))
        (if (compare (first list)) (cons elem list) (nconc list (list elem))))
       (t 
        ;; Goes at beginning
        (when (compare (first list)) (return-from exit (cons elem list)))
        (let* ((last-cons-cell nil))
          (do ((sublist list (cdr sublist))) ((null (cdr sublist)))
            (when (compare (second sublist))
              ;; Goes in middle: list surgery
              (let ((cdr-cell (cdr sublist))
                    (elem-cons-cell (list elem)))
                (setf (cdr sublist) elem-cons-cell)
                (setf (cdr elem-cons-cell) cdr-cell)
                (return-from exit list)))
            (setq last-cons-cell (cdr sublist)))
          ;; Must go at the end
          (setf (cdr last-cons-cell) (list elem))
          list
          ))))))
