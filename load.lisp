;;; Note: this is for standalone loading of :swframes (outside of 3rdwheel).  

;;; Setup

(require :asdf-install)
(dolist (l asdf-install::*locations*)	;Why oh lord is this necessary?
  (push (cadr l) asdf::*central-registry*))

(load "/Users/Biobike/3rdwheel/3utils/3utils.asd") ;+++ MMM
(load "/Users/Biobike/3rdwheel/mtlisp/mtlisp.asd")

(mapc #'delete-file (directory "/Users/Biobike/3rdwheel/swframes/*.dx64fsl"))
;;; Load SWFRAMES

(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :defaults "swframes.asd"))

(asdf:operate 'asdf:load-op :swframes)

(pushnew :swframes *features*)
  
;;; Run tests

(defun run-tests ()
  (shadowing-import '(lisp-unit:set-equal) :sw)
  (use-package :lisp-unit :sw)
  (dolist (f (directory (make-pathname :defaults (append (pathname-directory *load-pathname*) '("t")) :name "t-*" :type "lisp")))
    (load f))
  (in-package :sw)
  (lisp-unit:run-tests))
