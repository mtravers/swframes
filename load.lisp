;;; Note: this is for standalone loading of :swframes (outside of 3rdwheel).  

;;; Setup

(require :asdf-install)
(dolist (l asdf-install::*locations*)	;Why oh lord is this necessary?
  (push (cadr l) asdf::*central-registry*))

(load "/Users/Biobike/3rdwheel/3utils/3utils.asd") ;+++
(load "/Users/Biobike/3rdwheel/mtlisp/mtlisp.asd")


;;; Load SWFRAMES

(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :defaults "swframes.asd"))

(asdf:operate 'asdf:load-op :swframes)

(pushnew :swframes *features*)
  

