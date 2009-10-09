;;; Codebase uses MCL, but OpenMCL uses CCL.  Argh.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (member :ccl *features*)
      (pushnew :mcl *features*)))

(defvar *swframes-directory* (pathname-directory *load-pathname*))

#+SBCL
(asdf:operate 'asdf:load-op :aserve)

;;; Load SWFRAMES

(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :defaults "swframes.asd"))

(asdf:operate 'asdf:load-op :swframes)

(pushnew :sw *features*)
  

