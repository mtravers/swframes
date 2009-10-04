;;; Codebase uses MCL, but OpenMCL uses CCL.  Argh.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (member :ccl *features*)
      (pushnew :mcl *features*)))

;;; Set up ASDF libraries
(require :asdf)

;;; Other lisps can use the same source as SBCL (+++ probably not right for BioBike)
#-SBCL
;(pushnew #p"/usr/local/asdf-install/site-systems/" asdf:*central-registry*)
(pushnew #p"~/.sbcl/systems/" asdf:*central-registry*)

#+(or :ACL :ABCL)
(pushnew "~/.asdf-install-dir/systems/" asdf:*central-registry* :test #'equal)

#+SBCL
(asdf:operate 'asdf:load-op :aserve)

#-SBCL
(asdf:operate 'asdf:load-op :s-xml)

#+SBCL
(require :s-xml)

#+:SBCL
(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :defaults "sbcl-patch.lisp"))


;;; Load MT utils

'(require "mtlisp"
	 (make-pathname :directory (append (pathname-directory *load-pathname*) '("mt-lisp-utils"))
			:defaults "mtlisp.asd"))

;;; Load SWFRAMES

(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :defaults "swframes.asd"))

(asdf:operate 'asdf:load-op :swframes)

(pushnew :sw *features*)

  
