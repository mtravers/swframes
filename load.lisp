(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :defaults "swframes.asd"))

(asdf:operate 'asdf:load-op :swframes)

(pushnew :sw *features*)
