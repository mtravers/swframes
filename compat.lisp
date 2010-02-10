(in-package :swframes)

#|
The code here attempts to match the API of the older frame system.  As you might expect, it sort of works but there
are many rough edges.

If you patch the web code by hand (see the end of this file) it works well enough for the old BioBike frame browser
to run
|#

(defun frames::make-frame (string)
  (make-frame string))

;; hook into old code, including listener
(defun frames::frame-fnamed (name &optional force?)
  (if force?
      (make-frame name)
      (frame-named (expand-uri name))))

(defun frames:fname (f)
  (frame-label f))

(defun frames::slotv (frame slot)
  (msv frame slot))

(defun frames::set-slotv (frame slot value)
  (set-msv frame slot value))

(defun frames::%frame-slots (frame)
  (awhen (frame-slots frame)
	 (ht-contents it)))

(defun frames::frame-slots-of (frame)
  (%frame-slots frame))

;;; temp, we might want to get indexes working...
;;; but this allows slot-lookup to work
(defun frames::slot-index (slot)
  nil)

;;; +++ this redefinition doesn't work unless you recompile uses, because it's a macro 
(defmacro frames::for-all-frames ((var) &body body)
  `(for-all-frames (,var) ,@body))

(defun frames::framep (x)
  (frame-p x))

(defun frames::isframe? (x)
  (frame-p x))

;;; +++ no-op for now
(defun frames::create-inverse-slot (inverse-slot slot pf af)
  )

;;; +++ no-op for now
(defun frames::defslot 
       (frame 
        &key
        (base #$crx:slot)
        (applicable-to nil) ;default is applies to everything
        (value-type nil) ;default is anything
        (set-valued? nil)
        )
  )

(defun frames::def-indexed-slot (&rest ignore)
  )

(defun frames::describe-frame (f)
  (describe-frame f))

(defun frames::rename-frame (f new-name)
  (rename-frame f new-name))

'(defun frames::add-element (frame slot elt &key test)
  (declare (ignore test))
  (add-triple frame slot elt))

(defun frames::frame-name (frame)
  (frame-name frame))

#|
In Webdefs/webframes-display.lisp

Change methods that refer to class %frame to sw:frame (emit-value, wob-html, wob-url)

Add call to fill-frame in wob-html
|#
