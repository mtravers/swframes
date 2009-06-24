(in-package :swframes)

;; hook into old code, including listener
(defun frames::frame-fnamed (name &optional force?)
  (if (typep name 'frame) 
      name
      (if force?
	  (uri name)
	  (frame-named name))))

(defun frames:fname (f)
  (frame-label f))

(defun frames::slotv (frame slot)
  (slotv frame slot))

(defun frames::set-slotv (frame slot value)
  (set-slotv frame slot value))

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

(defun frames::def-indexed-slot (&rest intore)
  )

(defun frames::describe-frame (f)
  (describe-frame f))

(defun frames::rename-frame (f new-name)
  (rename-frame f new-name))

'(defun frames::add-element (frame slot elt &key test)
  (declare (ignore test))
  (add-triple frame slot elt))

