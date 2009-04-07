(in-package :swframes)

;; hook into old code, including listener
(defun frames::frame-fnamed (name &optional force?)
  (declare (ignore force?))
  (frame-named name))

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

(defmacro frames::for-all-frames ((var) &body body)
  `(for-all-frames (,var) ,@body))

(defun frames::framep (x)
  (frame-p x))

(defun frames::isframe? (x)
  (frame-p x))
