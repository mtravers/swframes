(in-package :sw)

(defun jp ()
  (dotimes (n 10000000) 
    (let* ((uri (format nil "crx:test/~A" n))
	   (frame (intern-uri uri)))
      (setf (ssv frame #$crx:test/slot) (random 100)))))

(defun jp-2 (n)
  (dotimes (i n)
    (make-frame (format nil "crx:test/~A" i) )))
