(in-package :swframes)

;;; Glue into BioBike in the kludgiest way possible;;; TEMP make them look like Biobike frames

;;; changing the syntax
(defun frame-printer (frame stream ignore)
  (format stream "#$~A" (abbreviate-uri (frame-uri frame))))

(defun frames::frame-name (name)
  (frame-name name))

(defun frames::frame-fnamed (name &rest force?)
  (intern-uri name))

(defun %frame-slots (frame)
  (fill-sframe frame)
  (utils:hash-table-keys (frame-slots frame)))

(defun %frame-inverse-slots (frame)
  (fill-sframe-inverse frame)
  (utils:hash-table-keys (frame-inverse-slots frame)))

(defun frames::slotv (frame slot)
  (slotv frame slot))

(defmethod frames::wob-url ((object frame))
  (utils:formatn
   (utils:one-string
    "/frame"
    (frames::wob-state-variable-values-url-argstring)
    "&name=~A")
   (utils:url-safe-string (frame-uri object))
   ))

(defmethod frames::wob-html ((frame frame))

;  (frame-display-hook frame)		;experimental +++

  ;; Generate HTML output for a frame

  (labels ((emit-section-title (title)
             (html :p (:b (:i (:princ-safe title))) "   " :newline))
           (emit-section-show-hide-control (hide-varname)
             (let ((opposite-value (null (symbol-value hide-varname))))
               (html
                ((:a :href 
                  (progv (list hide-varname) (list opposite-value)
                    (wob-url frame)))
                 (if opposite-value "[Hide]" "[Show]"))
                :newline
                )))
           (emit-section-header (title hide-variable)
             (emit-section-title title)
             (emit-section-show-hide-control hide-variable)))

    (let ((frames::*current-object* frame))

      ;; Generate a table, one row for each slot/value pair in the frame.
      ;; The first column show the slot name, the second shows the value
      ;; in a format controlled by the state variables.

      (html 
       ((:table :border 1 :cellpadding 3 :cellspacing 0)
        :newline
	(:tr
	 (:th "Slot Name")
	 (:th "Slot Value"))
        :newline
        ;; general frame description 
        ;; (should leave out ones done as hierarchy)
        (let ((slots (%frame-slots frame)))
          ;; display slots and values in alphabetical order by slot name.
          (setq slots (sort slots 'string-lessp :key #'frame-name))
          (loop for slot in slots do
	       ;; THis sort of stuff almost surely doesn't want to live in the display code.
;                (when (member #$AlwaysComputedSlot (slotv slot-frame #$isA))
;                  (setq slot-value (slotv frame slot-frame)))
                (html
                 (:tr
                  (:td
                   ((:a :href 
                     (utils:forward-funcall 
                      'wb::make-weblistener-evalstring-url
                      :evalstring
                      (utils:url-safe-string 
                       (prin1-to-string `(slotv ,frame ,slot)))
                      :pkg (utils:forward-funcall 'wb::user-session-id)
                      ))
                    :newline
                    ((:font :color :green) (:princ-safe "#^")))
                   ((:a :href (frames::wob-url slot))
                    (:princ-safe (frame-name slot)))
		   :newline
                    )
                  (:td
		   (emit-slot-value nil (slotv frame slot))
		   ))
                 :newline
                 )))))


      ;; Inverses
      (html 
	(:h3 "Inbound links")
       ((:table :border 1 :cellpadding 3 :cellspacing 0)
        :newline
	(:tr
	 (:th "Subject")
	 (:th "Slot"))
        :newline
        ;; general frame description 
        ;; (should leave out ones done as hierarchy)
        (let ((slots (%frame-inverse-slots frame)))
          ;; display slots and values in alphabetical order by slot name.
          (setq slots (sort slots 'string-lessp :key #'frame-name))
          (loop for slot in slots do
	       ;; THis sort of stuff almost surely doesn't want to live in the display code.
;                (when (member #$AlwaysComputedSlot (slotv slot-frame #$isA))
;                  (setq slot-value (slotv frame slot-frame)))
                (html
                 (:tr
                  (:td
		   (emit-slot-value nil (slotv-inverse frame slot))
		   )
                  (:td
                   ((:a :href 
                     (utils:forward-funcall 
                      'wb::make-weblistener-evalstring-url
                      :evalstring
                      (utils:url-safe-string 
                       (prin1-to-string `(slotv ,frame ,slot)))
                      :pkg (utils:forward-funcall 'wb::user-session-id)
                      ))
                    :newline
                    ((:font :color :green) (:princ-safe "#^")))
                   ((:a :href (frames::wob-url slot))
                    (:princ-safe (frame-name slot)))
		   :newline
                    ))
                 :newline
                 )))))


#|
      (when (slotv frame #$isA)
        (emit-section-header "Parents" '*hide-parents*)
        (emit-parents frame #$isA))
      (when (slotv frame #$subclasses)
        (emit-section-header "Children" '*hide-children*)
        (emit-hierarchy frame #$subclasses))
      (when (slotv frame #$partOf)
        (emit-section-header "SuperParts" '*hide-superparts*)
        (emit-parents frame #$partOf))
      (when (slotv frame #$parts)
        (emit-section-header "Parts" '*hide-parts*)
        (emit-hierarchy frame #$parts))
|#

      )))

(defun emit-slot-value (slot-frame slot-value)
;;   (vif (html-generator
;; 	(and (not *print-lispy*) 
;; 	     (slotv slot-frame #$HTMLGenerator)))
;;        (funcall html-generator slot-value)
       (frames::emit-value slot-value)
       )

(defmethod frames::emit-value 
           ((object frame) &optional (print-limit nil))
  (declare (ignore print-limit))
  (html
   ((:a :href (frames::wob-url object))
    "#$"
    (:princ-safe (frame-name object)))
   :newline
   ))
