(in-package :swframes)

(import  'wb::html)

(wb::publish 
 :path wb::*weblistener-frames-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (wb::request-query req))
;;; CCC
;          (package-name (url-parameter-value :pkg input))
	  (package-name (wb::cookie-package req))
          (package-symbol (keywordize package-name))
          (name (wb::url-parameter-value :name input))
          )
     (wb::execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (frames::with-wob-state-variable-values (req)
          (html-for-browse-frame name)))))))

(defun html-for-browse-frame (name)
  (let* ((frame (intern-uri name))
         (title (formatn "Frame #$~A" name)))
    (fill-sframe frame)
    (wb::with-standard-weblistener-page-header (title)
      (if frame
          (progn
;            (emit-min-less-more-max-control frame)
;            (emit-block-format-or-lisp-format-control frame)
            (html (:princ "    "))
            (when (forward-funcall 'wb::user-session-id)
              (html 
               ;; Send the frame back as a faked user typein.
               ((:a :href 
                 (forward-funcall 
                  'wb::make-weblistener-evalstring-url
                  :evalstring
                  (formatn 
                   "%23%24~A" (url-safe-string (frame-name  frame)))
                  :pkg (forward-funcall 'wb::user-session-id)
                  ))
                (:princ-safe "Frame->Listener"))
               :newline))
            (html :br :br)
            (frames::wob-html frame)
            )
        (html (:i "Frame not found.  (May have been uninterned)") :newline)
        )))
   ;; Any time we use the frame browser we purge the WOB table
   ;; of timed-out references so that they don't accumulate forever.
;   (purge-wob-table (* 3600 3))
   )


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
                    (frames::wob-url frame)))
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
        (let ((slots (copy-list (%frame-slots frame))))
          ;; display slots and values in alphabetical order by slot name.
          (setq slots (sort slots 'string-lessp
                            :key #'frame-label))
          (loop for slot-frame in slots do
	       ;; THis sort of stuff almost surely doesn't want to live in the display code.
	       (setq slot-value (slotv frame slot-frame))
                (html
                 (:tr
                  (:td
                   ((:a :href 
                     (forward-funcall 
                      'wb::make-weblistener-evalstring-url
                      :evalstring
                      (url-safe-string 
                       (prin1-to-string `(slotv ,frame ,slot-frame)))
                      :pkg (forward-funcall 'wb::user-session-id)
                      ))
                    :newline
                    ((:font :color :green) (:princ-safe "#^")))
                   ((:a :href (frames::wob-url slot-frame))
                    (:princ-safe (frame-name slot-frame)))
                   :newline
                    )
                  (:td
		   (frames::emit-slot-value slot-frame slot-value)
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


(defmethod frames::wob-url ((object frame))
  (formatn
   (one-string
    "/frame"
;    (wob-state-variable-values-url-argstring)
    "?name=~A")
   (url-safe-string (frame-name object))
   ))

