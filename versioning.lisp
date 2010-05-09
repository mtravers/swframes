(in-package :sw)

(export '(version-frame write-frame-versioned frame-version-history current-user))
#|
Versioning theory:

- frames (objects) need to retain their identity
- when a versioned frame is written out, 
-- its old contents get attached to a new frame
  ---- but without the type field, so we don't get faux objects
  --- with a link to the master
  --- with time and user information

Alternate idea: do it all in the db with a wildcard insert...much faster, not as clean but a good trick I guess

Caveats:  only works if you use write-frame-versioned.  Other ways of writing content like write-slot won't make a version.

|#

;;; Spawn a version frame (copying the contents of FRAME)
;;; No db writes
(defun version-frame (frame)
  (let* ((last-version (ssv frame #$sw:previous_version))
	 (version-number (if last-version (1+ (ssv last-version #$sw:version)) 0))
	 (version-uri (string+ (frame-name frame) "/v/" (fast-string version-number)))
	 (version-frame (intern-uri version-uri)))
    (frame-copy frame :new-frame version-frame)
    (frame-delete-slot version-frame #$rdf:type)
    (setf (ssv version-frame #$sw:version_of) frame)
    (setf (ssv version-frame #$sw:version) version-number)
    (setf (ssv version-frame #$sw:timestamp) (now))
    (setf (ssv version-frame #$sw:writer) (current-user))
    (setf (ssv frame #$sw:previous_version) version-frame)
    version-frame))

;;; NL overwrites this -- the idea is that SW is independent of a user-management scheme
(defun current-user ()
  #$sw:TestUser)

(defun write-frame-versioned (frame)
  (write-frame (version-frame frame))
  (write-frame frame))

;;; Test if frame has actually changed before writing a version

;;; Get a history
(defun frame-version-history (frame)
  (sort (copy-list (slotv-inverse frame #$sw:version_of)) #'> :key (ssv-accessor #$sw:version)))




    
