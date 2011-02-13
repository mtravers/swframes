(in-package :sw)

;;; +=========================================================================+
;;; | Copyright (c) 2009, 2010  Mike Travers and CollabRx, Inc                |
;;; |                                                                         |
;;; | Released under the MIT Open Source License                              |
;;; |   http://www.opensource.org/licenses/mit-license.php                    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  Mike Travers


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
    (frame-copy frame :new-frame version-frame :omit-slots (list #$rdf:type))
;    (frame-delete-slot version-frame #$rdf:type)
    (setf (ssv version-frame #$sw:version_of) frame)
    (setf (ssv version-frame #$sw:version) version-number)
    (setf (ssv version-frame #$sw:timestamp) (now))
    (setf (ssv version-frame #$sw:writer) (current-user))
    (setf (ssv frame #$sw:previous_version) version-frame)
    version-frame))

;;; NL overwrites this -- the idea is that SW should be independent of a user-management scheme
(defun current-user ()
  #$sw:TestUser)

(defun write-frame-versioned (frame)
  (write-frame (version-frame frame))
  (write-frame frame))

;;; Test if frame has actually changed before writing a version

;;; Get a history
(defun frame-version-history (frame)
  (sort (copy-list (slotv-inverse frame #$sw:version_of)) #'> :key (ssv-accessor #$sw:version)))




    
