(in-package :sw)

#|
LXML format is described here: http://opensource.franz.com/xmlutils/xmlutils-dist/pxml.htm
|#

(defun lxml-subelements (lxml &optional elts-only)
  (if (listp lxml)
      ;; sometimes whitespace text chunks get inserted
      (if elts-only
	  (filter #'consp (cdr lxml))
	  (cdr lxml))
      nil))

(defun lxml-all-subelements (lxml &optional elts-only)
  (if (listp lxml)
      ;; sometimes whitespace text chunks get inserted
      (if elts-only
          (filter #'consp (cdr lxml))
          (cdr lxml))
      nil))

(defun lxml-attributes (lxml)
  (and (listp (car lxml))
       (cdr (car lxml))))

(defun lxml-subelement (lxml tag)
  (dolist (elt (lxml-subelements lxml))
    (when (lxml-tag-p tag elt)
      (return elt))))

(defun lxml-attribute (lxml attr)
  (and (listp lxml)
       (cadr (member attr (lxml-attributes lxml)))))

;;; does anything call this? (yes, sparql)
(defun lxml-find-element (lxml tag)
  (find tag (cdr lxml) :test #'lxml-tag-p))

(defun lxml-tag (elt)
  (cond ((symbolp elt)
         elt)
        ((listp (car elt))
         (caar elt))
        (t (car elt))))

(defun lxml-tag (elt)
  (cond ((symbolp elt)
         elt)
        ((listp (car elt))
         (caar elt))
        (t (car elt))))

(defun lxml-tag-p (tag elt)
  (or (eq tag elt)
      (if (listp (car elt))
	  (eq tag (caar elt))
	  (eq tag (car elt)))))

;;; these are from LSW, I think.
(defun lxml-find-element-with-tag (element tag &rest more-tags)
  (let ((found
         (loop with q = (list element)
            for this = (pop q)
            for (name . children) = this
            if (cond ((consp name)
                      (tag-equal (car name) tag))
                     (t (tag-equal name tag)))
            do (return this)
            else do (when (listp children) (setq q (nconc q (remove-if-not 'consp children))))
            while q)))
    (if (and found more-tags)
        (apply 'lxml-find-element-with-tag found more-tags)
        found)))

(defun lxml-find-elements-with-tag (element tag &rest more-tags)
  (let ((found
         (loop with q = (list element)
            for this = (pop q)
            for (name . children) = this
            if (cond ((consp name)
                      (tag-equal (car name) tag))
                     (t (tag-equal name tag)))
            collect this
            else do (when (listp children) (setq q (nconc q (remove-if-not 'consp children))))
            while q)))
    (if (and found more-tags)
        (loop for el in found append (apply 'lxml-find-elements-with-tag el more-tags))
        found)))

(defun tag-equal (a b)
  (equal (string a) (string b)))

(defun lxml-find-element-with-attribute (xml tag attribute value)
  (car (lxml-find-elements-with-attribute xml tag attribute value)))

(defun lxml-find-elements-with-attribute (xml tag attribute value)
  (filter #'(lambda (elt)
	      (equal value
		     (lxml-attribute elt attribute)))
	  (lxml-find-elements-with-tag xml tag)))

(defun xml-clean (xml)
  (cond ((null xml) nil)
        ((listp xml)
         (filter-out #'null
		     (mapcar #'xml-clean xml)))
        ((stringp xml)
         (let ((trimmed (string-trim-whitespace xml)))
           (if (equal trimmed "")
               nil
             trimmed)))
        (t xml)))

