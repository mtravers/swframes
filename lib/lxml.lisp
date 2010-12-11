(defpackage "LXML"
  (:documentation "Utilities for searching LXML structures.")
  (:use "MT" "COMMON-LISP"))
(in-package "LXML")

#|
Utilities for searching LXML structures.
LXML format is described here: http://opensource.franz.com/xmlutils/xmlutils-dist/pxml.htm
|#

(export '(lxml-subelement lxml-subelements lxml-all-subelements 
	  lxml-attribute lxml-attributes
	  lxml-tag lxml-tag-p 
	  lxml-find-element lxml-find-element-with-tag lxml-find-elements-with-tag
	  lxml-find-element-with-attribute lxml-find-elements-with-attribute))

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

(defun lxml-subelement (lxml tag)
  (dolist (elt (lxml-subelements lxml))
    (when (lxml-tag-p tag elt)
      (return elt))))

(defun lxml-attribute (lxml attr)
  (and (listp lxml)
       (cadr (member attr (lxml-attributes lxml)))))

(defun lxml-attributes (lxml)
  (and (listp lxml)
       (listp (car lxml))
       (cdr (car lxml))))

;;; does anything call this? (yes, sparql)
(defun lxml-find-element (lxml tag)
  (find tag (cdr lxml) :test #'lxml-tag-p))

(defun lxml-tag (elt)
  (cond ((symbolp elt)
         elt)
        ((and (listp elt)
	      (listp (car elt)))
         (caar elt))
	((listp elt)
	 (car elt))
	(t nil)))

(defun lxml-tag-p (tag elt)
  (or (eq tag elt)
      (if (listp (car elt))
	  (eq tag (caar elt))
	  (eq tag (car elt)))))

(defun lxml-find-element-with-tag (element tag &rest more-tags)
  (if (eq tag (lxml-tag element))
      (if more-tags
	  (apply 'lxml-find-element-with-tag element more-tags)
	  element)
      (dolist (sub (lxml-subelements element))
	(awhen (apply 'lxml-find-element-with-tag sub tag more-tags)
	       (return it)))))

;;; rewrite this one too ++
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

;;; This finds all <tag> elements, recursively, including tags within tags.  
(defun lxml-find-all-elements-with-tag (element tag)
  (let ((subs (mapappend #'(lambda (sub) (lxml-find-all-elements-with-tag sub tag))
			 (lxml-subelements element t))))
    (if (eq tag (lxml-tag element))
	(cons element subs)
	subs)))


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

