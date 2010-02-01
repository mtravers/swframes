(in-package :sw)

(use-package :lisp-unit)

(define-test deref-test
    (delete-frame #$http://dbpedia.org/resource/Panitumumab)
  (let ((f (intern-uri "http://dbpedia.org/resource/Panitumumab")))
    (dereference f)
    (assert-true (slotv f #$rdfs:comment))
    ;; test transitive dereference 
    (let ((subject (car (slotv f #$skos:subject))))
      (assert-true subject)
      (fill-frame subject :force? t)	; +++ should not be necessary, this stuff still needs work
      (assert-true (slotv subject #$skos:broader))
      (assert-true (> (length (slotv-inverse subject #$skos:subject)) 100)))))

(defun frames-equal (f1 f2)
  (let ((s1 (ht-contents (frame-slots f1)))
	(s2 (ht-contents (frame-slots f2))))
    (and (null (set-difference s1 s2 :test #'equal))
	 (null (set-difference s2 s1 :test #'equal)))
    ))

(defun frame-diff (f1 f2)
  (let ((s1 (ht-contents (frame-slots f1)))
	(s2 (ht-contents (frame-slots f2))))
    (princ "in first, not second:")
    (pprint (set-difference s1 s2 :test #'(lambda (a b) (tree-equal a b :test #'equal))))
    (princ "in second not first:")
    (pprint (set-difference s2 s1 :test #'(lambda (a b) (tree-equal a b :test #'equal)) ))))

;;; +++ fails because of integer/string issues
(define-test deref-round-trip
  (let ((f (gen-random-frame))
	ff fff xml sxml pxml)
    (setf ff (frame-copy f))
    ;; sanity check equality tester
    (assert-true (frames-equal f ff))

    (setf xml (frame-description-xml f))
    (setf sxml (s-xml:print-xml-string xml :pretty t))
    (delete-frame f)
    (setf pxml (parse-xml sxml))
    (setf fff (car (process-rdf-xml pxml)))
    (assert-true (frames-equal ff fff))))

#|
Here we'll keep track of some of the available data sources:

; This one works at least some of the time.
; #$http://www4.wiwiss.fu-berlin.de/drugbank/resource/drugs/DB00022

;#$http://dbpedia.org/page/Aminophyllinen
;  returns HTML with embedded RDFa(?) but it can't be XML parsed.
; But you can substitute in
; (dereference #$http://dbpedia.org/resource/Panitumumab)
; and apparently:
;  http://dbpedia.org/data/Panitumumab.rdf
;  http://dbpedia.org/data/Panitumumab.n3


;;; Times out
;(dereference #$http://bio2rdf.org/proteinlinks/cas:317-34-0)

Many linked data sets here:
http://esw.w3.org/topic/TaskForces/CommunityProjects/LinkingOpenData/DataSets

http://www.rdfabout.com/rdf/usgov/sec/id/cik0001308161
- Returns XML prefaced by two garbage chars (fixed by improving adjust-sparql-string)
- Has some blank nodes, so deal with them now.

http://www.rdfabout.com/rdf/usgov/geo/us

- Returns headers like this:
<?xml version="1.0" encoding="UTF-8"?>

<?xml-stylesheet type="text/xsl" href="http://sw.opencyc.org/xsl/OpenCycOWLCollectionDisplayLatest.xsl"?>

<!DOCTYPE rdf:RDF [
     <!ENTITY ocyc "http://sw.opencyc.org/concept/" >
     <!ENTITY cyc  "http://sw.cyc.com/concept/" >
     <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#" >

Which our XML parser can't handle.  Forget it.

http://www4.wiwiss.fu-berlin.de/bookmashup/books/006251587X
- Works!

#$http://rdf.freebase.com/ns/en.blade_runner
- Works!
- unfortunately freebase doesn't seem to provide a SPARQL endpoint, sigh.

#$diseasome:diseases
- Works (not any more)


NOT WORKING
#$http://www.bbc.co.uk/music/artists/5f6ab597-f57a-40da-be9e-adad48708203#artist

returns some RDF but it gets applied to the wrong frame. Namespace problem?


#$http://wiki.rkbexplorer.com/id/resist
-- gets entity error because of that annoying syntax.

 #$http://www.geonames.org/2950159/about.rdf
-- NS-2 error

http://data.linkedmdb.org/all/director
-- gets data but ins't handled proplery, mot suure why...
  ah, its data is not about itself!  Odd.

|#
