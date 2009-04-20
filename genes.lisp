(in-package :sw)

;;; Get gene frames from local database by name

(defun local-gene-reactome (gene-name &key human?)
  (do-sparql-one-var
      *collabrx-sparql*
    `(:select (?s) ()
	      (?s #$http://purl.org/science/ontology/reactome/name ,gene-name)
	      ,@(if human?
		    `((?s #$http://purl.org/science/ontology/reactome/species ?species)
		      (?species #$rdfs:label "Homo sapiens")))
	      )))

;;; using NCBI records instead, there are more of them
;;; the species names aren't in the DB, but the URIs are 9606 is human
(defun local-gene-ncbi (gene-name &key human?)
  (do-sparql-one-var
      *collabrx-sparql*
    `(:select (?s) ()
	      (?s #$http://purl.org/science/owl/sciencecommons/ggp_has_symbol ,gene-name)
	      ,@(if human?
		    `((?s #$http://purl.org/science/owl/sciencecommons/ggp_from_species_described_by
			  #$http://purl.org/commons/record/ncbi_taxonomy/9606))))
    ))

(defun local-gene-nci (gene-name)
  (do-sparql-one-var
      *collabrx-sparql*
    `(:select (?s) ()
	      (?s #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Synonym ,gene-name)
	      (?s #$rdfs:subClassOf #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Gene)
	      )
    ))

(defun local-gene-nci (gene-name)
  (do-sparql-one-var
      *collabrx-sparql*
    `(:select (?s) ()
	      (?s #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Synonym ,gene-name)
	      (?s #$rdfs:subClassOf #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Gene)
	      )
    ))


;;; "Gene or Genome" "Receptor"
(defun local-nci (name &key category)
  (do-sparql-one-var
      *collabrx-sparql*
    `(:select (?s) ()
	      (?s #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Synonym ,name)
	      ,@(if category
		    `((?s #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Semantic_Type ,category)))
	      )
    ))


(defun local-nci-2 (name)
  (do-sparql
      *collabrx-sparql*
    `(:select (?s ?category) ()
	      (?s #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Synonym ,name)
	      (?s #$http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#Semantic_Type ?category))))




