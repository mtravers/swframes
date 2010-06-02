(in-package :sw)

;;; Some standard namespaces
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *standard-namespaces*
    '(;; SemWeb infrastructure
      ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
      ("xsd" "http://www.w3.org/2001/XMLSchema#")
      ("owl" "http://www.w3.org/2002/07/owl#")

      ;; Common schemas
      ("dc" "http://purl.org/dc/terms/")
      ("foaf" "http://xmlns.com/foaf/0.1/")
      ("skos" "http://www.w3.org/2004/02/skos/core#")

      ;; Local
      ("sw" "http://swframes.org/rdf/")	;+++ well, who should own this?
      ("crx" "http://collabrx.com/rdf/")	;MMM this should move

    ;;; MMM - following should move out of SW

      ;; Linked Data related
      ("linkedct" "http://data.linkedct.org/resource/")
      ("linkedct" "http://data.linkedct.org/resource/linkedct/")
      ("d2r" "http://sites.wiwiss.fu-berlin.de/suhl/bizer/d2r-server/config.rdf#")
      ("dbpedia" "http://dbpedia.org/property/")
      ("drugbank" "http://www4.wiwiss.fu-berlin.de/drugbank/resource/")
      ("dailymed" "http://www4.wiwiss.fu-berlin.de/dailymed/resource/")
      ;; +++ These namespaces are fucked.  Playing with diseasome, should deal with the other wiwiss ones
      ("diseasome" "http://www4.wiwiss.fu-berlin.de/diseasome/resource/diseasome/")
      ("diseasomer" "http://www4.wiwiss.fu-berlin.de/diseasome/resource/")

      ;; Neurocommons -- based on LSW code
      ("wordnet" "http://xmlns.com/wordnet/1.6/")
      ("inoh" "http://www.inoh.org/owl#")
      ("efo" "http://www.ebi.ac.uk/experimentalfactors.owl#")
      ("lsrn" "http://www.lsrn.org/lsrn/schema.owl#")
      ("mir" "http://www.biomodels.net/MIRIAM/")
      ("sn" "http://sharedname.net/sn/")
      ("bpxref" "http://xref.biopax.org/xref/")
      ("ncbilsid" "urn:lsid:ncbi.nlm.nih.gov.lsid.biopathways.org:")
      ("geo" "http://www.w3.org/2003/01/geo/wgs84_pos#")
      ("con" "http://www.w3.org/2000/10/swap/pim/contact#")
      ("wikipedia" "http://www.wikipedia.com/wiki/")
      ("dcterms" "http://purl.org/dc/terms/")
      ("gen" "http://www.w3.org/2006/gen/ont#")
      ("pim" "http://www.w3.org/2000/10/swap/pim/doc#")
      ("go" "http://purl.org/obo/owl/GO#GO_")
      ("oboont" "http://purl.org/obo/owl/")
      ("proto" "http://www.biopax.org/prototype#")
      ("reactome" "http://www.reactome.org/biopax#")
      ("cas" "http://www.biopax.org/xref/cas/#")
      ("kegg" "http://www.biopax.org/xref/kegg/#")
      ("bug" "http://karma.med.harvard.edu/wiki/Debugging_the_bug/bug.owl#")
      ("ecocyc" "http://ecocyc.org/compound/#")
      ("palsson" "http://gcrg.ucsd.edu/iJR904/#")
      ("dolce" "http://www.loa-cnr.it/ontologies/DOLCE-Lite#")
      ("acgt" "http://www.ifomis.org/acgt/1.0#")
      ("internal" "http://mumble.net/#")
      ("ex" "http://example.com/") ("exo" "http://example.org/")
      ("skosx" "http://www.w3.org/2004/02/skos/extensions#")
      ("meta" "http://www.co-ode.org/ontologies/meta/2006/05/15/meta.owl")
      ("senselab" "http://semweb.med.yale.edu/NeuroWeb/owl/senselab#")
      ("cocodat" "http://semweb.med.yale.edu/NeuroWeb/owl/cocodat#")
      ("neurondb" "http://neuroweb.med.yale.edu/senselab/neuron_ontology.owl#")
      ("neuro"
       "http://neuroscientific.net/ont/biopax-level2_neuro_extension.owl#")
      ("kidb" "http://neuroscientific.net/ont/kidb#")
      ("obi" "http://purl.obolibrary.org/obo/")
      ("obo" "http://purl.obolibrary.org/obo/")
      ("span" "http://www.ifomis.org/bfo/1.1/span#")
      ("ro" "http://www.ifomis.org/bfo/1.1/ro#")
      ("snap" "http://www.ifomis.org/bfo/1.1/snap#")
      ("bfo" "http://www.ifomis.org/bfo/1.1#")
      ("bforo" "http://www.ifomis.org/obo/ro/1.0/light#")
      ("protegeowl" "http://protege.stanford.edu/plugins/owl/protege#")
      ("ire" "http://www.loa-cnr.it/ontologies/IRE/IRE.owl#")
      ("edns" "http://www.loa-cnr.it/ontologies/ExtendedDnS.owl#")
      ("dolce" "http://www.loa-cnr.it/ontologies/DOLCE-Lite.owl#")
      ("od" "http://www.loa-cnr.it/ontologies/OD/OntologyDesign.owl#")
      ("infob" "http://www.loa-cnr.it/ontologies/InformationObjects.owl#")
      ("oboinowl" "http://www.geneontology.org/formats/oboInOwl#")
      ("oborel" "http://www.obofoundry.org/ro/ro.owl#")
      ("pato" "http://purl.org/obo/owl/PATO#PATO_")
      ("unit" "http://purl.org/obo/owl/UO#UO_")
      ("so" "http://purl.org/obo/owl/SO#SO_")
      ("cell" "http://purl.org/obo/owl/CL#CL_")
      ("caro" "http://purl.org/obo/owl/CARO#CARO_")
      ("chebi" "http://purl.org/obo/owl/CHEBI#CHEBI_")
      ("evidence" "http://purl.org/obo/owl/ECO#ECO_")
      ("lsrnschema" "http://www.lsrn.org/lsrn/schema.owl#")
      ("biotop" "http://www.ifomis.org/biotop/1.0#")
      ("collections"
       "http://swan.mindinformatics.org/ontology/1.0/20070313/collections.owl#")
      ("swan"
       "http://swan.mindinformatics.org/ontology/1.0/20070410/core.owl#")
      ("scdef" "http://purl.org/science/owl/sciencecommons/")
      ("scto" "http://purl.org/science/owl/thesaurus/")
      ("sclt" "http://purl.org/science/locusthesaurus/")
      ("science" "http://purl.org/science/")
      ("record" "http://purl.org/commons/record/")
      ("mesh" "http://purl.org/commons/record/mesh/")
      ("doap" "http://usefulinc.com/ns/doap#")
      ("ccdb" "http://ccdb.ucsd.edu/SAO/1.1#")
      ("birn-annot"
       "http://purl.org/nbirn/birnlex/ontology/annotation/BIRNLex_annotation_properties.owl#")
      ("obo-annot"
       "http://purl.org/nbirn/birnlex/ontology/annotation/OBO_annotation_properties.owl#")
      ("birn-org"
       "http://purl.org/nbirn/birnlex/ontology/BIRNLex-OrganismalTaxonomy.owl#")
      ("taxon" "http://purl.org/obo/owl/NCBITaxon#NCBITaxon_")
      ("myexp" "http://www.myexperiment.org/rdf/ontology#")
      ("roproposed" "http://purl.org/obo/owl/OBO_REL#")

      ;; Other
      ("nci" "http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#")
      ("go" "http://www.geneontology.org/go#")
      ("go1" "http://www.geneontology.org/dtds/go.dtd#") ;slots use this
      ("bp" "http://www.biopax.org/release/biopax-level2.owl#")

      ))

(dolist (n *standard-namespaces*)
  (register-namespace (car n) (cadr n) t))

)
