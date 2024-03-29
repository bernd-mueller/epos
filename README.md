[![DOI](https://zenodo.org/badge/150077421.svg)](https://zenodo.org/badge/latestdoi/150077421)[![](https://cranlogs.r-pkg.org/badges/grand-total/epos)](https://cran.r-project.org/package=epos)


# epos
Analysis and Visualization of statistical information derived from biomedical named entities that were automatically extracted with a UIMA-based text mining workflow on the corpus of BioASQ. The major scope of this R package is the comparison of drug names that co-occur with entities from epilepsy ontologies in the same documents.

Basically, the UIMA-based workflow takes as input dictionaries containing biomedical entities with synonyms for identifying them in documents of the BioASQ corpus. The epilepsy ontologies EpSO, ESSO, EPILONT, EPISEM and FENICS are used for creating epilepsy-related dictionaries. The current version of the DrugBank open data vocabulary is taken for creating a dictionary for drug names (https://go.drugbank.com/releases/latest#open-data).

The UIMA-based text mining workflow is described in the following three publications:

Müller B, Hagelstein A (2016) Beyond Metadata – Enriching Life Science Publications in LIVIVO with Semantic Entities from the Linked Data Cloud. In: Joint Proceedings of the Posters and Demos Track of the 12th International Conference on Semantic Systems – SEMANTiCS2016 and the 1st International Workshop on Semantic Change & Evolving Semantics SuCCESS’16, Leipzig, Germany <doi:10.4126/FRL01-006408558>

Müller B, Hagelstein A, Gübitz T (2016) Life Science Ontologies in Literature Retrieval: A Comparison of Linked Data Sets for Use on Semantic Search on a Heterogeneous Corpus. In: Proceedings of the 20th International Conference on Knowledge Engineering and Knowledge Management. Bologna, Italy <doi:10.1007/978-3-319-58694-6_22>

Müller B, Rebholz-Schuhmann D (2020) Selected Approaches Ranking Contextual Term for the BioASQ Multi-label Classification (Task6a and 7a). In: Proceedings of the Joint European Conference on Machine Learning and Knowledge Discovery in Databases ECML PKDD 2019, Würzburg, Germany <doi:10.1007/978-3-030-43887-6_52>

Müller, B., Castro, L.J. & Rebholz-Schuhmann, D. Ontology-based identification and prioritization of candidate drugs for epilepsy from literature. J Biomed Semant 13, 3 (2022). <doi:10.1186/s13326-021-00258-w>

Please cite this work as:

Bernd Müller. R-package for the Analysis and Visualization of Epilepsy Ontologies' Similarities According to Co-Occurring Drug Names in the 2021 BioASQ corpus. ZENODO, 10.5281/zenodo.4682869