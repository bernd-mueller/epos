[![Build Status](https://travis-ci.org/ZBMEDLABS/epilepsyontologysimilarities.svg?branch=master)](https://travis-ci.org/ZBMEDLABS/epilepsyontologysimilarities)

# epilepsyontologysimilarities
This is an R package that is developed for analyzing and visualizing statistical information of biomedical named entities that were automatically identified with a UIMA-based text mining workflow on the corpus of LIVIVO. The major scope of this R package is the comparison of drug names that co-occur with entities of epilepsy ontologies in documents of the LIVIVO corpus.

The UIMA-based text mining workflow is described in the following two publications:

Müller B, Hagelstein A (2016) Beyond Metadata – Enriching Life Science Publications in LIVIVO with Semantic Entities from the Linked Data Cloud (Joint Proceedings of the Posters and Demos Track of the 12th International Conference on Semantic Systems – SEMANTiCS2016 and the 1st International Workshop on Semantic Change & Evolving Semantics (SuCCESS’16), co-located with the 12th International Conference on Semantic Systems (SEMANTiCS 2016, Leipzig, Germany))

Müller B, Hagelstein A, Gübitz T (2016) Life Science Ontologies in Literature Retrieval: A Comparison of Linked Data Sets for Use on Semantic Search on a Heterogeneous Corpus. In: Proceedings of the 20th International Conference on Knowledge Engineering and Knowledge Management. Bologna, Italy

Basically, the UIMA-based workflow takes as input a dictionary containing biomedical entities with synonyms for identifying them in documents of the LIVIVO corpus. The epilepsy ontologies EpSO, ESSO, and EPILONT are used for creating three different dictionaries for epilepsy. The current version of DrugBank is taken for creating a dictionary for drug names. The results of the text mining workflow is written into a MongoDB databases. Finally, the information in the MongoDB is aggregated for creating lists of drug names that are sorted by their frequency of documents in which they co-occur with entities from each of the epilepsy dictionaries. These three frequency sorted lists are saved as R-objects in inst/resources/[tepso.rda,tesso.rda,tepi.rda].

The major entry point of this R package is the execution of the function main () that calls all relevant steps for creating a final results table.


% latex table generated in R 3.5.1 by xtable 1.8-2 package
% Tue Sep 04 10:38:12 2018
\begin{table}[ht]
\centering
\begin{tabular}{rllllllllllll}
  \hline
 & Rank & Intersection & DrugName & N03 & N05 & N06 & N01 & N02 & N04 & N07 & Lancet & DSE \\ 
  \hline
1 & 1 & EpSO\_ESSO\_EPILONT & Phenol &  &  &  & x &  &  &  &  &  \\ 
  2 & 2 & EpSO\_ESSO\_EPILONT & Ketamine &  &  &  & X &  &  &  &  &  \\ 
  3 & 3 & EpSO\_ESSO\_EPILONT & Lithium &  & X &  &  &  &  &  &  &  \\ 
  4 & 4 & EpSO\_ESSO\_EPILONT & Choline &  &  &  &  & X &  &  &  &  \\ 
  5 & 5 & EpSO\_ESSO\_EPILONT & L-Tryptophan &  &  & X &  &  &  &  &  &  \\ 
  6 & 6 & EpSO\_ESSO\_EPILONT & Morphine &  &  &  &  & X &  &  &  &  \\ 
  7 & 7 & EpSO\_ESSO\_EPILONT & Pethidine &  &  &  &  & X &  &  &  &  \\ 
  8 & 8 & EpSO\_ESSO\_EPILONT & Diazepam &  & X &  &  &  &  &  &  & X \\ 
  9 & 9 & EpSO\_ESSO\_EPILONT & Dextroamphetamine &  &  & X &  &  &  &  &  &  \\ 
  10 & 10 & EpSO\_ESSO\_EPILONT & Phenobarbital &  & X &  &  &  &  &  & X & X \\ 
  11 & 11 & EpSO\_ESSO\_EPILONT & Melatonin &  & X &  &  &  &  &  &  &  \\ 
  12 & 12 & EpSO\_ESSO\_EPILONT & Carbamazepine & X &  &  &  &  &  &  & X &  \\ 
  13 & 13 & EpSO\_ESSO\_EPILONT & Phenytoin & X &  &  &  &  &  &  & X & X \\ 
  14 & 14 & EpSO\_ESSO\_EPILONT & Amphetamine &  &  & X &  &  &  &  &  &  \\ 
  15 & 15 & EpSO\_ESSO\_EPILONT & Propofol &  &  &  & X &  &  &  &  & X \\ 
  16 & 16 & EpSO\_ESSO\_EPILONT & Esketamine &  &  &  & X &  &  &  &  &  \\ 
  17 & 17 & EpSO\_ESSO\_EPILONT & Acetaminophen &  &  &  &  & X &  &  &  &  \\ 
  18 & 18 & EpSO\_ESSO\_EPILONT & Clozapine &  & X &  &  &  &  &  &  &  \\ 
  19 & 19 & EpSO\_ESSO\_EPILONT & Midazolam &  & X &  &  &  &  &  &  & X \\ 
  20 & 20 & EpSO\_ESSO\_EPILONT & Fentanyl &  &  &  &  & X &  &  &  &  \\ 
  21 & 21 & EpSO\_ESSO\_EPILONT & Pentobarbital &  & X &  &  &  &  &  &  & X \\ 
  22 & 22 & EpSO\_ESSO\_EPILONT & Fluoxetine &  &  & X &  &  &  &  &  &  \\ 
  23 & 23 & EpSO\_ESSO\_EPILONT & Methadone &  &  &  &  & X &  &  &  &  \\ 
  24 & 24 & EpSO\_ESSO\_EPILONT & Valproic Acid & X &  &  &  &  &  &  & X & X \\ 
  25 & 25 & EpSO\_ESSO\_EPILONT & Isoflurane &  &  &  & X &  &  &  &  & X \\ 
  26 & 27 & EpSO\_ESSO\_EPILONT & Methamphetamine &  &  & X &  &  &  &  &  &  \\ 
  27 & 28 & EpSO\_ESSO\_EPILONT & Levodopa &  &  &  &  &  & X &  &  &  \\ 
  28 & 29 & EpSO\_ESSO\_EPILONT & Olanzapine &  & X &  &  &  &  &  &  &  \\ 
  29 & 30 & EpSO\_ESSO\_EPILONT & Halothane &  &  &  & X &  &  &  &  &  \\ 
  30 & 26 & EpSO\_ESSO & Levobupivacaine &  &  &  & X &  &  &  &  &  \\ 
  31 & 31 & EpSO\_ESSO & Sevoflurane &  &  &  & X &  &  &  &  &  \\ 
  32 & 32 & EpSO\_ESSO & Escitalopram &  &  & X &  &  &  &  &  &  \\ 
  33 & 33 & EpSO\_ESSO & Apomorphine &  &  &  &  &  & X &  &  &  \\ 
  34 & 35 & EpSO\_ESSO & Thiopental &  & X &  &  &  &  &  &  & X \\ 
  35 & 37 & EpSO\_ESSO & Amitriptyline &  &  & X &  &  &  &  &  &  \\ 
  36 &  & EpSO\_ESSO & Imipramine &  &  & X &  &  &  &  &  &  \\ 
  37 &  & EpSO & Desipramine &  &  & X &  &  &  &  &  &  \\ 
  38 &  & EpSO & Paroxetine &  &  & X &  &  &  &  &  &  \\ 
  39 &  & EpSO\_EPILONT & Gabapentin & X &  &  &  &  &  &  & X &  \\ 
  40 & 36 & EPILONT & Lamotrigine & X &  &  &  &  &  &  & X &  \\ 
  41 & 38 & EPILONT & Levetiracetam & X &  &  &  &  &  &  & X & X \\ 
  42 &  & EPILONT & Clonazepam & X &  &  &  &  &  &  &  & X \\ 
  43 &  & EPILONT & Donepezil &  &  & X &  &  &  &  &  &  \\ 
  44 &  & EPILONT & Memantine &  &  & X &  &  &  &  &  &  \\ 
  45 &  & EPILONT & Topiramate & X &  &  &  &  &  &  & X &  \\ 
  46 &  & EPILONT & Vigabatrin & X &  &  &  &  &  &  &  &  \\ 
  47 & 34 & ESSO\_EPILONT & Pilocarpine &  &  &  &  &  &  & X &  &  \\ 
  48 & 39 & ESSO\_EPILONT & Dalfampridine &  &  &  &  &  &  & X &  &  \\ 
  49 &  & ESSO & Naltrexone &  &  &  &  &  &  & X &  &  \\ 
   \hline
\end{tabular}
\end{table}

