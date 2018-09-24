#' Load the RDS object inst/resources/MeSH.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary MeSH with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return mesh list with MesH terms sorted by their frequency
#'
#' @examples
#' mesh <- loadDictionaryFrequencyMeSH ()
loadDictionaryFrequencyMeSH <- function () {
  mesh <- readRDS("inst/resources/MeSH.rds")
  return (mesh)
}

#' Load the RDS object inst/resources/EpSO.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary EpSO with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return epso list with EpSO terms sorted by their frequency
#'
#' @examples
#' epso <- loadDictionaryFrequencyEpSO ()
loadDictionaryFrequencyEpSO <- function () {
  epso <- readRDS("inst/resources/EpSO.rds")
  return (epso)
}

#' Load the RDS object inst/resources/ESSO.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary ESSO with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return mesh list with ESSO terms sorted by their frequency
#'
#' @examples
#' esso <- loadDictionaryFrequencyESSO ()
loadDictionaryFrequencyESSO <- function () {
  esso <- readRDS("inst/resources/ESSO.rds")
  return (esso)
}

#' Load the RDS object inst/resources/EPILONT.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary EPILONT with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return epilont list with EPILONT terms sorted by their frequency
#'
#' @examples
#' epilont <- loadDictionaryFrequencyEPILONT ()
loadDictionaryFrequencyEPILONT <- function () {
  epilont <- readRDS("inst/resources/EPILONT.rds")
  return (epilont)
}

#' Load the RDS object inst/resources/DrugBank.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary DrugBank with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return drugbank list with MesH terms sorted by their frequency
#'
#' @examples
#' drugbank <- loadDictionaryFrequencyDrugBank ()
loadDictionaryFrequencyDrugBank <- function () {
  drugbank <- readRDS("inst/resources/DrugBank.rds")
  return (drugbank)
}