#' Load the RDS object inst/extdata/MeSH.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary MeSH with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return mesh list with MesH terms sorted by their frequency
#'
#' @examples
#' mesh <- loadDictionaryFrequencyMeSH ()
#' 
#' @export
loadDictionaryFrequencyMeSH <- function () {
  mesh <- readRDS(system.file("extdata", "MeSH.rds", package = "epos"))
  return (mesh)
}

#' Load the RDS object inst/extdata/EpSO.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary EpSO with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return epso list with EpSO terms sorted by their frequency
#'
#' @examples
#' epso <- loadDictionaryFrequencyEpSO ()
#' 
#' @export
loadDictionaryFrequencyEpSO <- function () {
  epso <- readRDS(system.file("extdata", "EpSO.rds", package = "epos"))
  return (epso)
}

#' Load the RDS object inst/extdata/ESSO.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary ESSO with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return mesh list with ESSO terms sorted by their frequency
#'
#' @examples
#' esso <- loadDictionaryFrequencyESSO ()
#' 
#' @export
loadDictionaryFrequencyESSO <- function () {
  esso <- readRDS(system.file("extdata", "ESSO.rds", package = "epos"))
  return (esso)
}

#' Load the RDS object inst/extdata/EPILONT.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary EPILONT with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return epilont list with EPILONT terms sorted by their frequency
#'
#' @examples
#' epilont <- loadDictionaryFrequencyEPILONT ()
#' 
#' @export
loadDictionaryFrequencyEPILONT <- function () {
  epilont <- readRDS(system.file("extdata", "EPILONT.rds", package = "epos"))
  return (epilont)
}

#' Load the RDS object inst/extdata/DrugBank.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary DrugBank with its frequency of
#' occurrences in the LIVIVO corpus. The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return drugbank list with MesH terms sorted by their frequency
#'
#' @examples
#' drugbank <- loadDictionaryFrequencyDrugBank ()
#' 
#' @export
loadDictionaryFrequencyDrugBank <- function () {
  drugbank <- readRDS(system.file("extdata", "DrugBank.rds", package = "epos"))
  return (drugbank)
}

#' Load the RDS object inst/extdata/DrugBankCoOcEpSO.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary DrugBank that co-occur in the same
#' documents with terms from the EpSO dictionary in the corpus of LIVIVO. 
#' The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return tepso list with drug terms sorted by their frequency
#'
#' @examples
#' tepso <- loadDictionaryFrequencyDrugBankCoOcEpSO ()
#' 
#' @export
loadDictionaryFrequencyDrugBankCoOcEpSO <- function () {
  tepso <- readRDS(system.file("extdata", "DrugBankCoOcEpSO.rds", package = "epos"))
  return (tepso)
}

#' Load the RDS object inst/extdata/DrugBankCoOcESSO.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary DrugBank that co-occur in the same
#' documents with terms from the ESSO dictionary in the corpus of LIVIVO. 
#' The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return tepso list with drug terms sorted by their frequency
#'
#' @examples
#' tesso <- loadDictionaryFrequencyDrugBankCoOcESSO ()
#' 
#' @export
loadDictionaryFrequencyDrugBankCoOcESSO <- function () {
  tesso <- readRDS(system.file("extdata", "DrugBankCoOcESSO.rds", package = "epos"))
  return (tesso)
}

#' Load the RDS object inst/extdata/DrugBankCoOcEPILONT.rds into a variable that is returned by this function. 
#' The returned list contains all terms from the dictionary DrugBank that co-occur in the same
#' documents with terms from the EPILONT dictionary in the corpus of LIVIVO. 
#' The moment of execution for generating the data 
#' stored in the RDS-file on the corpus of LIVIVO is June 26th 2018.
#'
#' @return tepi list with drug terms sorted by their frequency
#'
#' @examples
#' tepi <- loadDictionaryFrequencyDrugBankCoOcEPILONT ()
#' 
#' @export
loadDictionaryFrequencyDrugBankCoOcEPILONT <- function () {
  tepi <- readRDS(system.file("extdata", "DrugBankCoOcEPILONT.rds", package = "epos"))
  return (tepi)
}