

#' Loads mesh object from harddrive into the R environment
#'
#' @return amesh list of mesh terms sorted by frequency
#' @export
#'
#' @examples
#' \dontrun{
#' loadMeSH()
#' }
loadMeSH <- function () {
  load(file = "inst/mesh.rda")
  amesh <- genDictListFromRawFreq(mesh)
  amesh
}
#' Loads drugbank object from harddrive into the R environment
#'
#' @return adrugbank list of drugbank terms sorted by frequency
#' @export
#'
#' @examples
#' \dontrun{
#' loadDrugBank()
#' }
loadDrugBank <- function () {
  load("inst/DrugBank.rda")
  adrugbank <- genDictListFromRawFreq(drugbank)
}

#' Loads epso object from harddrive into the R environment
#'
#' @return aepso list of mesh terms sorted by frequency
#' @export
#'
#' @examples
#' \dontrun{
#' loadEpSO()
#' }
loadEpSO <- function () {
  load("inst/EpSO.rda")
  aepso <- genDictListFromRawFreq(epso)
}

#' Loads esso object from harddrive into the R environment
#'
#' @return aesso list of mesh terms sorted by frequency
#' @export
#'
#' @examples
#' \dontrun{
#' loadESSO()
#' }
loadESSO <- function () {
  load("inst/ESSO.rda")
  aesso <- genDictListFromRawFreq(esso)
}

#' Loads epilont object from harddrive into the R environment
#'
#' @return aepi list of mesh terms sorted by frequency
#' @export
#'
#' @examples
#' \dontrun{
#' loadEPILONT()
#' }
loadEPILONT <- function () {
  load("inst/EPILONT.rda")
  aepi <- genDictListFromRawFreq(epi)
}

#' Clears object that was loaded from harddrive into a list of terms sorted by frequency
#'
#' @param topfreqdictraw 
#'
#' @return a sorted list of terms
#' @export
#'
#' @examples
#' \dontrun{
#' genDictListFromRawFreq(epi)
#' }
genDictListFromRawFreq <- function (topfreqdictraw) {
  # remove last element from all lists because it is falsely a 1 from previous processing
  la = length(topfreqdictraw)
  topfreqdictraw <- topfreqdictraw[-la]
  a <- attributes(topfreqdictraw)$names
  a
}