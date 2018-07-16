

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
  mesh <- load(file = "inst/similaritydata/dictfreq/mesh.rda")
  amesh <- genDictListFromRawFreq(mesh)
  return (amesh)
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
  drugbank <- load("inst/similaritydata/dictfreq/DrugBank.rda")
  adrugbank <- genDictListFromRawFreq(drugbank)
  return (adrugbank)
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
  epso <- load("inst/similaritydata/dictfreq/EpSO.rda")
  aepso <- genDictListFromRawFreq(epso)
  return (aepso)
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
  esso <- load("inst/similaritydata/dictfreq/ESSO.rda")
  aesso <- genDictListFromRawFreq(esso)
  return (aesso)
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
  epi <- load("inst/similaritydata/dictfreq/EPILONT.rda")
  aepi <- genDictListFromRawFreq(epi)
  return (aepi)
}