

#' Loads objects from harddrive into the R environment
#'
#' @return no direct return but loading into environment
#' @export
#'
#' @examples
#' \dontrun{
#' loadSavedSortedMapsIntoEnv()
#' }
loadSavedSortedMapsIntoEnv <- function () {
    mesh     <<- load(file = "inst/MeSH.rda")
    drugbank <<- load(file = "inst/DrugBank.rda")
    epso     <<- load(file = "inst/EpSO.rda")
    esso     <<- load(file = "inst/ESSO.rda")
    epilont  <<- load(file = "inst/EPILONT.rda")
}