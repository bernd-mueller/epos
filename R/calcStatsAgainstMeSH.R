#' Creates data frame for plotting the dice coefficients against MeSH
#'
#' @return ddicemesh the data frame for the dice coefficient against MeSH that can be used by createDicePlotMeSH
#'
#' @examples
#' \dontrun{
#' createDiceFrameMeSH()
#' }
createDiceFrameMeSH <- function (mesh, drugbank, epso, esso, epi) {
  ddrugbankmesh <- calcDice(drugbank, mesh)
  depsomesh <- calcDice (epso, mesh)
  dessomesh <- calcDice (esso, mesh)
  depimesh <- calcDice(epi,mesh)
  
  ddicemesh <- data.frame (Elements = 1:length(ddrugbankmesh), DrugBank = ddrugbankmesh, EpSO = depsomesh, ESSO = dessomesh, EPILONT = depimesh)
  return (ddicemesh)
}

#' Creates data frame for plotting the cosine coefficients against MeSH
#'
#' @return dcosinemesh the data frame for the cosine coefficient against MeSH that can be used by createCosinePlotMeSH
#'
#' @examples
#' \dontrun{
#' createCosineFrameMeSH()
#' }
createCosineFrameMeSH <- function (mesh, drugbank, epso, esso, epi) {
  cdrugbankmesh <- calcCosine(drugbank, mesh)
  cepsomesh <- calcCosine (epso, mesh)
  cessomesh <- calcCosine (esso, mesh)
  cepimesh <- calcCosine(epi,mesh)
  
  dcosinemesh <- data.frame (Elements = 1:length(cdrugbankmesh), DrugBank = cdrugbankmesh, EpSO = cepsomesh, ESSO = cessomesh, EPILONT = cepimesh)
  return (dcosinemesh)
}

#' Creates data frame for plotting the jaccard coefficients against MeSH
#'
#' @return djaccardmesh the data frame for the jaccard coefficient that can be used by createJaccardPlotMeSH
#'
#' @examples
#' \dontrun{
#' createJaccardFrameMeSH()
#' }
createJaccardFrameMeSH <- function (mesh, drugbank, epso, esso, epi) {
  jdrugbankmesh <- calcCosine(drugbank, mesh)
  jepsomesh <- calcCosine (epso, mesh)
  jessomesh <- calcCosine (esso, mesh)
  jepimesh <- calcCosine(epi,mesh)
  djaccardmesh <- data.frame (Elements = 1:length(jdrugbankmesh), DrugBank = jdrugbankmesh, EpSO = jepsomesh, ESSO = jessomesh, EPILONT = jepimesh)
  return (djaccardmesh)
}