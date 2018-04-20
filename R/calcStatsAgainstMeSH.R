#' Creates data frame for plotting the dice coefficients against MeSH
#'
#' @return ddicemesh the data frame for the dice coefficient against MeSH that can be used by createDicePlotMeSH
#'
#' @examples
#' \dontrun{
#' createDiceFrameMeSH()
#' }
createDiceFrameMeSH <- function (ddrugbankmesh, depsomesh, dessomesh, depimesh) {
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
createCosineFrameMeSH <- function (ddrugbankmesh, depsomesh, dessomesh, depimesh) {
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
createJaccardFrameMeSH <- function (ddrugbankmesh, depsomesh, dessomesh, depimesh) {
  djaccardmesh <- data.frame (Elements = 1:length(jdrugbankmesh), DrugBank = jdrugbankmesh, EpSO = jepsomesh, ESSO = jessomesh, EPILONT = jepimesh)
  return (djaccardmesh)
}



#' Calculate jaccard coefficient of drugbank against mesh
#'
#' @param drugbank list with drugbank terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return jdrugbankmesh list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardDrugBankMeSH(drugbank, mesh) 
#' }
calcJaccardDrugBankMeSH <- function (drugbank, mesh) {
  jdrugbankmesh <- calcJaccard(drugbank, mesh)
  return (jdrugbankmesh)
}

#' Calculate jaccard coefficient of epso against mesh
#'
#' @param epso list with epso terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return jepsomesh list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardEpSOMeSH(epso, mesh)
#' }
calcJaccardEpSOMeSH <- function (epso, mesh) {
  jepsomesh <- calcJaccard(epso, mesh)
  return (jepsomesh)
}

#' Calculate jaccard coefficient of esso against mesh
#'
#' @param esso list with esso terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return jepsomesh list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardESSOMeSH(esso, mesh)
#' }
calcJaccardESSOMeSH <- function (esso, mesh) {
  jessomesh <- calcJaccard(esso, mesh)
  return (jessomesh)
}

#' Calculate jaccard coefficient of epi against mesh
#'
#' @param epi list with epi terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return jepimesh list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardEPIMeSH(epi, mesh)
#' }
calcJaccardEPIMeSH <- function (epi, mesh) {
  jepimesh <- calcJaccard(epi, mesh)
  return (jepimesh)
}

#' Calculate dice coefficient of drugbank against mesh
#'
#' @param drugbank list with drugbank terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return ddrugbankmesh list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDiceDrugBankMeSH(drugbank, mesh)
#' }
calcDiceDrugBankMeSH <- function (drugbank, mesh) {
  ddrugbankmesh <- calcDice(drugbank, mesh)
  return (ddrugbankmesh)
}

#' Calculate dice coefficient of epso against mesh
#'
#' @param epso list with epso terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return depsomesh list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDiceEpSOMeSH(epso, mesh)
#' }
calcDiceEpSOMeSH <- function (epso, mesh) {
  depsomesh <- calcDice(epso, mesh)
  return (depsomesh)
}

#' Calculate dice coefficient of esso against mesh
#'
#' @param esso list with esso terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return depsomesh list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDiceESSOMeSH(esso, mesh)
#' }
calcDiceESSOMeSH <- function (esso, mesh) {
  dessomesh <- calcDice(esso, mesh)
  return (dessomesh)
}

#' Calculate dice coefficient of epi against mesh
#'
#' @param epi list with epi terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return depimesh list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDiceEPIMeSH(epi, mesh)
#' }
calcDiceEPIMeSH <- function (epi, mesh) {
  depimesh <- calcDice(epi, mesh)
  return (depimesh)
}

#' Calculate cosine coefficient of drugbank against mesh
#'
#' @param drugbank list with drugbank terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return cdrugbankmesh list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineDrugBankMeSH(drugbank, mesh)
#' }
calcCosineDrugBankMeSH <- function (drugbank, mesh) {
  cdrugbankmesh <- calcCosine(drugbank, mesh)
  return (cdrugbankmesh)
}

#' Calculate cosine coefficient of epso against mesh
#'
#' @param epso list with epso terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return cepsomesh list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineEpSOMeSH(epso, mesh)
#' }
calcCosineEpSOMeSH <- function (epso, mesh) {
  cepsomesh <- calcCosine(epso, mesh)
  return (cepsomesh)
}

#' Calculate cosine coefficient of esso against mesh
#'
#' @param esso list with esso terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return cepsomesh list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineESSOMeSH(esso, mesh)
#' }
calcCosineESSOMeSH <- function (esso, mesh) {
  cepsomesh <- calcCosine(esso, mesh)
  return (cepsomesh)
}

#' Calculate cosine coefficient of epi against mesh
#'
#' @param epi list with epi terms sorted by frequency
#' @param mesh list with mesh terms sorted by frequency
#'
#' @return cepimesh list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineEPIMeSH(epi, mesh)
#' }
calcCosineEPIMeSH <- function (epi, mesh) {
  cepimesh <- calcCosine(epi, mesh)
  return (cepimesh)
}