#' Creates data frame for plotting the dice coefficients against DrugBank
#'
#' @return ddicedrugbank the data frame for the dice coefficient against DrugBank that can be used by createDicePlotDrugBank
#'
#' @examples
#' \dontrun{
#' createDiceFrameDrugBank(mesh, drugbank, epso, esso, epi)
#' }
createDiceFrameDrugBank <- function (mesh, drugbank, epso, esso, epi) {
  dmeshdrugbank <- calcDice (mesh, drugbank)
  depsodrugbank <- calcDice (epso, drugbank)
  dessodrugbank <- calcDice (esso, drugbank)
  depidrugbank <- calcDice (epi, drugbank)
  
  ddicedrugbank <- data.frame (Elements = 1:length(dmeshdrugbank), MeSH = dmeshdrugbank, EpSO = depsodrugbank, ESSO = dessodrugbank, EPILONT = depidrugbank)
  return (ddicedrugbank)
}

#' Creates data frame for plotting the cosine coefficients against DrugBank
#'
#' @return dcosinedrugbank the data frame for the cosine coefficient against DrugBank that can be used by createCosinePlotDrugBank
#'
#' @examples
#' \dontrun{
#' createCosineFrameDrugBank(mesh, drugbank, epso, esso, epi)
#' }
createCosineFrameDrugBank <- function (mesh, drugbank, epso, esso, epi) {
  cmeshdrugbank <- calcCosine (mesh, drugbank)
  cepsodrugbank <- calcCosine (epso, drugbank)
  cessodrugbank <- calcCosine (esso, drugbank)
  cepidrugbank <- calcCosine (epi, drugbank)
  dcosinedrugbank <- data.frame (Elements = 1:length(cmeshdrugbank), MeSH = cmeshdrugbank, EpSO = cepsodrugbank, ESSO = cessodrugbank, EPILONT = cepidrugbank)
  return (dcosinedrugbank)
}

#' Creates data frame for plotting the jaccard coefficients against DrugBank
#'
#' @return djaccarddrugbank the data frame for the jaccard coefficient against DrugBank that can be used by createJaccardPlotDrugBank
#'
#' @examples
#' \dontrun{
#' createJaccardFrameDrugBank(mesh, drugbank, epso, esso, epi)
#' }
createJaccardFrameDrugBank <- function (mesh, drugbank, epso, esso, epi) {
  jmeshdrugbank <- calcJaccard (mesh, drugbank)
  jepsodrugbank <- calcJaccard (epso, drugbank)
  jessodrugbank <- calcJaccard (esso, drugbank)
  jepidrugbank <- calcJaccard (epi, drugbank)
  djaccarddrugbank <- data.frame (Elements = 1:length(jmeshdrugbank), MeSH = jmeshdrugbank, EpSO = jepsodrugbank, ESSO = jessodrugbank, EPILONT = jepidrugbank)
  return (djaccarddrugbank)
}