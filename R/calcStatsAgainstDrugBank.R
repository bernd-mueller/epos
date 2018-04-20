#' Creates data frame for plotting the dice coefficients against DrugBank
#'
#' @return ddicedrugbank the data frame for the dice coefficient against DrugBank that can be used by createDicePlotDrugBank
#'
#' @examples
#' \dontrun{
#' createDiceFrameDrugBank()
#' }
createDiceFrameDrugBank <- function (dmeshdrugbank, depsodrugbank, dessodrugbank, depidrugbank) {
  ddicedrugbank <- data.frame (Elements = 1:length(dmeshdrugbank), DrugBank = dmeshdrugbank, EpSO = depsodrugbank, ESSO = dessodrugbank, EPILONT = depidrugbank)
  return (ddicedrugbank)
}

#' Creates data frame for plotting the cosine coefficients against DrugBank
#'
#' @return dcosinedrugbank the data frame for the cosine coefficient against DrugBank that can be used by createCosinePlotDrugBank
#'
#' @examples
#' \dontrun{
#' createCosineFrameDrugBank()
#' }
createCosineFrameDrugBank <- function (ddrugbankmesh, depsomesh, dessomesh, depimesh) {
  dcosinedrugbank <- data.frame (Elements = 1:length(cdrugbankmesh), DrugBank = cmeshdrugbank, EpSO = cepsodrugbank, ESSO = cessodrugbank, EPILONT = cepidrugbank)
  return (dcosinedrugbank)
}

#' Creates data frame for plotting the jaccard coefficients against DrugBank
#'
#' @return djaccarddrugbank the data frame for the jaccard coefficient against DrugBank that can be used by createJaccardPlotDrugBank
#'
#' @examples
#' \dontrun{
#' createJaccardFrameDrugBank()
#' }
createJaccardFrameDrugBank <- function (dmeshdrugbank, depsodrugbank, dessodrugbank, depidrugbank) {
  djaccarddrugbank <- data.frame (Elements = 1:length(jmeshdrugbank), MeSH = jmeshdrugbank, EpSO = jepsodrugbank, ESSO = jessodrugbank, EPILONT = jepidrugbank)
  return (djaccarddrugbank)
}

#' Calculate jaccard coefficient of mesh against drugbank
#'
#' @param mesh list with drugbank terms sorted by frequency
#' @param drugbank list with mesh terms sorted by frequency
#'
#' @return jmeshdrugbank list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardMeSHDrugBank(mesh, drugbank) 
#' }
calcJaccardMeSHDrugBank <- function (mesh, drugbank) {
  jmeshdrugbank <- calcJaccard(mesh, drugbank)
  return (jmeshdrugbank)
}

#' Calculate jaccard coefficient of epso against drugbank
#'
#' @param epso list with epso terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return jepsodrugbank list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardEpSODrugBank(epso, drugbank)
#' }
calcJaccardEpSODrugBank <- function (epso, drugbank) {
  jepsodrugbank <- calcJaccard(epso, drugbank)
  return (jepsodrugbank)
}

#' Calculate jaccard coefficient of esso against drugbank
#'
#' @param esso list with esso terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return jepsdrugbank list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardESSODrugBank(esso, drugbank)
#' }
calcJaccardESSODrugBank <- function (esso, drugbank) {
  jepsdrugbank <- calcJaccard(esso, drugbank)
  return (jepsdrugbank)
}

#' Calculate jaccard coefficient of epi against drugbank
#'
#' @param epi list with epi terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return jepidrugbank list with jaccard coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccardEPIDrugBank(epi, drugbank)
#' }
calcJaccardEPIDrugBank <- function (epi, drugbank) {
  jepidrugbank <- calcJaccard(epi, drugbank)
  return (jepidrugbank)
}

#' Calculate dice coefficient of mesh against drugbank
#'
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return dmeshdrugbank list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDicekMeSHDrugBank(mesh, drugbank)
#' }
calcDicekMeSHDrugBank <- function (mesh, drugbank) {
  dmeshdrugbank <- calcDice(mesh, drugbank)
  return (dmeshdrugbank)
}

#' Calculate dice coefficient of epso against drugbank
#'
#' @param epso list with epso terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return depsodrugbank list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDiceEpSODrugBank(epso, drugbank)
#' }
calcDiceEpSODrugBank <- function (epso, drugbank) {
  depsodrugbank <- calcDice(epso, drugbank)
  return (depsodrugbank)
}

#' Calculate dice coefficient of esso against drugbank
#'
#' @param esso list with esso terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return depsodrugbank list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDiceESSODrugBank(esso, drugbank)
#' }
calcDiceESSODrugBank <- function (esso, drugbank) {
  depsodrugbank <- calcDice(esso, drugbank)
  return (depsodrugbank)
}

#' Calculate dice coefficient of epi against drugbank
#'
#' @param epi list with epi terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return depidrugbank list with dice coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcDiceEPIDrugBank(epi, drugbank)
#' }
calcDiceEPIDrugBank <- function (epi, drugbank) {
  depidrugbank <- calcDice(epi, drugbank)
  return (depidrugbank)
}

#' Calculate cosine coefficient of mesh against drugbank
#'
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return cmeshdrugbank list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineMeSHDrugBank(mesh, drugbank)
#' }
calcCosineMeSHDrugBank <- function (mesh, drugbank) {
  cmeshdrugbank <- calcCosine(mesh, drugbank)
  return (cmeshdrugbank)
}

#' Calculate cosine coefficient of epso against drugbank
#'
#' @param epso list with epso terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return cepsodrugbank list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineEpSODrugBank(epso, drugbank)
#' }
calcCosineEpSODrugBank <- function (epso, drugbank) {
  cepsodrugbank <- calcCosine(epso, drugbank)
  return (cepsodrugbank)
}

#' Calculate cosine coefficient of esso against drugbank
#'
#' @param esso list with esso terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return cepsodrugbank list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineESSODrugBank(esso, drugbank)
#' }
calcCosineESSODrugBank <- function (esso, drugbank) {
  cepsodrugbank <- calcCosine(esso, drugbank)
  return (cepsodrugbank)
}

#' Calculate cosine coefficient of epi against drugbank
#'
#' @param epi list with epi terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency
#'
#' @return cepimesh list with cosine coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosineEPIMeSH(epi, drugbank)
#' }
calcCosineEPIMeSH <- function (epi, drugbank) {
  cepimesh <- calcCosine(epi, drugbank)
  return (cepimesh)
}