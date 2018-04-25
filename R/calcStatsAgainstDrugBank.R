#' Creates data frame for plotting the dice coefficients against DrugBank
#'
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency 
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return dicedrugbank the data frame for the dice coefficient against DrugBank that can be used by createDicePlotDrugBank
#'
#' @examples
#' \dontrun{
#' dicedrugbank <- createDiceFrameDrugBank(mesh, drugbank, epso, esso, epi)
#' }
createDiceFrameDrugBank <- function (mesh, drugbank, epso, esso, epi) {
  dmeshdrugbank <- calcDice (mesh, drugbank)
  depsodrugbank <- calcDice (epso, drugbank)
  dessodrugbank <- calcDice (esso, drugbank)
  depidrugbank <- calcDice (epi, drugbank)
  
  dicedrugbank <- data.frame (Elements = 1:length(dmeshdrugbank), MeSH = dmeshdrugbank, EpSO = depsodrugbank, ESSO = dessodrugbank, EPILONT = depidrugbank)
  return (dicedrugbank)
}

#' Creates data frame for plotting the cosine coefficients against DrugBank
#' 
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency 
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#' @return cosinedrugbank the data frame for the cosine coefficient against DrugBank that can be used by createCosinePlotDrugBank
#'
#' @examples
#' \dontrun{
#' cosinedrugbank <- createCosineFrameDrugBank(mesh, drugbank, epso, esso, epi)
#' }
createCosineFrameDrugBank <- function (mesh, drugbank, epso, esso, epi) {
  cmeshdrugbank <- calcCosine (mesh, drugbank)
  cepsodrugbank <- calcCosine (epso, drugbank)
  cessodrugbank <- calcCosine (esso, drugbank)
  cepidrugbank <- calcCosine (epi, drugbank)
  cosinedrugbank <- data.frame (Elements = 1:length(cmeshdrugbank), MeSH = cmeshdrugbank, EpSO = cepsodrugbank, ESSO = cessodrugbank, EPILONT = cepidrugbank)
  return (cosinedrugbank)
}

#' Creates data frame for plotting the jaccard coefficients against DrugBank
#' 
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency 
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#' 
#' @return jaccarddrugbank the data frame for the jaccard coefficient against DrugBank that can be used by createJaccardPlotDrugBank
#'
#' @examples
#' \dontrun{
#' jaccarddrugbank <- createJaccardFrameDrugBank(mesh, drugbank, epso, esso, epi)
#' }
createJaccardFrameDrugBank <- function (mesh, drugbank, epso, esso, epi) {
  jmeshdrugbank <- calcJaccard (mesh, drugbank)
  jepsodrugbank <- calcJaccard (epso, drugbank)
  jessodrugbank <- calcJaccard (esso, drugbank)
  jepidrugbank <- calcJaccard (epi, drugbank)
  jaccarddrugbank <- data.frame (Elements = 1:length(jmeshdrugbank), MeSH = jmeshdrugbank, EpSO = jepsodrugbank, ESSO = jessodrugbank, EPILONT = jepidrugbank)
  return (jaccarddrugbank)
}