#' Creates data frame for plotting the dice coefficients against MeSH
#' 
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency 
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#' 
#' @return dicemesh the data frame for the dice coefficient against MeSH that can be used by createDicePlotMeSH
#'
#' @examples
#' \dontrun{
#' dicemesh <- createDiceFrameMeSH(mesh, drugbank, epso, esso, epi)
#' }
createDiceFrameMeSH <- function (mesh, drugbank, epso, esso, epi) {
  ddrugbankmesh <- calcDice(drugbank, mesh)
  depsomesh <- calcDice (epso, mesh)
  dessomesh <- calcDice (esso, mesh)
  depimesh <- calcDice(epi,mesh)
  
  dicemesh <- data.frame (Elements = 1:length(ddrugbankmesh), DrugBank = ddrugbankmesh, EpSO = depsomesh, ESSO = dessomesh, EPILONT = depimesh)
  return (dicemesh)
}

#' Creates data frame for plotting the cosine coefficients against MeSH
#'
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency 
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return cosinemesh the data frame for the cosine coefficient against MeSH that can be used by createCosinePlotMeSH
#'
#' @examples
#' \dontrun{
#' cosinemesh <- createCosineFrameMeSH(mesh, drugbank, epso, esso, epi)
#' }
createCosineFrameMeSH <- function (mesh, drugbank, epso, esso, epi) {
  cdrugbankmesh <- calcCosine(drugbank, mesh)
  cepsomesh <- calcCosine (epso, mesh)
  cessomesh <- calcCosine (esso, mesh)
  cepimesh <- calcCosine(epi,mesh)
  
  cosinemesh <- data.frame (Elements = 1:length(cdrugbankmesh), DrugBank = cdrugbankmesh, EpSO = cepsomesh, ESSO = cessomesh, EPILONT = cepimesh)
  return (cosinemesh)
}

#' Creates data frame for plotting the jaccard coefficients against MeSH
#' 
#' @param mesh list with mesh terms sorted by frequency
#' @param drugbank list with drugbank terms sorted by frequency 
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#' 
#' @return jaccardmesh the data frame for the jaccard coefficient that can be used by createJaccardPlotMeSH
#'
#' @examples
#' \dontrun{
#' jaccardmesh <- createJaccardFrameMeSH(mesh, drugbank, epso, esso, epi)
#' }
createJaccardFrameMeSH <- function (mesh, drugbank, epso, esso, epi) {
  jdrugbankmesh <- calcJaccard(drugbank, mesh)
  jepsomesh <- calcJaccard (epso, mesh)
  jessomesh <- calcJaccard (esso, mesh)
  jepimesh <- calcJaccard(epi,mesh)
  jaccardmesh <- data.frame (Elements = 1:length(jdrugbankmesh), DrugBank = jdrugbankmesh, EpSO = jepsomesh, ESSO = jessomesh, EPILONT = jepimesh)
  return (jaccardmesh)
}