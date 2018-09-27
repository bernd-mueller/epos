#' Calculate cosine similarity coefficients for each of the frequency vector mesh, epso
#' esso, and epi against the drugbank vector
#'
#' @return dcframe dataframe with the columns for Elements, MeSH, EpSO, ESSO and EPILONT 
#'
#' @examples
#' dcframe <- calcCosineAgainstDrugBank ()
#' 
#' @export
calcCosineAgainstDrugBank <- function () {
  mesh <- loadDictionaryFrequencyMeSH()
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  drugbank <- loadDictionaryFrequencyDrugBank ()
  
  dcmesh <- calcCosine(mesh, drugbank)
  dcepso <- calcCosine(epso, drugbank)
  dcesso <- calcCosine(esso, drugbank)
  dcepi <- calcCosine(epi, drugbank)
  dcframe <- data.frame(
    Elements = c(1,length(drugbank)),
    MeSH = dcmesh,
    EpSO=dcepso,
    ESSO=dcesso,
    EPILONT=dcepi
  )
  return (dcframe)
}

#' Calculate dice similarity coefficients for each of the frequency vector mesh, epso
#' esso, and epi against the drugbank vector
#'
#' @return ddframe dataframe with the columns for Elements, MeSH, EpSO, ESSO and EPILONT
#'
#' @examples
#' ddframe <- calcDiceAgainstDrugBank ()
#' 
#' @export
calcDiceAgainstDrugBank <- function () {
  mesh <- loadDictionaryFrequencyMeSH()
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  drugbank <- loadDictionaryFrequencyDrugBank ()
  
  ddmesh <- calcDice(mesh, drugbank)
  ddepso <- calcDice(epso, drugbank)
  ddesso <- calcDice(esso, drugbank)
  ddepi <- calcDice(epi, drugbank)
  ddframe <- data.frame(
    Elements = c(1,length(drugbank)),
    MeSH = ddmesh,
    EpSO=ddepso,
    ESSO=ddesso,
    EPILONT=ddepi
  )
  return (ddframe)
}


#' Calculate jaccard similarity coefficients for each of the frequency vector mesh, epso
#' esso, and epi against the drugbank vector
#'
#' @return ddframe dataframe with the columns for Elements, MeSH, EpSO, ESSO and EPILONT
#'
#' @examples
#' djframe <- calcJaccardAgainstDrugBank ()
#' 
#' @export
calcJaccardAgainstDrugBank <- function () {
  mesh <- loadDictionaryFrequencyMeSH()
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  drugbank <- loadDictionaryFrequencyDrugBank ()
  
  djmesh <- calcJaccard(mesh, drugbank)
  djepso <- calcJaccard(epso, drugbank)
  djesso <- calcJaccard(esso, drugbank)
  djepi <- calcJaccard(epi, drugbank)
  djframe <- data.frame(
    Elements = c(1,length(drugbank)),
    MeSH = djmesh,
    EpSO=djepso,
    ESSO=djesso,
    EPILONT=djepi
  )
  return (djframe)
}