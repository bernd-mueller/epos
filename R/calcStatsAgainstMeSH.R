#' Calculate cosine similarity coefficients for each of the frequency vector drugbank, epso
#' esso, and epi against the mesh vector
#'
#' @return mcframe dataframe with the columns for Elements, DrugBank, EpSO, ESSO and EPILONT 
#'
#' @examples
#' mcframe <- calcCosineAgainstMeSH ()
#' 
#' @export
calcCosineAgainstMeSH <- function () {
  mesh <- loadDictionaryFrequencyMeSH()
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  drugbank <- loadDictionaryFrequencyDrugBank ()
  
  mcdrugbank <- calcCosine(drugbank, mesh)
  mcepso <- calcCosine(epso, mesh)
  mcesso <- calcCosine(esso, mesh)
  mcepi <- calcCosine(epi, mesh)
  mcframe <- data.frame(
    Elements = c(1,length(mesh)),
    DrugBank = mcdrugbank,
    EpSO=mcepso,
    ESSO=mcesso,
    EPILONT=mcepi
  )
  return (mcframe)
}

#' Calculate dice similarity coefficients for each of the frequency vector drugbank, epso
#' esso, and epi against the mesh vector
#'
#' @return mdframe dataframe with the columns for Elements, DrugBank, EpSO, ESSO and EPILONT 
#'
#' @examples
#' mdframe <- calcDiceAgainstMeSH ()
#' 
#' @export
calcDiceAgainstMeSH <- function () {
  mesh <- loadDictionaryFrequencyMeSH()
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  drugbank <- loadDictionaryFrequencyDrugBank ()
  
  mddrugbank <- calcDice(drugbank, mesh)
  mdepso <- calcDice(epso, mesh)
  mdesso <- calcDice(esso, mesh)
  mdepi <- calcDice(epi, mesh)
  mdframe <- data.frame(
    Elements = c(1,length(mesh)),
    DrugBank = mddrugbank,
    EpSO=mdepso,
    ESSO=mdesso,
    EPILONT=mdepi
  )
  return (mdframe)
}

#' Calculate jaccard similarity coefficients for each of the frequency vector drugbank, epso
#' esso, and epi against the mesh vector
#'
#' @return mjframe dataframe with the columns for Elements, DrugBank, EpSO, ESSO and EPILONT 
#'
#' @examples
#' mjframe <- calcJaccardAgainstMeSH ()
#' 
#' @export
calcJaccardAgainstMeSH <- function () {
  mesh <- loadDictionaryFrequencyMeSH()
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  drugbank <- loadDictionaryFrequencyDrugBank ()
  
  mjdrugbank <- calcJaccard(drugbank, mesh)
  mjepso <- calcJaccard(epso, mesh)
  mjesso <- calcJaccard(esso, mesh)
  mjepi <- calcJaccard(epi, mesh)
  mjframe <- data.frame(
    Elements = c(1,length(mesh)),
    DrugBank = mjdrugbank,
    EpSO=mjepso,
    ESSO=mjesso,
    EPILONT=mjepi
  )
  return (mjframe)
}