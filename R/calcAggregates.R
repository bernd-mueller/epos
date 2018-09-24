#' Calculate cosine similarity coefficients for each of the frequency vector mesh, epso
#' esso, and epi against the drugbank vector
#'
#' @return dcframe dataframe with the columns for Elements, MeSH, EpSO, ESSO and EPILONT 
#'
#' @examples
#' dcframe <- calcCosineAgainstDrugBank ()
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


#' Calculate required metrics for the aggregated plots against MeSH, DrugBank, and all Epilepsy Ontologies
#'
#' @param cosine data.frame containing the cosine vectors for MeSH, DrugBank, EpSO, ESSO, and EPILONT
#' @param dice data.frame containing the dice vectors for MeSH, DrugBank, EpSO, ESSO, and EPILONT
#' @param jaccard data.frame containing the jaccard vectors for MeSH, DrugBank, EpSO, ESSO, and EPILONT
#' @param elements vector of maximum length for plotting the x axis
#'
#' @return metrics data.frame containing cosine, dice, jaccard, elements, and mean and median with standard deviation
#'
#' @examples
#' dcframe <- calcCosineAgainstDrugBank ()
#' ddframe <- calcDiceAgainstDrugBank ()
#' djframe <- calcJaccardAgainstDrugBank()
#' meshmetrics <- create_metrics (dcframe$MeSH, ddframe$MeSH, djframe$MeSH, djframe$Elements)
create_metrics <- function (cosine, dice, jaccard, elements) {
  metrics <- data.frame(cosine = cosine, dice = dice, jaccard = jaccard, elements = elements)  
  return(metrics)
}

#' Calculate data.frame containing stats for mean, median, and standard deviation
#'
#' @param metrics the data.frame return from create_metrics
#'
#' @return stats the data.frame metrics with added columns for mean, media, and standard deviation
#' 
#' @importFrom dplyr rowwise
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @examples
#' dcframe <- calcCosineAgainstDrugBank ()
#' ddframe <- calcDiceAgainstDrugBank ()
#' djframe <- calcJaccardAgainstDrugBank()
#' meshmetrics <- create_metrics (dcframe$MeSH, ddframe$MeSH, djframe$MeSH, djframe$Elements)
#' meshstats <- create_stats(meshmetrics)
create_stats <- function (metrics) {
  stats <- metrics %>% dplyr::rowwise() %>% dplyr::mutate(comean = mean (c(cosine,dice,jaccard)), comedian = stats::median(c(cosine,dice,jaccard)),cosd = stats::sd(c(cosine,dice,jaccard)))  
  return (stats)
}