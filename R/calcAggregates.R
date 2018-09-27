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
#' 
#' @export
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
#' 
#' @export
create_stats <- function (metrics) {
  stats <- metrics %>% dplyr::rowwise() %>% dplyr::mutate(comean = mean (c(cosine,dice,jaccard)), comedian = stats::median(c(cosine,dice,jaccard)),cosd = stats::sd(c(cosine,dice,jaccard)))  
  return (stats)
}