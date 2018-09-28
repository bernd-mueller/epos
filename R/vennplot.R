#' Draw venn diagram with example values
#'
#' @param dneuromaxk object created with TopKLists::calculate.maxK()
#'
#' @return venn diagram
#' 
#' @importFrom VennDiagram draw.triple.venn
#' @importFrom stringr str_count
#' 
#' @export
#'
#' @examples
#' utils::data(rawDrugBankCoOcEpSO, package="epos")
#' atchashda <-
#'   readAtcMapIntoHashMapDrugNamesAtcCodes(
#'     system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' tepso <- genDictListFromRawFreq(rawDrugBankCoOcEpSO)
#' neuroepso <- filterNeuroDrugs(tepso, atchashda)
#' utils::data(rawDrugBankCoOcESSO, package="epos")
#' tesso <- genDictListFromRawFreq(rawDrugBankCoOcESSO)
#' neuroesso <- filterNeuroDrugs(tesso, atchashda)
#' utils::data(rawDrugBankCoOcEPILONT, package="epos")
#' tepi <- genDictListFromRawFreq(rawDrugBankCoOcEPILONT)
#' neuroepi <- filterNeuroDrugs(tepi, atchashda)
#' dneuro <-
#'   data.frame(EpSO = neuroepso[1:210],
#'              ESSO = neuroesso[1:210],
#'              EPILONT = neuroepi[1:210])
#' dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)
#' v <- drawVennPlot(dneuromaxk)
drawVennPlot <- function(dneuromaxk)  {
  EPILONT_EpSO_ESSO = stringr::str_count(dneuromaxk$venntable$objects$EPILONT_EpSO_ESSO, "\\*")
  EPILONT_EpSO = stringr::str_count(dneuromaxk$venntable$objects$EPILONT_EpSO, "\\*")
  EPILONT_ESSO = stringr::str_count(dneuromaxk$venntable$objects$EPILONT_ESSO, "\\*")
  EpSO_ESSO = stringr::str_count(dneuromaxk$venntable$objects$EpSO_ESSO, "\\*")
  EpSO = stringr::str_count(dneuromaxk$venntable$objects$EpSO, "\\*")
  ESSO = stringr::str_count(dneuromaxk$venntable$objects$ESSO, "\\*")
  EPILONT = stringr::str_count(dneuromaxk$venntable$objects$EPILONT, "\\*")
  venn <- VennDiagram::draw.triple.venn(
    area1 = length(dneuromaxk$vennlists$EpSO),
    area2 = length(dneuromaxk$vennlists$ESSO),
    area3 = length(dneuromaxk$vennlists$EPILONT),
    n12 = (EPILONT_EpSO_ESSO + EpSO_ESSO + EPILONT_ESSO),
    n23 = (EPILONT_EpSO_ESSO + EPILONT_ESSO),
    n13 = (EPILONT_EpSO_ESSO + EPILONT_EpSO),
    n123 = (EPILONT_EpSO_ESSO),
    category = c("EpSO", "ESSO", "EPILONT"),
    fill = c("blue", "orange", "green"),
    lty = "blank",
    cex = 2,
    cat.cex = 2,
    cat.col = c("blue", "orange", "green")
  )
  return (venn)
}