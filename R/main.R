#' Main function to call everything and produce the results
#'
#' @return result table
#'
#' @importFrom TopKLists calculate.maxK
#' @importFrom xtable xtable
#'
#' @export
#'
#' @examples
#' main()
main <- function () {
  atchashda <-
    readAtcMapIntoHashMapDrugNamesAtcCodes(filename = "inst/resources/db-atc.map", seperator = "\t")
  
  atchashaa <-
    readAtcMapIntoHashMapAtcCodesAtcNames(filename = "inst/resources/db-atc.map", seperator = "\t")
  
  atchashsec <-
    readSecondLevelATC("inst/resources/drugbankatc-secondlevel.map", "\t")
  

  tepso <- loadDictionaryFrequencyDrugBankCoOcEpSO ()
  tesso <- loadDictionaryFrequencyDrugBankCoOcESSO ()
  tepi <- loadDictionaryFrequencyDrugBankCoOcEPILONT ()
  
  lepso <- genDictListFromRawFreq(tepso)
  neuroepso <- filterNeuroDrugs(lepso, atchashda)
  
  lesso <- genDictListFromRawFreq(tesso)
  neuroesso <- filterNeuroDrugs(lesso, atchashda)
  
  lepi <- genDictListFromRawFreq(tepi)
  neuroepi <- filterNeuroDrugs(lepi, atchashda)
  
  dneuro <-
    data.frame(EpSO = neuroepso[1:210],
               ESSO = neuroesso[1:210],
               EPILONT = neuroepi[1:210])
  
  dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)
  
  neurospace <- as.character(dneuromaxk$topkspace)
  
  neurotable <-
    createBaseTable(neurospace, atchashda, atchashsec, dneuromaxk)
  
  print(
    xtable::xtable(
      neurotable,
      type = "latex",
      tabular.environment = "longtable"
    ),
    file = "inst/results/neurotable.tex"
  )
  
  return (neurotable)
}