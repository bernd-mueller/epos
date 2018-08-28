#' Main function to call everything and produce the results
#'
#' @return result table
#' @export
#'
#' @examples
#' \dontrun{
#' main()
#' }
main <- function () {

atchashda <- readAtcMapIntoHashMapDrugNamesAtcCodes(filename = "inst/resources/db-atc.map", seperator = "\t")

atchashaa <- readAtcMapIntoHashMapAtcCodesAtcNames(filename = "inst/resources/db-atc.map", seperator = "\t")

atchashsec <- readSecondLevelATC("inst/resources/drugbankatc-secondlevel.map", "\t")

tepso <- getTermMatrix("DrugBank", "snoke260618aggEpilepsieEpSO")
lepso <- genDictListFromRawFreq(tepso)
neuroepso <- filterNeuroDrugs(lepso, atchashda)

tesso <- getTermMatrix("DrugBank", "snoke260618aggEpilepsieESSO")
lesso <- genDictListFromRawFreq(tesso)
neuroesso <- filterNeuroDrugs(lesso, atchashda)

tepi <- getTermMatrix("DrugBank", "snoke260618aggEpilepsieEPILONT")
lepi <- genDictListFromRawFreq(tepi)
neuroepi <- filterNeuroDrugs(lepi, atchashda)

dneuro <- data.frame(EpSO = neuroepso[1:210], ESSO=neuroesso[1:210], EPILONT=neuroepi[1:210])

dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)

neurospace <- as.character(dneuromaxk$topkspace)

return (createBaseTable(neurospace, atchashda, atchashsec, dneuromaxk))
}