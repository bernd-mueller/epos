library(epos)
context("test_drawVennPlot")

test_that("Test function drawVennPlot()", {
  utils::data(rawDrugBankCoOcEpSO, package="epos")
  utils::data(rawDrugBankCoOcESSO, package="epos")
  utils::data(rawDrugBankCoOcEPILONT, package="epos")
  
  atchashda <-
    readAtcMapIntoHashMapDrugNamesAtcCodes(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  tepso <- genDictListFromRawFreq(rawDrugBankCoOcEpSO)
  neuroepso <- filterNeuroDrugs(tepso, atchashda)
  
  tesso <- genDictListFromRawFreq(rawDrugBankCoOcESSO)
  neuroesso <- filterNeuroDrugs(tesso, atchashda)
  
  tepi <- genDictListFromRawFreq(rawDrugBankCoOcEPILONT)
  neuroepi <- filterNeuroDrugs(tepi, atchashda)
  dneuro <-
    data.frame(EpSO = neuroepso[1:210],
               ESSO = neuroesso[1:210],
               EPILONT = neuroepi[1:210])
  dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)
  v <- drawVennPlot(dneuromaxk)
  expect_that(length(v), equals(16))
})