library(epos)
context("test_createBaseTable")

test_that("Test function createBaseTable()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  utils::data(rawDrugNamesCoOcESSO, package="epos")
  utils::data(rawDrugNamesCoOcEPILONT, package="epos")
  utils::data(rawDrugNamesCoOcEPISEM, package="epos")
  utils::data(rawDrugNamesCoOcFENICS, package="epos")
  suppressWarnings({neurotable <- createBaseTable(
    coocepso = rawDrugNamesCoOcEpSO[1:250],
    coocesso=rawDrugNamesCoOcESSO[1:250],
    coocepi=rawDrugNamesCoOcEPILONT[1:250],
    coocepisem=rawDrugNamesCoOcEPISEM[1:250],
    coocfenics=rawDrugNamesCoOcFENICS[1:250])})
  expect_that(length(neurotable), equals(16))
})