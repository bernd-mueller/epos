library(epos)
context("test_loadDictionaryFrequencyDrugBank")

test_that("Test if the DrugBank file can be loaded from inst/resources/DrugBank.rds and if it the right one", {
  expect_that(length(loadDictionaryFrequencyDrugBank()), equals(2604))
})