library(epos)
context("test_loadDictionaryFrequencyEpSO")

test_that("Test if the EpSO file can be loaded from inst/resources/EpSO.rds and if it the right one", {
  expect_that(length(loadDictionaryFrequencyEpSO()), equals(919))
})