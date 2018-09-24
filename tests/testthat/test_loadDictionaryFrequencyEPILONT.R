library(epos)
context("test_loadDictionaryFrequencyEPILONT")

test_that("Test if the EPILONT file can be loaded from inst/resources/EPILONT.rds and if it the right one", {
  expect_that(length(loadDictionaryFrequencyEPILONT()), equals(75))
})