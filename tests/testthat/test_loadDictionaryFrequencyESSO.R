library(epos)
context("test_loadDictionaryFrequencyESSO")

test_that("Test if the ESSO file can be loaded from inst/resources/ESSO.rds and if it the right one", {
  expect_that(length(loadDictionaryFrequencyESSO()), equals(919))
})