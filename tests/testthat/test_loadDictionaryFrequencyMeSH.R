library(epos)
context("test_loadDictionaryFrequencyMeSH")

test_that("Test if the MeSH file can be loaded from inst/resources/MeSH.rds and if it the right one", {
  expect_that(length(loadDictionaryFrequencyMeSH()), equals(28108))
})