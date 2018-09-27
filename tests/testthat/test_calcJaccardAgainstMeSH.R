library(epos)
context("test_calcJaccardAgainstMeSH")

test_that("Test if the dataframe for jaccard similarity coefficients against MeSH is correctly created", {
  mjd <- calcJaccardAgainstMeSH()
  expect_that(length(mjd$Elements), equals(28108))
})