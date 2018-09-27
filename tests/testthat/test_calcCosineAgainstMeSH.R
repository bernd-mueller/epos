library(epos)
context("test_calcCosineAgainstMeSH")

test_that("Test if the dataframe for cosine similarity coefficients against MeSH is correctly created", {
  mcd <- calcCosineAgainstMeSH()
  expect_that(length(mcd$Elements), equals(28108))
})