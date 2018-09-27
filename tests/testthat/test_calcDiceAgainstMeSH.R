library(epos)
context("test_calcDiceAgainstMeSH")

test_that("Test if the dataframe for cosine similarity coefficients against MeSH is correctly created", {
  mdd <- calcDiceAgainstMeSH()
  expect_that(length(mdd$Elements), equals(28108))
})