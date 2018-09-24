library(epos)
context("test_calcCosineAgainstDrugBank")

test_that("Test if the dataframe for cosine similarity coefficients is correctly created", {
  dcd <- calcCosineAgainstDrugBank()
  expect_that(length(dcd$Elements), equals(2604))
})