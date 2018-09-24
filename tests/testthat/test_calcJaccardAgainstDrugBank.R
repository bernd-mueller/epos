library(epos)
context("test_calcJaccardAgainstDrugBank")

test_that("Test if the dataframe for dice similarity coefficients is correctly created", {
  djd <- calcJaccardAgainstDrugBank()
  expect_that(length(djd$Elements), equals(2604))
})