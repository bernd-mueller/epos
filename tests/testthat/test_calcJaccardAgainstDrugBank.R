library(epos)
context("test_calcJaccardAgainstDrugBank")

test_that("Test if the dataframe for dice similarity coefficients against DrugBank is correctly created", {
  djd <- calcJaccardAgainstDrugBank()
  expect_that(length(djd$Elements), equals(2604))
})