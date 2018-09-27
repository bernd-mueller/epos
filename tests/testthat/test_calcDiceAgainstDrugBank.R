library(epos)
context("test_calcDiceAgainstDrugBank")

test_that("Test if the dataframe for dice similarity coefficients against Drugbank is correctly created", {
  ddd <- calcDiceAgainstDrugBank()
  expect_that(length(ddd$Elements), equals(2604))
})