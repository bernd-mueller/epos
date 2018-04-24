library(epilepsyontologysimilarities)
context("test_jaccard")

test_that("Test jaccard if it calculates the correct jaccard coefficient", {
  expect_that(jaccard(1,3), equals(0.5))
})