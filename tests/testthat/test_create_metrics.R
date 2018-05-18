library(epilepsyontologysimilarities)
context("test_create_metrics")

test_that("Test create_metrics if it calculates the correct data.frame", {
  metrics <- create_metrics(c(0.1,0.2), c(0.2,0.3), c(0.2,0.4), c(1,2))
  test_n <- data.frame (cosine = c(0.1,0.2), dice = c(0.2,0.3), jaccard = c(0.2,0.4), elements = c(1,2))
  
  expect_that(metrics, equals(test_n))
  
})