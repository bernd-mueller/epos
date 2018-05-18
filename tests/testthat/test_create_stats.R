library(epilepsyontologysimilarities)
context("test_create_stats")

test_that("Test create_stats if it calculates the correct data.frame", {
  test_m <- data.frame (cosine = c(0.1,0.2), dice = c(0.2,0.3), jaccard = c(0.2,0.4), elements = c(1,2))
  
  test_s <- test_m %>% dplyr::rowwise() %>% dplyr::mutate(comean = mean (c(cosine,dice,jaccard)), comedian = median(c(cosine,dice,jaccard)),cosd = sd(c(cosine,dice,jaccard)))  
  
  expect_that(create_stats(test_m), equals(test_s))
  
})