library(epilepsyontologysimilarities)
context("test_createCosineFrameEpSO")

epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)

testframe <- data.frame(Elements = 1:length(epso),
                        ESSO = calcCosine(esso, epso),
                        EPILONT = calcCosine(epi, epso))
test_that("Test createCosineFrameEpSO creates the right data.frame", {
  expect_that( createCosineFrameEpSO (epso, esso, epi), equals(testframe))
})