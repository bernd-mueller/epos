library(epilepsyontologysimilarities)
context("test_createCosineFrameESSO")

epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)

testframe <- data.frame(Elements = 1:length(esso),
                        EpSO = calcCosine(epso, esso),
                        EPILONT = calcCosine(epi, esso))
test_that("Test createCosineFrameESSO creates the right data.frame", {
  expect_that( createCosineFrameESSO (epso, esso, epi), equals(testframe))
})