library(epilepsyontologysimilarities)
context("test_createJaccardFrameESSO")

epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)

testframe <- data.frame(Elements = 1:length(esso),
                        EpSO = calcJaccard(epso, esso),
                        EPILONT = calcJaccard(epi, esso))
test_that("Test createJaccardFrameESSO creates the right data.frame", {
  expect_that( createJaccardFrameESSO (epso, esso, epi), equals(testframe))
})