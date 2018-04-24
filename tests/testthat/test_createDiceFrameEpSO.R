library(epilepsyontologysimilarities)
context("test_createDiceFrameEpSO")

epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)

testframe <- data.frame(Elements = 1:length(epso),
                        ESSO = calcDice(esso, epso),
                        EPILONT = calcDice(epi, epso))
test_that("Test createDiceFrameEpSO creates the right data.frame", {
  expect_that( createDiceFrameEpSO (epso, esso, epi), equals(testframe))
})