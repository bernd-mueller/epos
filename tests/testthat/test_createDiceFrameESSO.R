library(epilepsyontologysimilarities)
context("test_createDiceFrameESSO")

epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)

testframe <- data.frame(Elements = 1:length(esso),
                        EpSO = calcDice(epso, esso),
                        EPILONT = calcDice(epi, esso))
test_that("Test createDiceFrameESSO creates the right data.frame", {
  expect_that( createDiceFrameESSO (epso, esso, epi), equals(testframe))
})