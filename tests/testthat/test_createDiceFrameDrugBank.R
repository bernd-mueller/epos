library(epilepsyontologysimilarities)
context("test_createDiceFrameDrugBank")

mesh <- c(1,2,3)
drugbank <- c(1,2,5)
epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)


testframe <- data.frame(Elements = 1:length(drugbank),
          MeSH = calcDice(mesh, drugbank),
          EpSO = calcDice(epso, drugbank),
          ESSO = calcDice(esso, drugbank),
          EPILONT = calcDice(epi, drugbank))
test_that("Test createDiceFrameDrugBank creates the right data.frame", {
  expect_that( createDiceFrameDrugBank(mesh, drugbank, epso, esso, epi), equals(testframe))
})