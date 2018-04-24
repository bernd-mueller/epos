library(epilepsyontologysimilarities)
context("test_createDiceFrameMeSH")

mesh <- c(1,2,3)
epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)
drugbank <- c(1,2,5)

testframe <- data.frame(Elements = 1:length(mesh),
                        DrugBank = calcDice(drugbank, mesh),
                        EpSO = calcDice(epso, mesh),
                        ESSO = calcDice(esso, mesh),
                        EPILONT = calcDice(epi, mesh))
test_that("Test createDiceFrameMeSH creates the right data.frame", {
  expect_that( createDiceFrameMeSH (mesh, drugbank, epso, esso, epi), equals(testframe))
})