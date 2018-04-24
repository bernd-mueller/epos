library(epilepsyontologysimilarities)
context("test_createJaccardFrameMeSH")

mesh <- c(1,2,3)
epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)
drugbank <- c(1,2,5)

testframe <- data.frame(Elements = 1:length(mesh),
                        DrugBank = calcJaccard(drugbank, mesh),
                        EpSO = calcJaccard(epso, mesh),
                        ESSO = calcJaccard(esso, mesh),
                        EPILONT = calcJaccard(epi, mesh))
test_that("Test createJaccardFrameMeSH creates the right data.frame", {
  expect_that( createJaccardFrameMeSH (mesh, drugbank, epso, esso, epi), equals(testframe))
})