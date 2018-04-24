library(epilepsyontologysimilarities)
context("test_createCosineFrameMeSH")

mesh <- c(1,2,3)
epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)
drugbank <- c(1,2,5)

testframe <- data.frame(Elements = 1:length(mesh),
                        DrugBank = calcCosine(drugbank, mesh),
                        EpSO = calcCosine(epso, mesh),
                        ESSO = calcCosine(esso, mesh),
                        EPILONT = calcCosine(epi, mesh))
test_that("Test createCosineFrameMeSH creates the right data.frame", {
  expect_that( createCosineFrameMeSH (mesh, drugbank, epso, esso, epi), equals(testframe))
})