library(epilepsyontologysimilarities)
context("test_createCosineFrameDrugBank")

mesh <- c(1,2,3)
epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)
drugbank <- c(1,2,5)

testframe <- data.frame(Elements = 1:length(drugbank),
                        MeSH = calcCosine(mesh, drugbank),
                        EpSO = calcCosine(epso, drugbank),
                        ESSO = calcCosine(esso, drugbank),
                        EPILONT = calcCosine(epi, drugbank))
test_that("Test createCosineFrameDrugBank creates the right data.frame", {
  expect_that( createCosineFrameDrugBank(mesh, drugbank, epso, esso, epi), equals(testframe))
})