library(epilepsyontologysimilarities)
context("test_createJaccardFrameDrugBank")

mesh <- c(1,2,3)
epso <-  c(2,3,4)
esso <- c(1,3,4)
epi <- c(1,2,4)
drugbank <- c(1,2,5)

testframe <- data.frame(Elements = 1:length(drugbank),
                        MeSH = calcJaccard(mesh, drugbank),
                        EpSO = calcJaccard(epso, drugbank),
                        ESSO = calcJaccard(esso, drugbank),
                        EPILONT = calcJaccard(epi, drugbank))
test_that("Test createJaccardFrameDrugBank creates the right data.frame", {
  expect_that( createJaccardFrameDrugBank(mesh, drugbank, epso, esso, epi), equals(testframe))
})