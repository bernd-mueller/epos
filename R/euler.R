drawEuler <- function () {
  set1 <- list(set1 = c('a', 'b', 'c'))
  set2 <- list(set2 = c('e', 'f', 'c'))
  set3 <- list(set3 = c('c', 'b', 'e'))
  myNV <- plotVenn(list(set1, set2, set3), sNames=c("One", "Two", "Three"),
                   labelRegions = c("aOne", "bTwo", "Tchree"))
  showSVG(myNV, opacity=0.2, outFile="a.svg")
}