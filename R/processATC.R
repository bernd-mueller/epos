#' Processes the input file db-atc.map to form a HashMap containing the drug names with ATC codes
#'
#' @param filename character vector with the file name of the file db-atc.map
#' @param seperator character vector with the seperator used within the map-file
#'
#' @return atchashda hashmap with drug names as keys and atc codes as values 
#' @export
#'
#' @examples
#' \dontrun{
#' atchashda <- readAtcMapIntoHashMapDrugNamesAtcCodes(file = "inst/misc/db-atc.map", sep = "\t")
#' }
readAtcMapIntoHashMapDrugNamesAtcCodes <-  function (filename, seperator) {
  atcmap <- utils::read.csv(file = "inst/misc/db-atc.map", sep = "\t")
  drugnames <- atcmap[,4]
  atccodes <- atcmap[,2]
  catccodes <- as.character(atccodes)
  cdrugnames <- as.character(drugnames)
  atchashda <- hashmap::hashmap(cdrugnames, catccodes)
  return (atchashda)
}

#' Processes the input file db-atc.map to form a HashMap containing the drug names with ATC codes
#'
#' @param filename character vector with the file name of the file db-atc.map
#' @param seperator character vector with the seperator used within the map-file
#'
#' @return atchashaa hashmap with atc codes as keys and atc names as values 
#' @export
#'
#' @examples
#' \dontrun{
#' atchashaa <- readAtcMapIntoHashMapAtcCodesAtcNames(file = "inst/misc/db-atc.map", sep = "\t")
#' }
readAtcMapIntoHashMapAtcCodesAtcNames <-  function (filename, seperator) {
  atcmap <- utils::read.csv(file = "inst/misc/db-atc.map", sep = "\t")
  atcnames <- atcmap[,3]
  atccodes <- atcmap[,2]
  catccodes <- as.character(atccodes)
  catcnames <- as.character(atcnames)
  atchashaa <- hashmap::hashmap(catccodes, catcnames)
  return (atchashaa)
}

#' Filter a given list of drug names for having an ATC code, if not they are dropped
#'
#' @param druglist a list of drug names
#' @param atchashda a hashmap containing the drug names as keys
#'
#' @return approveddrugs a hashmap filtered for having an ATC code
#' @export
#'
#' @examples
#' \dontrun{
#' filtereddruglist <- filterApprovedDrugs(druglist, atchashda)
#' }
filterApprovedDrugs <- function (druglist, atchashda) {
  counter = 0
  for (drug in druglist) {
    if (drug %in% atchashda$keys()) {
      if (counter == 0) {
        approveddrugs <- drug
      } else {
        approveddrugs <- c(approveddrugs, drug)
      }
    }
    counter = counter + 1
  }
  
  return(approveddrugs)
}

#' Title
#'
#' @param druglist 
#' @param atchashda 
#' @param atchashaa 
#' @param atchashsec 
#'
#' @return atccounter
#' @export
#'
#' @examples
#' \dontrun{
#' atccounter <- countATC(druglist, atchashda, atchashaa, atchashsec)
#' }
countATC <- function (druglist, atchashda, atchashaa, atchashsec) {
  counter <- 0
  
  for (drug in druglist) {
    atccode <- atchashda$find(drug)
    atcname <- atchashaa$find(atccode)
    atccode <- substr(atccode, 1, 3)
    atcup   <- atchashsec$find(atccode)
    atckey <- paste(atcup, " ", atccode, sep = "")
    if (counter == 0) {
      atccounter <- hashmap (atckey, 1)
    } else {
      curcount <- 1
      if (atckey %in% atccounter$keys()) {
        curcount <- atccounter$find(atckey)
        curcount <- curcount + 1
      }
      atccounter$insert(atckey, curcount)
    }
    counter <- counter + 1
  }
  
  return (atccounter)
}

sortHashMapByValue <- function (ahashmap) {
  thekeys <- ahashmap$keys()
  thevalues <- as.integer(ahashmap$values())
  m <- matrix(thevalues, nrow=length(thekeys))
  rownames(m) <- thekeys
  res <- sort(rowSums(m), decreasing = TRUE)
  return (res)
}

readSecondLevelATC <- function (filename, seperator) {
  secondatc <- read.csv(file = "inst/misc/drugbankatc-secondlevel.map", sep = "\t")
  atcnames <- secondatc[,2]
  atccodes <- secondatc[,1]
  catccodes <- as.character(atccodes)
  catcnames <- as.character(atcnames)
  atchashsec <- hashmap::hashmap(catccodes, catcnames)
  return (atchashsec)
}