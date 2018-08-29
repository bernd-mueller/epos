#' Processes the input file db-atc.map to form a HashMap containing the drug names with ATC codes
#'
#' @param filename character vector with the file name of the file db-atc.map
#' @param seperator character vector with the seperator used within the map-file
#'
#' @return atchashda hashmap with drug names as keys and atc codes as values 
#' 
#' @importFrom utils read.csv
#' @importFrom hashmap hashmap
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' atchashda <- readAtcMapIntoHashMapDrugNamesAtcCodes(filename = "db-atc.map", seperator = "\t")
#' }
readAtcMapIntoHashMapDrugNamesAtcCodes <-  function (filename, seperator) {
  atcmap <- utils::read.csv(file = filename, sep = seperator)
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
#' atchashaa <- readAtcMapIntoHashMapAtcCodesAtcNames(filename = "db-atc.map", seperator = "\t")
#' }
readAtcMapIntoHashMapAtcCodesAtcNames <-  function (filename, seperator) {
  atcmap <- utils::read.csv(file = filename, sep = seperator)
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

#' Filter a given list of drug names for having an ATC code starting with N indicating to be a 
#' drug for the Nervous System
#'
#' @param druglist a list of drug names
#' @param atchashda a hashmap containing the drug names as keys
#'
#' @return neurodrugs a hashmap filtered for having an ATC code starting with N
#' @export
#'
#' @examples
#' \dontrun{
#' neurodrugs <- filterNeuroDrugs(druglist, atchashda)
#' }
filterNeuroDrugs <- function (druglist, atchashda) {
  counter = 0
  for (drug in druglist) {
    if (drug %in% atchashda$keys()) {
      atccode <- atchashda$find(drug)
      if (startsWith(atccode, "N")) {
        if (counter == 0) {
          neurodrugs <- drug
        } else {
          neurodrugs <- c(neurodrugs, drug)
        }
        counter = counter + 1
      }
    }
  }
  return(neurodrugs)
}
#' Count the ATC second level classes for all drug names in the druglist
#'
#' @param druglist a character vector containing a list of drug names
#' @param atchashda a hashmap with drug names as keys and atc classes as values
#' @param atchashaa a hashmap with atc classes as keys and atc names as values
#' @param atchashlevel a hashmap containing atc classes from same level as keys and their names as values
#' @param length length of the ATC code
#'
#' @return atccounter
#' 
#' @importFrom hashmap hashmap
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' atccounter <- countATC(druglist, atchashda, atchashaa, atchashlevel, length)
#' }
countATC <- function (druglist, atchashda, atchashaa, atchashlevel, length) {
  counter <- 0
  
  for (drug in druglist) {
    atccode <- atchashda$find(drug)
    atcname <- atchashaa$find(atccode)
    atccode <- substr(atccode, 1, length)
    atcup   <- atchashlevel$find(atccode)
    atckey <- paste(atcup, " ", atccode, sep = "")
    if (counter == 0) {
      atccounter <- hashmap::hashmap (atckey, 1)
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

#' Sort a hashmap by its values
#'
#' @param ahashmap a hashmap that will be sorted by its values
#'
#' @return sortedhm the sorted hashmap
#' @export
#'
#' @examples
#' \dontrun{
#' shm <- sortHashMapByValue(ahm)
#' }
sortHashMapByValue <- function (ahashmap) {
  thekeys <- ahashmap$keys()
  thevalues <- as.integer(ahashmap$values())
  m <- matrix(thevalues, nrow=length(thekeys))
  rownames(m) <- thekeys
  sortedhm <- sort(rowSums(m), decreasing = TRUE)
  return (sortedhm)
}

#' Read the second level ATC classes from the file drugbankatc-secondlevel.map
#'
#' @param filename the file name that is supposed to be drugbankatc-secondlevel.map
#' @param seperator the csv file delimiter
#'
#' @return atchashsec a hashmap with second level ATC classes as keys and their names as values
#' 
#' @importFrom utils read.csv
#' @importFrom hashmap hashmap
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' atchashsec <- readSecondLevelATC("inst/resources/drugbankatc-secondlevel.map", "\t")
#' }
readSecondLevelATC <- function (filename, seperator) {
  secondatc <- utils::read.csv(file = filename, sep = seperator)
  atcnames <- secondatc[,2]
  atccodes <- secondatc[,1]
  catccodes <- as.character(atccodes)
  catcnames <- as.character(atcnames)
  atchashsec <- hashmap::hashmap(catccodes, catcnames)
  return (atchashsec)
}

#' Read the third level ATC classes from the file drugbankatc-thirdlevel.map
#'
#' @param filename the file name that is supposed to be drugbankatc-thirdlevel.map
#' @param seperator the csv file delimiter
#'
#' @return atchashthird a hashmap with third level ATC classes as keys and their names as values
#' 
#' @importFrom utils read.csv
#' @importFrom hashmap hashmap
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' atchashthird <- readThirdLevelATC("inst/resources/drugbankatc-thirdlevel.map", "\t")
#' }
readThirdLevelATC <- function (filename, seperator) {
  thirdatc <- utils::read.csv(file = filename, sep = seperator)
  atcnames <- thirdatc[,2]
  atccodes <- thirdatc[,1]
  catccodes <- as.character(atccodes)
  catcnames <- as.character(atcnames)
  atchashthird <- hashmap::hashmap(catccodes, catcnames)
  return (atchashthird)
}