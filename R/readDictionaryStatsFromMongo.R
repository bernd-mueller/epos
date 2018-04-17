#' Receives a sorted hashmap with found entities from a dictionary
#'
#' @param dictionary Character vector that is the name of a dicitonary having pre-calculated stats. This can be
#' MeSH, DrugBank, Agrovoc, EpSO, ESSO, or EPILONT
#' 
#'
#' @return a sorted hashmap containing all found entities from the respective dictionaries with frequencies
#' @export
#' @importFrom mongolite mongo
#' @importFrom hashmap hashmap
#' @examples
#' \dontrun{
#' mesh <- getTermMatrix("MeSH")
#' }
#'
getTermMatrix <- function(dictionary) {
  dictionaries <- list(
    "MeSH" = "MeSH", 
    "DrugBank" = "DrugBank", 
    "Agrovoc" = "Agrovoc", 
    "EpSO" = "EpSO",
    "ESSO" = "ESSO",
    "EPILONT" = "EPLIONT")
  
  ## TDM Server
  serverip <- "134.95.56.146"
  mydb <- "snoke"
  mycoll <- "totalcounts2018"
  
  mydbcoll <- sprintf('%s.%s', mydb, mycoll)
  #dictionary = "MeSH"
  con <- mongolite::mongo(collection = mycoll, db = mydb, url = paste('mongodb://', serverip, sep = ""), verbose = TRUE)
  
  
  
  #  s <- paste('{"CONCEPTCLASS" : "', , '"}', collapse='')
  s <- sprintf('{"dict" : "%s"}', dictionary)
  
  cat('\nSearching for entries from dictionary', dictionary, 'with', s)
  
  # iter <- con$iterate(query = s, fields = '{}', limit = 1)
  iter <- con$iterate(query = s, fields = '{"dict": 1, "concept":1, "count":1}')
  
  ## create the counter
  counter = 1
  hm <- hashmap("1", "1")
  
  while(length(doc <- iter$one())) {
    curdoc <- doc
    curdict <- curdoc$dict
    curconcept <- curdoc$concept
    curcount <- curdoc$count
    if (curdict==dictionary) {
      hm$insert(curconcept, curcount)     
    }
    counter = counter + 1
    if (counter %% 100 == 0) {
      cat("\nProcessing: ", counter)
    }
  }
  cat("\nProcessed ", counter , "concepts")
  
  concepts <- hm$keys()
  frequencies <- as.integer(hm$values())
  m <- matrix(frequencies, nrow=length(concepts))
  rownames(m) <- concepts
  sort(rowSums(m), decreasing = TRUE)
}