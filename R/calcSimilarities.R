#' Calculate the jaccard coefficient for two lists a and b
#'
#' @param a list with elements that should be of same type as in list b
#' @param b list with elements
#'
#' @return ja list with length of set b containing the jaccard similarity coefficient for each list element
#' @export
#'
#' @examples
#' \dontrun{
#' calcJaccard(c(1,2,3), c(2,3,4))
#' }
calcJaccard <- function (a, b) {
  la = length(a)
  lb = length(b)
  lo = lb
  
  for (i in 1:lo) {
    cinter <- length(intersect(a[1:i], b[1:i]))
    cunion <- length(union(x = a[1:i], y = b[1:i]))
    cjac <- jaccard (cinter, cunion)
    
    
    if (i == 1) {
      ja = cjac
    } else {
      ja <- c(ja, cjac)
    }
  }
  return (ja)
}

#' Calculate jaccard similarity coefficient or two sets a and b
#'
#' @param ainterb integer value with number of intersecting elements between set a and b
#' @param aunionb integer value with number of union elements between set a and b
#'
#' @return jac double value with the jaccard similarity coefficient 
#' @export
#'
#' @examples
#' \dontrun{
#' jaccard(1,3)
#' }
jaccard <- function (ainterb, aunionb) {
  jac <- (ainterb) / (aunionb-ainterb)
  return (jac)
}

#' Calculate the dice coefficient for two lists a and b
#'
#' @param a list with elements that should be of same type as in list b
#' @param b list with elements
#'
#' @return di list with length of set b containing the dice similarity coefficient at each list element
#' @export
#'
#' @examples
#' \dontrun{
#' calcDice(c(1,2,3), c(2,3,4))
#' }
calcDice <- function (a, b) {
  la = length(a)
  lb = length(b)
  lo = lb
  
  for (i in 1:lo) {
    cinter <- length(intersect(a[1:i], b[1:i]))
    curalength <- i
    if (la<i) {
      curalength <- la  
    } 
    curblength <- i
    if (lb < i) {
      curblength <- lb
    } 
    cdice <- dice (cinter, curalength, curblength)
    if (i == 1) {
      di = cdice
    } else {
      di <- c(di, cdice)
    }
  }
  di
}

#' Calculate dice similarity coefficient
#'
#' @param ainterb integer value with number of intersecting elements between set a and b
#' @param lengtha integer value with the number of items in set a
#' @param lengthb integer value with the number of items in set b
#'
#' @return dice double vlaue with the dice similarity coefficient 
#' @export
#'
#' @examples
#' \dontrun{
#' dice(1,3, 4)
#' }
dice <- function (ainterb, lengtha, lengthb) {
  dice <- 2*(ainterb/(lengtha+lengthb))
  dice
}

#' Calculate the cosine coefficient for two lists a and b
#'
#' @param a list with elements that should be of same type as in list b
#' @param b list with elements
#'
#' @return co list with length of set b containing the cosine similarity coefficient at each position
#' @export
#'
#' @examples
#' \dontrun{
#' calcCosine(c(1,2,3), c(2,3,4))
#' }
calcCosine <- function (a, b) {
  la = length(a)
  lb = length(b)
  lo = lb
  
  for (i in 1:lo) {
    cinter <- length(intersect(a[1:i], b[1:i]))
    curalength <- i
    if (la<i) {
      curalength <- la  
    } 
    curblength <- i
    if (lb < i) {
      curblength <- lb
    } 
    ccos <- cosine (cinter, curalength, curblength)
    if (i == 1) {
      co = ccos
    } else {
      co <- c(co, ccos)
    }
    #print(cjac)
  }
  return (co)
}

#' Calculate cosine similarity coefficient
#'
#' @param ainterb integer value with number of intersecting elements between set a and b
#' @param lengtha integer value with the number of items in set a
#' @param lengthb integer value with the number of items in set b
#'
#' @return cosine double vlaue with the cosine similarity coefficient 
#' @export
#'
#' @examples
#' \dontrun{
#' cosine(1,3)
#' }
cosine <- function (ainterb, lengtha, lengthb) {
  cosine <- (ainterb) / (lengtha^(1/2) * lengthb^(1/2))
  return (cosine)
}