#' Aggregat a list to matrix
#'
#' @param x this is a list
#'
#' @return res this is a matrix
#' @export
#'
#' @examples
#' \dontrun{
#' getPerms(c("a",b"))
#' }
getPerms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}
