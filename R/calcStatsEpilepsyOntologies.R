#' Creates data frame for plotting the dice coefficients against EpSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return ddiceepso the data frame for the dice coefficient against EpSO
#'
#' @examples
#' \dontrun{
#' ddiceepso <- createDiceFrameEpSO(epso, esso, epi)
#' }
createDiceFrameEpSO <- function (epso, esso, epi) {
  dessoepso <- calcDice (esso, epso)
  depiepso <- calcDice (epi, epso)
  ddiceepso <- data.frame (Elements = 1:length(dessoepso), 
                           ESSO = dessoepso, 
                           EPILONT = depiepso)
  return (ddiceepso)
}

#' Creates data frame for plotting the dice coefficients against ESSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return ddiceesso the data frame for the dice coefficient against ESSO
#'
#' @examples
#' \dontrun{
#' ddiceesso <- createDiceFrameESSO(epso, esso, epi)
#' }
createDiceFrameESSO <- function (epso, esso, epi) {
  depsoesso <- calcDice (epso, esso)
  depiesso <- calcDice (epi, esso)
  
  ddiceesso <- data.frame (Elements = 1:length(depsoesso), 
                          EpSO = depsoesso, 
                          EPILONT = depiesso)
  return (ddiceesso)
}

#' Creates data frame for plotting the jaccard coefficients against EpSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return djaccardepso the data frame for the jaccard coefficient against EpSO
#'
#' @examples
#' \dontrun{
#' djaccardepso <- createJaccardFrameEpSO(epso, esso, epi)
#' }
createJaccardFrameEpSO <- function (epso, esso, epi) {
  dessoepso <- calcJaccard (esso, epso)
  depiepso <- calcJaccard (epi, epso)
  djaccardepso <- data.frame (Elements = 1:length(dessoepso), 
                           ESSO = dessoepso, 
                           EPILONT = depiepso)
  return (djaccardepso)
}

#' Creates data frame for plotting the jaccard coefficients against ESSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return djaccardesso the data frame for the jaccard coefficient against ESSO
#'
#' @examples
#' \dontrun{
#' djaccardesso <- createJaccardFrameESSO(epso, esso, epi)
#' }
createJaccardFrameESSO <- function (epso, esso, epi) {
  depsoesso <- calcJaccard (epso, esso)
  depiesso <- calcJaccard (epi, esso)
  
  djaccardesso <- data.frame (Elements = 1:length(depsoesso), 
                           EpSO = depsoesso, 
                           EPILONT = depiesso)
  return (djaccardesso)
}

#' Creates data frame for plotting the cosine coefficients against EpSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return dcosineepso the data frame for the cosine coefficient against EpSO
#'
#' @examples
#' \dontrun{
#' dcosineepso <- createCosineFrameEpSO(epso, esso, epi)
#' }
createCosineFrameEpSO <- function (epso, esso, epi) {
  dessoepso <- calcCosine (esso, epso)
  depiepso <- calcCosine (epi, epso)
  dcosineepso <- data.frame (Elements = 1:length(dessoepso), 
                               ESSO = dessoepso, 
                               EPILONT = depiepso)
  return (dcosineepso)
}

#' Creates data frame for plotting the cosine coefficients against ESSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return dcosineesso the data frame for the cosine coefficient against ESSO
#'
#' @examples
#' \dontrun{
#' dcosineesso <- createCosineFrameESSO(epso, esso, epi)
#' }
createCosineFrameESSO <- function (epso, esso, epi) {
  depsoesso <- calcCosine (epso, esso)
  depiesso <- calcCosine (epi, esso)
  
  dcosineesso <- data.frame (Elements = 1:length(depsoesso), 
                               EpSO = depsoesso, 
                               EPILONT = depiesso)
  return (dcosineesso)
}