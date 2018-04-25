#' Creates data frame for plotting the dice coefficients against EpSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return diceepso the data frame for the dice coefficient against EpSO
#'
#' @examples
#' \dontrun{
#' diceepso <- createDiceFrameEpSO(epso, esso, epi)
#' }
createDiceFrameEpSO <- function (epso, esso, epi) {
  dessoepso <- calcDice (esso, epso)
  depiepso <- calcDice (epi, epso)
  diceepso <- data.frame (Elements = 1:length(dessoepso), 
                           ESSO = dessoepso, 
                           EPILONT = depiepso)
  return (diceepso)
}

#' Creates data frame for plotting the dice coefficients against ESSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return diceesso the data frame for the dice coefficient against ESSO
#'
#' @examples
#' \dontrun{
#' diceesso <- createDiceFrameESSO(epso, esso, epi)
#' }
createDiceFrameESSO <- function (epso, esso, epi) {
  depsoesso <- calcDice (epso, esso)
  depiesso <- calcDice (epi, esso)
  
  diceesso <- data.frame (Elements = 1:length(depsoesso), 
                          EpSO = depsoesso, 
                          EPILONT = depiesso)
  return (diceesso)
}

#' Creates data frame for plotting the jaccard coefficients against EpSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return jaccardepso the data frame for the jaccard coefficient against EpSO
#'
#' @examples
#' \dontrun{
#' jaccardepso <- createJaccardFrameEpSO(epso, esso, epi)
#' }
createJaccardFrameEpSO <- function (epso, esso, epi) {
  dessoepso <- calcJaccard (esso, epso)
  depiepso <- calcJaccard (epi, epso)
  jaccardepso <- data.frame (Elements = 1:length(dessoepso), 
                           ESSO = dessoepso, 
                           EPILONT = depiepso)
  return (jaccardepso)
}

#' Creates data frame for plotting the jaccard coefficients against ESSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return jaccardesso the data frame for the jaccard coefficient against ESSO
#'
#' @examples
#' \dontrun{
#' jaccardesso <- createJaccardFrameESSO(epso, esso, epi)
#' }
createJaccardFrameESSO <- function (epso, esso, epi) {
  depsoesso <- calcJaccard (epso, esso)
  depiesso <- calcJaccard (epi, esso)
  
  jaccardesso <- data.frame (Elements = 1:length(depsoesso), 
                           EpSO = depsoesso, 
                           EPILONT = depiesso)
  return (jaccardesso)
}

#' Creates data frame for plotting the cosine coefficients against EpSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return cosineepso the data frame for the cosine coefficient against EpSO
#'
#' @examples
#' \dontrun{
#' cosineepso <- createCosineFrameEpSO(epso, esso, epi)
#' }
createCosineFrameEpSO <- function (epso, esso, epi) {
  dessoepso <- calcCosine (esso, epso)
  depiepso <- calcCosine (epi, epso)
  cosineepso <- data.frame (Elements = 1:length(dessoepso), 
                               ESSO = dessoepso, 
                               EPILONT = depiepso)
  return (cosineepso)
}

#' Creates data frame for plotting the cosine coefficients against ESSO
#'
#' @param epso list with epso terms sorted by frequency
#' @param esso list with esso terms sorted by frequency
#' @param epi list with epi terms sorted by frequency
#'
#' @return cosineesso the data frame for the cosine coefficient against ESSO
#'
#' @examples
#' \dontrun{
#' cosineesso <- createCosineFrameESSO(epso, esso, epi)
#' }
createCosineFrameESSO <- function (epso, esso, epi) {
  depsoesso <- calcCosine (epso, esso)
  depiesso <- calcCosine (epi, esso)
  
  cosineesso <- data.frame (Elements = 1:length(depsoesso), 
                               EpSO = depsoesso, 
                               EPILONT = depiesso)
  return (cosineesso)
}