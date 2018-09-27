#' Creates data frame for plotting the cosine coefficients against ESSO
#'
#' @return cosineesso the data frame for the cosine coefficient against ESSO
#'
#' @examples
#' cosineesso <- createCosineFrameESSO()
#' 
#' @export
createCosineFrameESSO <- function () {
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  
  depsoesso <- calcCosine (epso, esso)
  depiesso <- calcCosine (epi, esso)
  
  cosineesso <- data.frame (Elements = 1:length(depsoesso), 
                            EpSO = depsoesso, 
                            EPILONT = depiesso)
  return (cosineesso)
}


#' Creates data frame for plotting the dice coefficients against ESSO
#'
#' @return diceesso the data frame for the dice coefficient against ESSO
#'
#' @examples
#' diceesso <- createDiceFrameESSO()
#' 
#' @export
createDiceFrameESSO <- function () {
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  
  depsoesso <- calcDice (epso, esso)
  depiesso <- calcDice (epi, esso)
  
  diceesso <- data.frame (Elements = 1:length(depsoesso), 
                          EpSO = depsoesso, 
                          EPILONT = depiesso)
  return (diceesso)
}

#' Creates data frame for plotting the jaccard coefficients against EpSO
#'
#' @return jaccardepso the data frame for the jaccard coefficient against EpSO
#'
#' @examples
#' jaccardepso <- createJaccardFrameEpSO()
#' 
#' @export
createJaccardFrameEpSO <- function () {
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  dessoepso <- calcJaccard (esso, epso)
  depiepso <- calcJaccard (epi, epso)
  jaccardepso <- data.frame (Elements = 1:length(dessoepso), 
                           ESSO = dessoepso, 
                           EPILONT = depiepso)
  return (jaccardepso)
}

#' Creates data frame for plotting the jaccard coefficients against ESSO
#'
#' @return jaccardesso the data frame for the jaccard coefficient against ESSO
#'
#' @examples
#' jaccardesso <- createJaccardFrameESSO()
#' 
#' @export
createJaccardFrameESSO <- function () {
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  
  depsoesso <- calcJaccard (epso, esso)
  depiesso <- calcJaccard (epi, esso)
  
  jaccardesso <- data.frame (Elements = 1:length(depsoesso), 
                           EpSO = depsoesso, 
                           EPILONT = depiesso)
  return (jaccardesso)
}

#' Creates data frame for plotting the cosine coefficients against EpSO
#'
#' @return cosineepso the data frame for the cosine coefficient against EpSO
#'
#' @examples
#' cosineepso <- createCosineFrameEpSO()
#' 
#' @export
createCosineFrameEpSO <- function () {
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  dessoepso <- calcCosine (esso, epso)
  depiepso <- calcCosine (epi, epso)
  cosineepso <- data.frame (Elements = 1:length(dessoepso), 
                               ESSO = dessoepso, 
                               EPILONT = depiepso)
  return (cosineepso)
}

#' Creates data frame for plotting the dice coefficients against EpSO
#'
#' @return diceepso the data frame for the dice coefficient against EpSO
#'
#' @examples
#' diceepso <- createDiceFrameEpSO()
#' 
#' @export
createDiceFrameEpSO <- function () {
  epso <- loadDictionaryFrequencyEpSO()
  esso <- loadDictionaryFrequencyESSO()
  epi <- loadDictionaryFrequencyEPILONT()
  dessoepso <- calcDice (esso, epso)
  depiepso <- calcDice (epi, epso)
  diceepso <- data.frame (Elements = 1:length(dessoepso), 
                          ESSO = dessoepso, 
                          EPILONT = depiepso)
  return (diceepso)
}