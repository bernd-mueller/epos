#' Creates the plot for all jaccard coefficients against mesh
#'
#' @param djaccardmesh the data frame containing the columns with the jaccard coefficients
#'
#' @return jaccardplotmesh the ggplot object
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 element_line
#'
#' @examples
#' \dontrun{
#' createJaccardPlotMeSH(createJaccardFrameMeSH())
#' }
createJaccardPlotMeSH <- function (djaccardmesh) {
  cols <- c("ATC" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPI"="#800080")
  
  jaccardplotmesh <- ggplot2::ggplot(data = djaccardmesh, ggplot2::aes_string(x="Elements", 
                                                                              y="DrugBank", colour = "ATC"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = ggplot2::element_line(colour = "gray")) +
    ggplot2::labs (y="JaccardTopK", x="TopK", title = "JaccardTopK against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = djaccardmesh, ggplot2::aes_string(x="Elements", y="EpSO", colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = djaccardmesh, ggplot2::aes_string(x="Elements", y="ESSO", colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = djaccardmesh, ggplot2::aes_string(x="Elements", y="EPILONT", colour = "EPI"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 28107), limy = c(-0.001,0.15)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000,28107)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual() 
  return (jaccardplotmesh)
}

#' Creates the plot for all dice coefficients against mesh
#'
#' @param ddicemesh the data frame containing the columns with the dice coefficients
#'
#' @return diceplotmesh the ggplot object
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 element_line
#' 
#' @examples
#' \dontrun{
#' createDicePlotMeSH(createDiceFrameMeSH())
#' }
createDicePlotMeSH <- function (ddicemesh) {
  cols <- c("ATC" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPI"="#800080")
  
  diceplotmesh <- ggplot2::ggplot(data = ddicemesh, ggplot2::aes_string(x="Elements", 
                                                                        y="DrugBank", colour = "ATC"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = ggplot2::element_line(colour = "gray")) +
    ggplot2::labs (y="DiceTopK", x= "TopK", title = "DiceTopK against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = ddicemesh, ggplot2::aes_string(x="Elements", y="EpSO", colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = ddicemesh, ggplot2::aes_string(x="Elements", y="ESSO", colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = ddicemesh, ggplot2::aes_string(x="Elements", y="EPILONT", colour = "EPI"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 28107), limy = c(-0.001,0.15)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000,28107)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (diceplotmesh)
}

#' Creates the plot for all cosine coefficients against mesh
#'
#' @param dcosinemesh the data frame containing the columns with the cosine coefficients
#'
#' @return cosineplot the ggplot object
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 element_line
#' 
#' @examples
#' \dontrun{
#' createCosinePlotMeSH(createCosineFrameMeSH())
#' }
createCosinePlotMeSH <- function (dcosinemesh) {
  cols <- c("ATC" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPI"="#800080")
  
  dcosineplotmesh <- ggplot2::ggplot(data = dcosinemesh, ggplot2::aes_string(x="Elements", y="DrugBank", colour = "ATC"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = ggplot2::element_line(colour = "gray")) +
    ggplot2::labs (y="CosineTopK", x="TopK", title = "CosineTopK against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = dcosinemesh, ggplot2::aes_string(x="Elements", y="EpSO", colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = dcosinemesh, ggplot2::aes_string(x="Elements", y="ESSO", colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = dcosinemesh, ggplot2::aes_string(x="Elements", y="EPILONT", colour = "EPI"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 28107), limy = c(-0.001,0.15)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000,28107)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (dcosineplotmesh)
}
