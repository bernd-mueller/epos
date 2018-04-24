#' Creates the plot for all jaccard coefficients against drugbank
#'
#' @param djaccarddrugbank the data frame containing the columns with the jaccard coefficients
#'
#' @return jaccardplotdrugbank the ggplot object
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
#' jaccarddrugbankplot <- createJaccardPlotDrugBank(djaccarddrugbank)
#' }
createJaccardPlotDrugBank <- function (djaccarddrugbank) {
  cols <- c("MeSH" = "#40e0d0", "EpSO"="#3591d1","ESSO"="#62c76b","EPI"="#800080")
  
  jaccarddrugbankplot <- ggplot2::ggplot(data = djaccarddrugbank, ggplot2::aes_string(x="Elements", y="MeSH", colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = ggplot2::element_line(colour = "gray")) +
    ggplot2::labs (y="JaccardTopK", x="TopK", title = "JaccardTopK against ATC") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, ggplot2::aes_string(x="Elements", y="EpSO", colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, ggplot2::aes_string(x="Elements", y="ESSO", colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, ggplot2::aes_string(x="Elements", y="EPILONT", colour = "EPI"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 2603), limy = c(-0.001,0.06125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,2603)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.0125, 0.025, 0.0375, 0.05, 0.06125, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual() 
  return (jaccarddrugbankplot)
}

#' Creates the plot for all dice coefficients against drugbank
#'
#' @param ddicedrugbank the data frame containing the columns with the dice coefficients
#'
#' @return diceplotdrugbank the ggplot object
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
#' dicedrugbankplot <- createDicePlotDrugBank(ddicedrugbank)
#' }
createDicePlotDrugBank <- function (ddicedrugbank) {
  cols <- c("MeSH" = "#40e0d0", "EpSO"="#3591d1","ESSO"="#62c76b","EPI"="#800080")
  
  dicedrugbankplot <- ggplot2::ggplot(data = ddicedrugbank, ggplot2::aes_string(x="Elements", y="MeSH", colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = ggplot2::element_line(colour = "gray")) +
    ggplot2::labs (y="DiceTopK", x="TopK", title = "DiceTopK against ATC") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, ggplot2::aes_string(x="Elements", y="EpSO", colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, ggplot2::aes_string(x="Elements", y="ESSO", colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, ggplot2::aes_string(x="Elements", y="EPILONT", colour = "EPI"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 2603), limy = c(-0.001,0.06125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,2603)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.0125, 0.025, 0.0375, 0.05, 0.06125, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (dicedrugbankplot)
}

#' Creates the plot for all cosine coefficients against drugbank
#'
#' @param dcosinedrugbank the data frame containing the columns with the cosine coefficients
#'
#' @return cosineplotdrugbank the ggplot object
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
#' cosinedrugbankplot <- createCosinePlotDrugBank(dcosinedrugbank)
#' }
createCosinePlotDrugBank <- function (dcosinedrugbank) {
  cols <- c("MeSH" = "#40e0d0", "EpSO"="#3591d1","ESSO"="#62c76b","EPI"="#800080")
  
  cosinedrugbankplot <- ggplot2::ggplot(data = dcosinedrugbank, ggplot2::aes_string(x="Elements", y="MeSH", colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = ggplot2::element_line(colour = "gray")) +
    ggplot2::labs (y="CosineTopK", x="TopK", title = "CosineTopK against ATC") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = dcosinedrugbank, ggplot2::aes_string(x="Elements", y="EpSO", colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = dcosinedrugbank, ggplot2::aes_string(x="Elements", y="ESSO", colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = dcosinedrugbank, ggplot2::aes_string(x="Elements", y="EPILONT", colour = "EPI"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 2603), limy = c(-0.001,0.06125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,2603)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.0125, 0.025, 0.0375, 0.05, 0.06125, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (cosinedrugbankplot)
}