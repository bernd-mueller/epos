#' Creates the plot for all jaccard coefficients against drugbank
#'
#' @param jaccarddrugbank the data frame containing the columns with the jaccard coefficients
#'
#' @return jaccardplotdrugbank the ggplot object
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
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
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#'
#' @examples
#' \dontrun{
#' jaccarddrugbankplot <- createJaccardPlotDrugBank(jaccarddrugbank)
#' }
createJaccardPlotDrugBank <- function (jaccarddrugbank) {
  cols <- c("MeSH" = "#ff0000", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  jaccarddrugbankplot <- ggplot2::ggplot(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="MeSH", colour = shQuote("MeSH")), log10="x") + 
    ggplot2::theme_minimal ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), 
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=11),
                   legend.position=c(0,1), 
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 11)) +
    ggplot2::labs (y="JaccardTopK", x="TopK", title = "JaccardTopK against ATC", subtitle = "") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="EpSO", colour = shQuote("EpSO")), size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="ESSO", colour = shQuote("ESSO")), size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="EPILONT", colour = shQuote("EPILONT")), size=1) + 
    ggplot2::coord_trans(limx = c(1,2603),limy = c(0.9375,1)) +
    ggplot2::scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2603)) +
    ggplot2::scale_y_continuous(breaks = c(0.9, 0.9125, 0.925, 0.9375, 0.95, 0.9625, 0.975, 0.9875, 1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()
  return (jaccarddrugbankplot)
}

#' Creates the plot for all dice coefficients against drugbank
#'
#' @param dicedrugbank the data frame containing the columns with the dice coefficients
#'
#' @return diceplotdrugbank the ggplot object
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
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
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#'
#' @examples
#' \dontrun{
#' dicedrugbankplot <- createDicePlotDrugBank(dicedrugbank)
#' }
createDicePlotDrugBank <- function (dicedrugbank) {
  cols <- c("MeSH" = "#ff0000", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  dicedrugbankplot <- ggplot2::ggplot(data = dicedrugbank, ggplot2::aes_string(x="Elements", y="MeSH", colour = shQuote("MeSH")), log10="x") + 
    ggplot2::theme_minimal ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), 
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=11),
                   legend.position=c(0,1), 
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 11)) +
    ggplot2::labs (y="DiceTopK", x="TopK", title = "DiceTopK against ATC", subtitle = "") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="EpSO", colour = shQuote("EpSO")), size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="ESSO", colour = shQuote("ESSO")), size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="EPILONT", colour = shQuote("EPILONT")), size=1) + 
    ggplot2::coord_trans(limx = c(1,2603),limy = c(0.9375,1)) +
    ggplot2::scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2603)) +
    ggplot2::scale_y_continuous(breaks = c(0.9, 0.9125, 0.925, 0.9375, 0.95, 0.9625, 0.975, 0.9875, 1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (dicedrugbankplot)
}

#' Creates the plot for all cosine coefficients against drugbank
#'
#' @param cosinedrugbank the data frame containing the columns with the cosine coefficients
#'
#' @return cosineplotdrugbank the ggplot object
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
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
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' 
#' @examples
#' \dontrun{
#' cosinedrugbankplot <- createCosinePlotDrugBank(cosinedrugbank)
#' }
createCosinePlotDrugBank <- function (cosinedrugbank) {
  cols <- c("MeSH" = "#ff0000", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  cosinedrugbankplot <- ggplot2::ggplot(data = cosinedrugbank, ggplot2::aes_string(x="Elements", y="MeSH", colour = shQuote("MeSH")), log10="x") + 
    ggplot2::theme_minimal ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), 
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=11),
                   legend.position=c(0,1), 
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 11)) +
    ggplot2::labs (y="CosineTopK", x="TopK", title = "CosineTopK against ATC", subtitle = "") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="EpSO", colour = shQuote("EpSO")), size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="ESSO", colour = shQuote("ESSO")), size=1) + 
    ggplot2::geom_step(data = jaccarddrugbank, ggplot2::aes_string(x="Elements", y="EPILONT", colour = shQuote("EPILONT")), size=1) + 
    ggplot2::coord_trans(limx = c(1,2603),limy = c(0.9375,1)) +
    ggplot2::scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2603)) +
    ggplot2::scale_y_continuous(breaks = c(0.9, 0.9125, 0.925, 0.9375, 0.95, 0.9625, 0.975, 0.9875, 1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (cosinedrugbankplot)
}