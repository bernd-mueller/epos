#' Creates the plot for all jaccard coefficients against mesh
#'
#' @param jaccardmesh the data frame containing the columns with the jaccard coefficients
#'
#' @return jaccardmeshplot the ggplot object
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
#' jaccardmeshplot <- createJaccardPlotMeSH(jaccardmesh)
#' }
createJaccardPlotMeSH <- function (jaccardmesh) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  jaccardmeshplot <- ggplot2::ggplot(data = jaccardmesh, ggplot2::aes_string(x="Elements", 
                                                                              y="DrugBank", colour = shQuote("DrugBank")), log10="x") + 
    ggplot2::theme_minimal ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), 
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=10),
                   legend.position=c(0,1), 
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 10)) +
    ggplot2::labs (y="JaccardTopK", x="TopK", title = "JaccardTopK against MeSH", subtitle = "") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = jaccardmesh, ggplot2::aes_string(x="Elements", y="EpSO", colour = shQuote("EpSO")), size=1) + 
    ggplot2::geom_step(data = jaccardmesh, ggplot2::aes_string(x="Elements", y="ESSO", colour = shQuote("ESSO")), size=1) + 
    ggplot2::geom_step(data = jaccardmesh, ggplot2::aes_string(x="Elements", y="EPILONT", colour = shQuote("EPILONT")), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 28107), limy = c(-0.001,0.15)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000,28107)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15)) +
    ggplot2::scale_colour_manual(values=cols)+
    ggplot2::scale_size_manual() 
  return (jaccardmeshplot)
}

#' Creates the plot for all dice coefficients against mesh
#'
#' @param dicemesh the data frame containing the columns with the dice coefficients
#'
#' @return diceplotmesh the ggplot object
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
#' dicemeshplot <- createDicePlotMeSH(dicemesh)
#' }
createDicePlotMeSH <- function (dicemesh) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  dicemeshplot <- ggplot2::ggplot(data = dicemesh, ggplot2::aes_string(x="Elements", 
                                                                        y="DrugBank",  colour = shQuote("DrugBank")), log10="x") + 
    ggplot2::theme_minimal ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), 
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=10),
                   legend.position=c(0,1), 
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 10)) +
    ggplot2::labs (y="DiceTopK", x= "TopK", title = "DiceTopK against MeSH", subtitle = "") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = dicemesh, ggplot2::aes_string(x="Elements", y="EpSO", colour = shQuote("EpSO")), size=1) + 
    ggplot2::geom_step(data = dicemesh, ggplot2::aes_string(x="Elements", y="ESSO", colour = shQuote("ESSO")), size=1) + 
    ggplot2::geom_step(data = dicemesh, ggplot2::aes_string(x="Elements", y="EPILONT", colour = shQuote("EPILONT")), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 28107), limy = c(-0.001,0.15)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000,28107)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15)) +
    ggplot2::scale_colour_manual(values=cols)+
    ggplot2::scale_size_manual()   
  return (dicemeshplot)
}

#' Creates the plot for all cosine coefficients against mesh
#'
#' @param cosinemesh the data frame containing the columns with the cosine coefficients
#'
#' @return cosineplot the ggplot object
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
#' cosinemeshplot <- createCosinePlotMeSH(cosinemesh)
#' }
createCosinePlotMeSH <- function (cosinemesh) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  cosinemeshplot <- ggplot2::ggplot(data = cosinemesh, ggplot2::aes_string(x="Elements", y="DrugBank", colour = shQuote("DrugBank")), log10="x") + 
    ggplot2::theme_minimal ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), 
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=10),
                   legend.position=c(0,1), 
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 10)) +
    ggplot2::labs (y="CosineTopK", x= "TopK", title = "CosineTopK against MeSH", subtitle = "") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = cosinemesh, ggplot2::aes_string(x="Elements", y="EpSO", colour = shQuote("EpSO")), size=1) + 
    ggplot2::geom_step(data = cosinemesh, ggplot2::aes_string(x="Elements", y="ESSO", colour = shQuote("ESSO")), size=1) + 
    ggplot2::geom_step(data = cosinemesh, ggplot2::aes_string(x="Elements", y="EPILONT", colour = shQuote("EPILONT")), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 28107), limy = c(-0.001,0.15)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000,28107)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15)) +
    ggplot2::scale_colour_manual(values=cols)+
    ggplot2::scale_size_manual()   
  return (cosinemeshplot)
}
