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
#'
#' @examples
#' \dontrun{
#' createJaccardPlotDrugBank(createJaccardFrameDrugBank())
#' }
createJaccardPlotDrugBank <- function (djaccarddrugbank) {
  cols <- c("MeSH" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccarddrugbank %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  #Elements <- djaccarddrugbank$Elements
  #DrugBank <- djaccarddrugbank$DrugBank
  jaccardplotdrugbank <- ggplot2::ggplot(data = djaccarddrugbank, aes(x=Elements, y=MeSH, colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Jaccard", title = "Jaccard Similarity Coefficient against DrugBank") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual() 
  return (jaccardplotdrugbank)
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
#'
#' @examples
#' \dontrun{
#' createDicePlotDrugBank(createDiceFrameDrugBank())
#' }
createDicePlotDrugBank <- function (ddicedrugbank) {
  cols <- c("MeSH" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  diceplotdrugbank <- ggplot2::ggplot(data = ddicedrugbank, aes(x=Elements, y=MeSH, colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Dice", title = "Dice Similarity Coefficient against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (diceplotdrugbank)
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
#'
#' @examples
#' \dontrun{
#' createCosinePlotMeSH(createCosineFrame)
#' }
createCosinePlotMeSH <- function (dcosinemesh) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  cosineplot <- ggplot2::ggplot(data = dcosinemesh, aes(x=Elements, y=MeSH, colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Cosine", title = "Cosine Similarity Coefficient against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = dcosinemesh, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = dcosinemesh, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = dcosinemesh, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (cosineplot)
}

#coord_cartesian(xlim = c(0, 10000)) 
#scale_colour_manual(name="Dictionary",values=cols) +


#geom_text(data = epsolabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1)

# drugbanklabel <- djaccard %>% filter(Elements == max(Elements)) %>% mutate(point_label = "DrugBank")