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
  cols <- c("MeSH" = "#40e0d0", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  #drugbanklabel <- djaccarddrugbank %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  #Elements <- djaccarddrugbank$Elements
  #DrugBank <- djaccarddrugbank$DrugBank
  jaccardplotdrugbank <- ggplot2::ggplot(data = djaccarddrugbank, aes(x=Elements, y=MeSH, colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Jaccard", title = "Jaccard Similarity Coefficient against ATC") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = djaccarddrugbank, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 2603), limy = c(-0.001,0.06125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,2603)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.0125, 0.025, 0.0375, 0.05, 0.06125, 0.1)) +
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
  cols <- c("MeSH" = "#40e0d0", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  diceplotdrugbank <- ggplot2::ggplot(data = ddicedrugbank, aes(x=Elements, y=MeSH, colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Dice", title = "Dice Similarity Coefficient against ATC") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = ddicedrugbank, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 2603), limy = c(-0.001,0.06125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,2603)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.0125, 0.025, 0.0375, 0.05, 0.06125, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (diceplotdrugbank)
}

#' Creates the plot for all cosine coefficients against drugbank
#'
#' @param dcosinedrugbank the data frame containing the columns with the cosine coefficients
#'
#' @return dcosineplotdrugbank the ggplot object
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
#' createCosinePlotDrugBank(createCosineFrameDrugBank())
#' }
createCosinePlotDrugBank <- function (dcosinedrugbank) {
  cols <- c("MeSH" = "#40e0d0", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#800080")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  dcosineplotdrugbank <- ggplot2::ggplot(data = dcosinedrugbank, aes(x=Elements, y=MeSH, colour = "MeSH"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Cosine", title = "Cosine Similarity Coefficient against ATC") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = dcosinedrugbank, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = dcosinedrugbank, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = dcosinedrugbank, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 2603), limy = c(-0.001,0.06125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,2603)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.0125, 0.025, 0.0375, 0.05, 0.06125, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (dcosineplotdrugbank)
}

#coord_cartesian(xlim = c(0, 10000)) 
#scale_colour_manual(name="Dictionary",values=cols) +


#geom_text(data = epsolabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1)

# drugbanklabel <- djaccard %>% filter(Elements == max(Elements)) %>% mutate(point_label = "DrugBank")