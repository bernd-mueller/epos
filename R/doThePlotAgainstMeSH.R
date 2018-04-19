#' Creates data frame for plotting the dice coefficients against MeSH
#'
#' @return ddice the data frame for the dice coefficient that can be used by createCosinePlot
#'
#' @examples
#' \dontrun{
#' createDiceFrame()
#' }
createDiceFrame <- function () {
  ddice <- data.frame (Elements = 1:length(ddrugbankmesh), DrugBank = ddrugbankmesh, EpSO = depsomesh, ESSO = dessomesh, EPILONT = depimesh)
  return (ddice)
}

#' Creates data frame for plotting the cosine coefficients against MeSH
#'
#' @return dcosine the data frame for the cosine coefficient that can be used by createCosinePlot
#'
#' @examples
#' \dontrun{
#' createCosineFrame()
#' }
createCosineFrame <- function () {
  dcosine <- data.frame (Elements = 1:length(cdrugbankmesh), DrugBank = cdrugbankmesh, EpSO = cepsomesh, ESSO = cessomesh, EPILONT = cepimesh)
  return (dcosine)
}

#' Creates data frame for plotting the jaccard coefficients against MeSH
#'
#' @return dcosine the data frame for the jaccard coefficient that can be used by createCosinePlot
#'
#' @examples
#' \dontrun{
#' createJaccardFrame()
#' }
createJaccardFrame <- function () {
  djaccard <- data.frame (Elements = 1:length(jdrugbankmesh), DrugBank = jdrugbankmesh, EpSO = jepsomesh, ESSO = jessomesh, EPILONT = jepimesh)
  return (djaccard)
}

#' Creates the plot for all jaccard coefficients against mesh
#'
#' @param djaccard the data frame containing the columns with the jaccard coefficients
#'
#' @return jaccardplot the ggplot object
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
#' createJaccardPlot(createJaccardFrame)
#' }
createJaccardPlot <- function (djaccard) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  Elements <- djaccard$Elements
  DrugBank <- djaccard$DrugBank
  jaccardplot <- ggplot2::ggplot(data = djaccard, aes(x=Elements, y=DrugBank, colour = "DrugBank"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Jaccard", title = "Jaccard Similarity Coefficient against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = djaccard, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = djaccard, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = djaccard, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual() 
  return (jaccardplot)
}

#' Creates the plot for all dice coefficients against mesh
#'
#' @param ddice the data frame containing the columns with the dice coefficients
#'
#' @return diceplot the ggplot object
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
#' createDicePlot(createDiceFrame)
#' }
createDicePlot <- function (ddice) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  diceplot <- ggplot2::ggplot(data = ddice, aes(x=Elements, y=DrugBank, colour = "DrugBank"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Dice", title = "Dice Similarity Coefficient against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = ddice, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = ddice, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = ddice, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    ggplot2::coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  diceplot
}

#' Creates the plot for all cosine coefficients against mesh
#'
#' @param dcosine the data frame containing the columns with the cosine coefficients
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
#' createCosinePlot(createCosineFrame)
#' }
createCosinePlot <- function (dcosine) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  cosineplot <- ggplot2::ggplot(data = dcosine, aes(x=Elements, y=DrugBank, colour = "DrugBank"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="Cosine", title = "Cosine Similarity Coefficient against MeSH") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = dcosine, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    ggplot2::geom_step(data = dcosine, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    ggplot2::geom_step(data = dcosine, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
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