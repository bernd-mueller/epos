#' Creates the plot for all jaccard coefficients amongst the three epilepsy ontologies
#'
#' @param djaccardepso the data frame containing the columns with the jaccard coefficients of epi and esso against epso
#' @param djaccardesso the data frame containing the columns with the jaccard coefficients of epi and epso against esso
#'
#' @return jaccardepilepsyplot the ggplot object
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
#' jaccardepilepsyplot <- createJaccardPlotAll(djaccardepso, djaccardesso)
#' }
createJaccardPlotAll <- function (djaccardepso, djaccardesso) {
  cols <- c("ESSO vs. EpSO" = "#8A2BE2", "EPI vs. EpSO"="#7CFC00", "EPI vs. ESSO"="#C71585")
  
  jaccardepilepsyplot <- ggplot2::ggplot(data = djaccardepso, ggplot2::aes_string(x="Elements", 
                                                                                  y="ESSO", 
                                                                                  colour = "ESSO vs. EpSO"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="JaccardTopK", x="TopK", title = "JaccardTopK of all Epilepsy Ontologies") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = djaccardepso, ggplot2::aes(x="Elements", y="EPI", colour = "EPI vs. EpSO"), size=1) + 
    ggplot2::geom_step(data = djaccardesso, ggplot2::aes(x="Elements", y="EPI", colour = "EPI vs. ESSO"), size=1) +
    ggplot2::coord_trans(x = "log10", limx = c(1, 918), limy = c(-0.001,0.2575)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 918)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.2575)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (jaccardepilepsyplot)
}

#' Creates the plot for all dice coefficients amongst the three epilepsy ontologies
#'
#' @param ddiceepso the data frame containing the columns with the dice coefficients of epi and esso against epso
#' @param ddiceesso the data frame containing the columns with the dice coefficients of epi and epso against esso
#'
#' @return diceepilepsyplot the ggplot object
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
#' diceepilepsyplot <- createDicePlotAll(ddiceepso, ddiceesso)
#' }
createDicePlotAll <- function (ddiceepso, ddiceesso) {
  cols <- c("ESSO vs. EpSO" = "#8A2BE2", "EPI vs. EpSO"="#7CFC00", "EPI vs. ESSO"="#C71585")
  
  diceepilepsyplot <- ggplot2::ggplot(data = ddiceepso, ggplot2::aes_string(x="Elements", 
                                                                            y="ESSO", 
                                                                            colour = "ESSO vs. EpSO"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="DiceTopK", x="TopK", title = "DiceTopK of all Epilepsy Ontologies") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = ddiceepso, ggplot2::aes_string(x="Elements", y="EPI", colour = "EPI vs. EpSO"), size=1) + 
    ggplot2::geom_step(data = ddiceesso, ggplot2::aes_string(x="Elements", y="EPI", colour = "EPI vs. ESSO"), size=1) +
    ggplot2::coord_trans(x = "log10", limx = c(1, 918), limy = c(-0.001,0.2575)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 918)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.2575)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (diceepilepsyplot)
}

#' Creates the plot for all cosine coefficients against drugbank
#'
#' @param dcosineepso the data frame containing the columns with the cosine coefficients of epi and esso against epso
#' @param dcosineesso the data frame containing the columns with the cosine coefficients of epi and epso against esso
#'
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
#' cosineepilepsyplot <- createCosinePlotAll(dcosineepso, dcosineesso)
#' }
createCosinePlotAll <- function(dcosineepso, dcosineesso) {
  cols <- c("ESSO vs. EpSO" = "#8A2BE2", "EPI vs. EpSO"="#7CFC00", "EPI vs. ESSO"="#C71585")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  cosineepilepsyplot <- ggplot2::ggplot(data = dcosineepso, ggplot2::aes_string(x="Elements", 
                                                                                y="ESSO", 
                                                                                colour = "ESSO vs. EpSO"), log10="x") + 
    ggplot2::theme_classic ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    ggplot2::labs (y="CosineTopK", x="TopK", title = "CosineTopK of all Epilepsy Ontologies") +
    ggplot2::geom_step(size=1) + 
    ggplot2::geom_step(data = dcosineepso, ggplot2::aes_string(x="Elements", y="EPI", colour = "EPI vs. EpSO"), size=1) + 
    ggplot2::geom_step(data = dcosineesso, ggplot2::aes_string(x="Elements", y="EPI", colour = "EPI vs. ESSO"), size=1) +
    ggplot2::coord_trans(x = "log10", limx = c(1, 918), limy = c(-0.001,0.2575)) +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 100, 918)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.2575)) +
    ggplot2::scale_colour_manual(name="Dictionary",values=cols)+
    ggplot2::scale_size_manual()   
  return (cosineepilepsyplot)
}