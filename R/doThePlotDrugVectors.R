#' Creates the plot for all jaccard coefficients amongst the three epilepsy ontologies
#'
#' @param jaccardepso the data frame containing the columns with the jaccard coefficients of epi and esso against epso
#' @param jaccardesso the data frame containing the columns with the jaccard coefficients of epi and epso against esso
#'
#' @return jaccardepilepsyplot the ggplot object
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
#' jaccardepilepsyplot <- createJaccardPlotAll(jaccardepso, jaccardesso)
#' }
createJaccardPlotDBMeSH <-
  function (jmeshepso, jmeshesso, jmeshepi) {
    djmesh <- data.frame (
      Elements = 1:250,
      EpSO = jmeshepso[1:250],
      ESSO = jmeshesso[1:250],
      EPILONT = jmeshepi[1:250]
    )
    cols <-
      c("EpSO" = "#8A2BE2",
        "ESSO" = "#7CFC00",
        "EPILONT" = "#C71585")
    
    jaccarddbmesh <- ggplot2::ggplot(data = djmesh,
                                     ggplot2::aes_string(
                                       x = "Elements",
                                       y = "EpSO",
                                       colour = shQuote("EpSO")
                                     )) +
      ggplot2::theme_minimal () +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "gray"),
        panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
        legend.text = ggplot2::element_text(size =
                                              11),
        legend.position = c(0, 1),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11)
      ) +
      ggplot2::labs (
        y = "Jaccard",
        x = "Length",
        title = "Jaccard Similarity between DrugBank vectors of Epilepsy ontologies versus MeSH derived DrugBank vector",
        subtitle = ""
      ) +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "ESSO",
          colour = shQuote("ESSO")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPILONT",
          colour = shQuote("EPILONT")
        ),
        size = 1
      ) +
      ggplot2::coord_trans(limx = c(0, 250), limy = c(0, 1)) +
      ggplot2::scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250)) +
      ggplot2::scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
      ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
      ggplot2::scale_size_manual()
    return (jaccarddbmesh)
  }

#' Creates the plot for all jaccard coefficients amongst the three epilepsy ontologies
#'
#' @param jaccardepso the data frame containing the columns with the jaccard coefficients of epi and esso against epso
#' @param jaccardesso the data frame containing the columns with the jaccard coefficients of epi and epso against esso
#'
#' @return jaccardepilepsyplot the ggplot object
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
#' jaccardepilepsyplot <- createJaccardPlotAll(jaccardepso, jaccardesso)
#' }
createJaccardPlotMeSHFive <-
  function (jmeshepso, jmeshesso, jmeshepi, jmeshepilepsyand, jmeshepilepsyor) {
    djmesh <- data.frame (
      Elements = 1:962,
      EpSO = jmeshepso[1:962],
      ESSO = jmeshesso[1:962],
      EPILONT = jmeshepi[1:962],
      EPAND = jmeshepilepsyand[1:962],
      EPOR = jmeshepilepsyor[1:962]
    )
    cols <-
      c("EpSO" = "#800080", #purple
        "ESSO" = "#FFFF00", #yellow
        "EPILONT" = "#0000FF", #blue
        "EPAND" = "#008000", #green
        "EPOR" = "#FF0000" #red
        )
    
    jaccarddbmesh <- ggplot2::ggplot(data = djmesh,
                                     ggplot2::aes_string(
                                       x = "Elements",
                                       y = "EpSO",
                                       colour = shQuote("EpSO")
                                     )) +
      ggplot2::theme_minimal () +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "gray"),
        panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
        legend.text = ggplot2::element_text(size =
                                              11),
        legend.position = c(0, 1),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11)
      ) +
      ggplot2::labs (
        y = "Jaccard",
        x = "Length",
        title = "Jaccard Similarity between DrugBank vectors of Epilepsy ontologies versus MeSH derived DrugBank vector",
        subtitle = ""
      ) +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "ESSO",
          colour = shQuote("ESSO")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPILONT",
          colour = shQuote("EPILONT")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPAND",
          colour = shQuote("EPAND")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPOR",
          colour = shQuote("EPOR")
        ),
        size = 1
      ) +
      ggplot2::coord_trans(limx = c(0, 100), limy = c(0, 1)) +
      ggplot2::scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
      ggplot2::scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
      ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
      ggplot2::scale_size_manual()
    return (jaccarddbmesh)
  }