#' Create aggregated plot with mean and standard deviation of all similarity metrics against MeSH
#'
#' @param cosinemesh data.frame containing the vector with cosine metrics for DrugBank, EpSO, ESSO, and EPILONT against MeSH
#' @param dicemesh data.frame containing the vector with dice metrics for DrugBank, EpSO, ESSO, and EPILONT against MeSH
#' @param jaccardmesh data.frame containing the vector with jaccard metrics for DrugBank, EpSO, ESSO, and EPILONT against MeSH
#'
#' @return aggmesh the plot with the aggregated graphs
#' @export
#'
#' @examples
#' \dontrun{
#' aggmesh <- aggregatedPlotMeSH(cosinemesh, dicemesh, jaccardmesh)
#' }
aggregatedPlotMeSH <- function (cosinemesh, dicemesh, jaccardmesh) {
  # DrugBank: darkturquoise     #00CED1
  # EpSO: blue        #0000ff
  # ESSO: orange      #FFA500
  # EPILONT: purple   #800080
  cols <- c(DrugBank = "#00CED1", EpSO="#0000FF",ESSO="#FFA500",EPILONT="#800080")
  
  myalpha = shQuote("0.2")
  mymeansize = 3
  mysdsize = 1
  
  drug_metrics <- create_metrics (cosinemesh$DrugBank, dicemesh$DrugBank, jaccardmesh$DrugBank, jaccardmesh$Elements)
  drug_stats <- create_stats(drug_metrics)
  
  epso_metrics <- create_metrics (cosinemesh$EpSO, dicemesh$EpSO, jaccardmesh$EpSO, jaccardmesh$Elements)
  epso_stats <- create_stats(epso_metrics)
  
  esso_metrics <- create_metrics (cosinemesh$ESSO, dicemesh$ESSO, jaccardmesh$ESSO, jaccardmesh$Elements)
  esso_stats <- create_stats(esso_metrics)
  
  epi_metrics <- create_metrics (cosinemesh$EPILONT, dicemesh$EPILONT, jaccardmesh$EPILONT, jaccardmesh$Elements)
  epi_stats <- create_stats(epi_metrics)
  
  epimaxsd <- epi_stats$comean + epi_stats$cosd
  epimaxsd <- epimaxsd[epimaxsd > 1] <- 1
  
  aggmesh <- ggplot2::ggplot(
    data = drug_stats, 
    ggplot2::aes_string(x="elements", y="comean", colour = shQuote("DrugBank"))
    ) + 
    ggplot2::geom_step(size = mymeansize) +
    ggplot2::geom_ribbon(
      data = drug_stats,
      ggplot2::aes_string(x = "elements", ymin = "comean - cosd", ymax = "comean + cosd", colour = shQuote("DrugBank"), fill = shQuote("DrugBank"), alpha = myalpha), show.legend = FALSE, size = mysdsize
    ) +
    ggplot2::geom_step(
      data = epso_stats,
      ggplot2::aes_string(x="elements", y="comean", colour = shQuote("EpSO")), size = mymeansize
    ) +
    ggplot2::geom_ribbon(
      data = epso_stats,
      ggplot2::aes_string(x = "elements", ymin = "comean - cosd", ymax = "comean + cosd", colour = shQuote("EpSO"), fill = shQuote("EpSO"), alpha = myalpha), show.legend = FALSE, size = mysdsize
    ) +
    ggplot2::geom_step(
      data = esso_stats,
      ggplot2::aes_string(x="elements", y="comean", colour = shQuote("ESSO")), size = mymeansize
    ) +
    ggplot2::geom_ribbon(
      data = esso_stats,
      ggplot2::aes_string(x = "elements", ymin = "comean - cosd", ymax = "comean + cosd", colour = shQuote("ESSO"), fill = shQuote("ESSO"), alpha = myalpha), show.legend = FALSE, size = mysdsize
    ) +
    ggplot2::geom_step(
      data = epi_stats, 
      ggplot2::aes_string(x="elements", y="comean", colour = shQuote("EPILONT")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = epi_stats, 
      ggplot2::aes_string(x = "elements", ymin = "comean - cosd", ymax = "epimaxsd", colour = shQuote("EPILONT"), fill = shQuote("EPILONT"), alpha = myalpha), show.legend = FALSE, size = mysdsize    
    ) + 
    ggplot2::coord_cartesian (xlim = c(0,28108), ylim = c(0.875,1)) +
    ggplot2::scale_x_continuous(breaks = c(0, 5000,10000,15000, 20000, 25000, 28108)) +
    ggplot2::scale_y_continuous(breaks = c(0.875, 0.9, 0.925, 0.95, 0.975, 1)) +
    ggplot2::theme_minimal () +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"),
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=14),
                   legend.position=c(0,1),
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 11),
                   axis.text.y = ggplot2::element_text(size = 11)) +
    ggplot2::labs (y="Mean", x="TopK", title = "Mean of Cosine, Dice, and Jaccard with Standard Deviation against MeSH", subtitle = "") +
    ggplot2::scale_colour_manual(values=cols)+
    ggplot2::scale_fill_manual(values=cols) +
    ggplot2::scale_size_manual()
  return (aggmesh)
}