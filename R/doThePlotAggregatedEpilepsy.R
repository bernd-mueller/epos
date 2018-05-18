#' Title
#'
#' @param cosineepso 
#' @param cosineesso 
#' @param diceepso 
#' @param diceesso 
#' @param jaccardesso 
#' @param jaccardepso 
#'
#' @return aggeps
#' @export
#'
#' @examples
#' \dontrun{
#' aggeps <- aggregatedPlotEpilepsie(cosineepso, cosineesso, diceepso, diceesso, jaccardesso, jaccardepso)
#' }
aggregatedPlotEpilepsie <- function (cosineepso, cosineesso, diceepso, diceesso, jaccardesso, jaccardepso) {
  # DrugBank: darkturquoise     #00CED1
  # EpSO: blue        #0000ff
  # ESSO: orange      #FFA500
  # EPILONT: purple   #800080
  cols <- c("EpSO vs. ESSO" = "#00CED1", "EpSO vs. EPILONT" ="#0000FF","ESSO vs. EPILONT"="#FFA500",EPILONT="#800080")
  
  myalpha = shQuote("0.2")
  mymeansize = 3
  mysdsize = 1
  
  epsoesso_metrics <- create_metrics (cosineepso$ESSO, diceepso$ESSO, jaccardepso$ESSO, jaccardepso$Elements)
  epsoesso_stats <- create_stats(epsoesso_metrics)

  epsoepilont_metrics <- create_metrics (cosineepso$EPILONT, diceepso$EPILONT, jaccardepso$EPILONT, jaccardepso$Elements)
  epsoepilont_stats <- create_stats(epsoepilont_metrics)

  essoepilont_metrics <- create_metrics (cosineesso$EPILONT, diceesso$EPILONT, jaccardesso$EPILONT, jaccardesso$Elements)
  essoepilont_stats <- create_stats(essoepilont_metrics)

  aggeps <- ggplot2::ggplot(
    data = epsoesso_stats, 
    ggplot2::aes_string(x="elements", y="comean", colour = shQuote("EpSO vs. ESSO"))
  ) + 
    ggplot2::geom_step(size = mymeansize) +
    ggplot2::geom_ribbon(
      data = epsoesso_stats, 
      ggplot2::aes_string(x = "elements", ymin = "comean - cosd", ymax = "comean + cosd", colour = shQuote("EpSO vs. ESSO"), fill = shQuote("EpSO vs. ESSO"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::geom_step(
      data = epsoepilont_stats, 
      ggplot2::aes_string(x="elements", y="comean", colour = shQuote("EpSO vs. EPILONT")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = epsoepilont_stats, 
      ggplot2::aes_string(x = "elements", ymin = "comean - cosd", ymax = "comean + cosd", colour = shQuote("EpSO vs. EPILONT"), fill = shQuote("EpSO vs. EPILONT"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::geom_step(
      data = essoepilont_stats, 
      ggplot2::aes_string(x="elements", y="comean", colour = shQuote("ESSO vs. EPILONT")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = essoepilont_stats, 
      ggplot2::aes_string(x = "elements", ymin = "comean - cosd", ymax = "comean + cosd", colour = shQuote("ESSO vs. EPILONT"), fill = shQuote("ESSO vs. EPILONT"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::coord_cartesian (xlim = c(0,919), ylim = c(0.8475,1)) +
    ggplot2::scale_x_continuous(breaks = c(0, 250,500, 750, 919)) +
    ggplot2::scale_y_continuous(breaks = c(0.85, 0.875, 0.9, 0.925, 0.95, 0.975, 1)) +
    ggplot2::theme_minimal () +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"),
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=16, face = "bold"),
                   legend.position=c(0,1),
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 16, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14)) +
    ggplot2::labs (y="Mean", x="TopK", title = "", subtitle = "") +
#    ggplot2::labs (y="Mean", x="TopK", title = "Mean of Cosine, Dice, and Jaccard with Standard Deviation of EpSO vs. ESSO, EpSO vs. EPILONT, and ESSO vs. EPILONT", subtitle = "") +
    ggplot2::scale_colour_manual(values=cols)+
    ggplot2::scale_fill_manual(values=cols) +
    ggplot2::scale_size_manual()
  return (aggeps)
}