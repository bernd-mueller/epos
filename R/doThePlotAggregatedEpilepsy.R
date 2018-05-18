

# ggplot(data = drug_stats, aes(x = drug_stats$elements, y = drug_stats$mean)) + 
# geom_ribbon(data = drug_stats, 
# aes(x = drug_stats$elements, ymin = drug_stats$mean - drug_stats$sd, ymax=drug_stats$sd + drug_stats$sd), fill = "pink", alpha = 0.5) 

aggregatedPlotEpilepsie <- function (cosineepso, cosineesso, diceepso, diceesso, jaccardesso, jaccardepso) {
  # DrugBank: red     #ff0000
  # EpSO: blue        #0000ff
  # ESSO: orange      #FFA500
  # EPILONT: purple   #800080
  cols <- c("EpSO vs. ESSO"="#0000FF","EpSO vs. EPILONT"="#FFA500","ESSO vs. EPILONT"="#800080")
  
  myalpha = shQuote("0.9")
  mymeansize = 2
  
  epsoesso_metrics <- create_metrics (cosineepso$ESSO, diceepso$ESSO, jaccardepso$ESSO, jaccardepso$Elements)
  epsoesso_stats <- create_stats(epsoesso_metrics)
  epsoesso_error <- create_error(epsoesso_stats)
  
  epsoepilont_metrics <- create_metrics (cosineepso$EPILONT, diceepso$EPILONT, jaccardepso$EPILONT, jaccardepso$Elements)
  epsoepilont_stats <- create_stats(epsoepilont_metrics)
  epsoepilont_error <- create_error(epsoepilont_stats)
  
  essoepilont_metrics <- create_metrics (cosineesso$EPILONT, diceesso$EPILONT, jaccardesso$EPILONT, jaccardesso$Elements)
  essoepilont_stats <- create_stats(essoepilont_metrics)
  essoepilont_error <- create_error(essoepilont_stats)
  
  
  ggplot2::ggplot(
    data = epsoesso_stats, 
    ggplot2::aes_string(x="elements", y="mean", colour = shQuote("EpSO vs. ESSO"))
  ) + 
    ggplot2::geom_step(size = mymeansize) +
    ggplot2::geom_ribbon(
      data = epsoesso_stats, 
      ggplot2::aes_string(x = "elements", ymin = "mean - sd", ymax = "mean + sd", colour = shQuote("EpSO vs. ESSO"), fill = shQuote("EpSO vs. ESSO"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::geom_step(
      data = epsoepilont_stats, 
      ggplot2::aes_string(x="elements", y="mean", colour = shQuote("EpSO vs. EPILONT")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = epsoepilont_stats, 
      ggplot2::aes_string(x = "elements", ymin = "mean - sd", ymax = "mean + sd", colour = shQuote("EpSO vs. EPILONT"), fill = shQuote("EpSO vs. EPILONT"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::geom_step(
      data = essoepilont_stats, 
      ggplot2::aes_string(x="elements", y="mean", colour = shQuote("ESSO vs. EPILONT")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = essoepilont_stats, 
      ggplot2::aes_string(x = "elements", ymin = "mean - sd", ymax = "mean + sd", colour = shQuote("ESSO vs. EPILONT"), fill = shQuote("ESSO vs. EPILONT"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::theme_minimal () +
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
    ggplot2::labs (y="Mean", x="TopK", title = "Mean of Cosine, Dice, and Jaccard with Standard Deviation of EpSO vs. ESSO, EpSO vs. EPILONT, and ESSO vs. EPILONT", subtitle = "") +
    ggplot2::scale_colour_manual(values=cols)+
    ggplot2::scale_size_manual()
}