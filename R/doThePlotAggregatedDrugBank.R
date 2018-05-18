aggregatedPlotDrugBank <- function (cosinedrugbank, dicedrugbank, jaccarddrugbank) {
  # MeSH: red     #ff0000
  # EpSO: blue        #0000ff
  # ESSO: orange      #FFA500
  # EPILONT: purple   #800080
  cols <- c("MeSH" = "#ff0000", "EpSO"="#0000FF","ESSO"="#FFA500","EPILONT"="#800080")
  
  myalpha = shQuote("0.9")
  mymeansize = 2
  
  mesh_metrics <- create_metrics (cosinedrugbank$MeSH, dicedrugbank$MeSH, jaccarddrugbank$MeSH, jaccarddrugbank$Elements)
  mesh_stats <- create_stats(mesh_metrics)
  mesh_error <- create_error(mesh_stats)
  
  epso_metrics <- create_metrics (cosinedrugbank$EpSO, dicedrugbank$EpSO, jaccarddrugbank$EpSO, jaccarddrugbank$Elements)
  epso_stats <- create_stats(epso_metrics)
  epso_error <- create_error(epso_stats)
  
  esso_metrics <- create_metrics (cosinedrugbank$ESSO, dicedrugbank$ESSO, jaccarddrugbank$ESSO, jaccarddrugbank$Elements)
  esso_stats <- create_stats(esso_metrics)
  esso_error <- create_error(esso_stats)
  
  epi_metrics <- create_metrics (cosinedrugbank$EPILONT, dicedrugbank$EPILONT, jaccarddrugbank$EPILONT, jaccarddrugbank$Elements)
  epi_stats <- create_stats(epi_metrics)
  epi_error <- create_error(epi_stats)
  
  epi_stats[epi_stats < 0] <- 0
  
  ggplot2::ggplot(
    data = mesh_stats, 
    ggplot2::aes_string(x="elements", y="mean", colour = shQuote("MeSH"))
  ) + 
    ggplot2::geom_step(size = mymeansize) +
    ggplot2::geom_ribbon(
      data = mesh_stats, 
      ggplot2::aes_string(x = "elements", ymin = "mean - sd", ymax = "mean + sd", colour = shQuote("MeSH"), fill = shQuote("red"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::geom_step(
      data = epso_stats, 
      ggplot2::aes_string(x="elements", y="mean", colour = shQuote("EpSO")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = epso_stats, 
      ggplot2::aes_string(x = "elements", ymin = "mean - sd", ymax = "mean + sd", colour = shQuote("EpSO"), fill = shQuote("blue"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::geom_step(
      data = esso_stats, 
      ggplot2::aes_string(x="elements", y="mean", colour = shQuote("ESSO")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = esso_stats, 
      ggplot2::aes_string(x = "elements", ymin = "mean - sd", ymax = "mean + sd", colour = shQuote("ESSO"), fill = shQuote("orange"), alpha = myalpha), show.legend = FALSE
    ) + 
    ggplot2::geom_step(
      data = epi_stats, 
      ggplot2::aes_string(x="elements", y="mean", colour = shQuote("EPILONT")), size = mymeansize
    ) + 
    ggplot2::geom_ribbon(
      data = epi_stats, 
      ggplot2::aes_string(x="elements", ymin = "mean -sd", ymax = "mean + sd", colour = shQuote("EPILONT"), fill = shQuote("purple"), alpha = myalpha), show.legend = FALSE
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
    ggplot2::labs (y="Mean", x="TopK", title = "Mean of Cosine, Dice, and Jaccard with Standard Deviation against DrugBank", subtitle = "") +
    ggplot2::scale_colour_manual(values=cols)+
    ggplot2::scale_size_manual() 
}