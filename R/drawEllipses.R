drawEllipses <- function () {
  t <- theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908")
  
  elli <- data.frame (
    y0 = c(0,1,3,3),
    x0 = c(1,2,7,8),
    a = c(4,3.5,3.5,4),
    b = c(2,2,2,2),
    angle = c(-60,-90,60,105),
    m1 = 2,
    fill = c(
      shQuote("ESSO"), 
      shQuote("EPISEM"),
      shQuote("EPILONT"),
      shQuote("EpSO"))
  )
  
  p <- ggplot() +
    geom_ellipse(aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, m1 = m1,
                     fill = fill), elli, alpha = 0.3, linetype =2) +
    ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
    coord_fixed()
  return (p)
}