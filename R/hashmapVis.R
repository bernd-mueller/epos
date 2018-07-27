#' Creates a plot object for a given sorted hashmap.
#'
#' @param shm hashmap that is sorted and about to be visualized
#' @param plottitle character vector that is the title of the plot
#'
#' @return plotbar a ggplot2 object containing the plot
#' @export
#'
#' @examples
#' \dontrun{
#' createATCBarChart(flmesh, "These are the most frequent ATC classes for the MeSH vector")
#' }
createATCBarChart <- function (shm, plottitle) {
  counter <- 0
  sum <- 0
  for (i in 1:length(shm)) {
    atc <- shm[i]
    
    atcname <- names(atc)
    atccount <- levels(factor(atc))
    
    if (counter == 0) {
      labels <- atcname
      counts <- atccount
    } else if (counter < 14) {
      labels <- c(labels, atcname)
      counts <- c(counts, atccount)
    } else {
      sum <- sum + 1
    }
    counter <- counter + 1
  }
  #labels <- c(labels, "Other")
  #counts <- c(as.integer(counts), sum)
  counts <- c(as.integer(counts))

  dpie <- data.frame(ATC=labels, Count = counts)
  levels(dpie$ATC) <- gsub("([A-Z]+ [A-Z]+ )([A-Z]+ )", "\\1   \n\\2", levels(dpie$ATC))
  dpie$ATC <- factor(dpie$ATC, levels = dpie$ATC)

  plotbar <- ggplot2::ggplot(dpie, ggplot2::aes_string(x = "ATC", y = "Count")) + 
    ggplot2::theme_minimal ()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray"), 
                   panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
                   legend.text=ggplot2::element_text(size=11),
                   legend.position=c(0,1), 
                   legend.justification=c(0, 0),
                   legend.direction="horizontal",
                   legend.title = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 9)) + 
    ggplot2::geom_bar(stat="identity", width=.5, fill="tomato3") + 
    ggplot2::labs(title="",  subtitle=plottitle, caption="") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=65, vjust=0.6))
  
  return(plotbar)
}

