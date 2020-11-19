#' Create Venn Diagramm for the set properties of all five dictionaries from the
#' source ontologies
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quintuple.venn
#' @examples
#' ggplot2::ggsave("venn5.png", plot = drawVenn5(), width=240, height=160, 
#'   units = "mm", dpi = 300)
drawVenn5 <- function () {
  
  cols <-
    c("EpSO" = "#7800FE",#red
      "ESSO" = "#00FFD6",#cyan
      "EPILONT" = "#FFA501",#gold
      "EPISEM" = "#E80908",#blue
      "FENICS" = "#008000")
  
  dv <- VennDiagram::draw.quintuple.venn(
    area1 = 1357,
    area2 = 2694,
    area3 = 137,
    area4 = 1591,
    area5 = 141,
    n12 = 138,
    n13 = 21,
    n14 = 39,
    n15 = 0,
    
    n23 = 48,
    n24 = 375,
    n25 = 0,
    
    n34 = 21,
    n35 = 0,
    n45 = 0,
    
    n123 = 11,
    n124 = 24,
    n125 = 0,
    
    n134 = 4,
    n135 = 0,
    
    n145 = 0,
    
    n234 = 21,
    n235 = 0,
    
    n245 = 0,
    
    n345 = 0,
    
    n1234 = 4,
    
    n1235 = 0,
    
    n1245 = 0,
    
    n1345 = 0,
    
    n2345 = 0,
    
    n12345 = 0,
    
    category = c("Epso", "ESSO", "EPILONT", "EPISEM", "FENICS"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "plain",
    rotation.degree = 66
    
  )
  return (dv)
}

#' Create Venn Diagramm for the set properties of four dictionaries from the
#' source ontologies
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quad.venn
#' @examples
#' ggplot2::ggsave("venn4.png", plot = drawVenn4(), width=240, height=160, 
#'   units = "mm", dpi = 300)
drawVenn4 <- function () {
  
  cols <-
    c("EpSO" = "#7800FE",#red
      "ESSO" = "#00FFD6",#cyan
      "EPILONT" = "#FFA501",#gold
      "EPISEM" = "#E80908")
  
  dv <- VennDiagram::draw.quad.venn(
    area1 = 1357,
    area2 = 2694,
    area3 = 137,
    area4 = 1591,
    n12 = 138,
    n13 = 21,
    n14 = 39,
    
    n23 = 48,
    n24 = 375,
    
    n34 = 21,
    
    n123 = 11,
    n124 = 24,

    
    n134 = 4,


    
    n234 = 21,

    
    n1234 = 4,
    
    category = c("Epso", "ESSO", "EPILONT", "EPISEM"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "plain",
    rotation.degree = 0
    
  )
  return (dv)
}