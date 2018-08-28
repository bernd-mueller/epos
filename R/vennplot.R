#' Draw venn diagram with example values
#'
#' @return venn diagram
#' @export
#'
#' @examples
#' \dontrun{
#' drawVennPlot()
#' }
drawVennPlot <- function()  {
  venn <- VennDiagram::draw.triple.venn(
    area1 = 39,
    area2 = 39,
    area3 = 39,
    n12 = 36,
    n23 = 31,
    n13 = 30,
    n123 = 29,
    category = c("EpSO", "ESSO", "EPILONT"),
    fill = c("blue", "orange", "green"),
    lty = "blank",
    cex = 2,
    cat.cex = 2,
    cat.col = c("blue", "orange", "green")
  );
  return (venn)
}