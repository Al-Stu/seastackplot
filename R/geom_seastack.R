#' @importFrom utils globalVariables
globalVariables(c("layer",  "GeomFlatViolin"))

# this roxygen description is just copied from raincloudplots
#' ggplot2-ggproto
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSeastackStats <- ggplot2::ggproto("GeomSeastackStats", ggplot2::Geom,
                             required_aes = c("y", "bin.width", "mean.size", "median.size", "show.standard.dev", "show.confidence.int", "show.mean", "show.median"),
                             draw_key = ggplot2::draw_key_point,
                             draw_panel = draw_panel_function
                             )

#' geom_sea_stack
#'
#' add seastack statistics to your vertical histogram! uses standard ggplot
#' parameters on top of those described
#' @param bin.width the bin width being used to plot
#' @param mean.size the size of the mean diamond
#' @param median.size the size of the median point
#' @param show.standard.dev whether the standard deviation rectangle should be
#' plotted
#' @param show.confidence.int whether the confidence interval should be plotted
#' @param show.mean whether the mean should be plotted
#' @param show.median whether the median should be plotted
#'
#' @import ggplot2
#' @export
geom_seastack_stats <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, bin.width = 1,
                                mean.size = 4, median.size = 3.2,
                                show.standard.dev = T, show.confidence.int = F,
                                show.mean = T, show.median = F, ...) {
  layer(
    geom = GeomSeastackStats, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  bin.width = bin.width,
                  show.standard.dev = show.standard.dev,
                  show.confidence.int = show.confidence.int,
                  show.mean = show.mean,
                  show.median = show.median,
                  mean.size = mean.size,
                  median.size = median.size, ...)
    )
}
