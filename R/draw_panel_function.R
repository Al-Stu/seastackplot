#' draw_panel_function
#'
#' @param data the data being plotted
#' @param panel_params the parameters of the plot panel
#' @param coord the coordinates of the plot (I don't fully understand this)
#' @param bin.width the bin width being used to plot
#' @param mean.size the size of the mean diamond
#' @param median.size the size of the median point
#' @param show.standard.dev whether the standard deviation rectangle should be
#' plotted
#' @param show.confidence.int whether the confidence interval should be plotted
#' @param show.mean whether the mean should be plotted
#' @param show.median whether the median should be plotted
#' @param orientation "vertical" or "horizontal" whether or not the plot is being
#' plotted vertically (default) or horizontally
#'
#' @import ggplot2
draw_panel_function <- function(data, panel_params, coord,
                                bin.width, mean.size, median.size,
                                show.standard.dev, show.confidence.int,
                                show.mean, show.median,
                                orientation = "vertical") {
  ## compute the basic stats of the data
  if(orientation == "vertical"){
    df.stats <- dfStats(value = data$y, bin.width = bin.width) # for now I am just setting bin.width to 0.5
    vertical <- T
  } else if(orientation == "horizontal"){
    df.stats <- dfStats(value = data$x, bin.width = bin.width) # for now I am just setting bin.width to 0.5
    vertical <- F
  } else{
    warning('unrecognised value for orientation, please use either \"horizontal\" or \"vertical\"')
  }

  ## rescale the coordinated and stats so they follow the ggplot coordinate system
  coords <- coord$transform(data, panel_params) %>%
    transformStats(df.stats = df.stats, panel_params = panel_params, vertical = vertical)

  ## draw the base line
  base.line <- baselineGrob(coords = coords, vertical = vertical)

  ## draw the standard deviation rectangle
  # (so long as show.standard.dev = T, else plot a null grob)
  if(show.standard.dev){
    standard.dev.rectangle <- standardDevGrob(coords = coords, vertical = vertical)
  } else{
    standard.dev.rectangle <- grid::nullGrob()
  }

  ## draw two lines, each one confidence interval away from the mean
  # (so long as show.confidence.int = T, else plot a null grob)
  if(show.confidence.int){
    lower.ci.line <- confidenceIntGrob(coords = coords,
                                       upper.or.lower = "lower",
                                       vertical = vertical)
    upper.ci.line <- confidenceIntGrob(coords = coords,
                                       upper.or.lower = "upper",
                                       vertical = vertical)
  } else{
    lower.ci.line <- grid::nullGrob()
    upper.ci.line <- grid::nullGrob()
  }

  ## draw a diamond for the mean
  # (so long as show.mean = T, else plot a null grob)
  if(show.mean){
    mean.diamond <- meanGrob(coords = coords,
                             mean.size = mean.size,
                             vertical = vertical
                             )
  } else{
    mean.diamond <- grid::nullGrob()
  }

  ## draw dot for median
  # (so long as show.median = T, else plot a null grob)
  if(show.median){
    median.dot <- medianGrob(coords = coords,
                             median.size = median.size,
                             vertical = vertical)
  } else{
    median.dot <- grid::nullGrob()
  }

  ## return all the grobs to be drawn
  grid::gTree(children = grid::gList(base.line,
                               standard.dev.rectangle,
                               lower.ci.line,
                               upper.ci.line,
                               mean.diamond,
                               median.dot
                               )
              )

}
