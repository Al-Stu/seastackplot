#### FUNCTIONS USED IN THE DRAW_PANEL_FUNCTION

#' Transform stats
#'
#' This function allows you to transform the stats into the ggplot graph's
#' coordinate system
#' @param transformed.coords transformed base coordinates
#' @param df.stats the calculated statistics of the raw data (calculated by
#' dfStats)
#' @param panel_params the parameters of the plot panel
#' @param vertical whether or not the plot is being plotter vertically (as
#' opposed to horizontally)
transformStats <- function(transformed.coords, df.stats, panel_params, vertical = T){
  if(vertical){
    transformed.coords %>%
      dplyr::mutate(x.scaled.0 = scales::rescale(0, from = panel_params$x.range),
                    x.scaled.1 = scales::rescale(1, from = panel_params$x.range),
                    min = scales::rescale(df.stats$min, from = panel_params$y.range),
                    max = scales::rescale(df.stats$max, from = panel_params$y.range),
                    mean = scales::rescale(df.stats$mean, from = panel_params$y.range),
                    median = scales::rescale(df.stats$median, from = panel_params$y.range),
                    lower.standard.dev = scales::rescale(df.stats$lower.standard.dev, from = panel_params$y.range),
                    upper.standard.dev = scales::rescale(df.stats$upper.standard.dev, from = panel_params$y.range),
                    lower.confidence.int = scales::rescale(df.stats$lower.confidence.int, from = panel_params$y.range),
                    upper.confidence.int = scales::rescale(df.stats$upper.confidence.int, from = panel_params$y.range),
                    lower.quant = scales::rescale(df.stats$lower.quant, from = panel_params$y.range),
                    upper.quant = scales::rescale(df.stats$upper.quant, from = panel_params$y.range),
                    ci.line.length = scales::rescale(df.stats$ci.line.length, from = panel_params$x.range),
                    quant.line.length = scales::rescale(df.stats$quant.line.length, from = panel_params$x.range)
      )
  } else{
    transformed.coords %>%
      dplyr::mutate(y.scaled.0 = scales::rescale(0, from = panel_params$y.range),
                    y.scaled.1 = scales::rescale(1, from = panel_params$y.range),
                    min = scales::rescale(df.stats$min, from = panel_params$x.range),
                    max = scales::rescale(df.stats$max, from = panel_params$x.range),
                    mean = scales::rescale(df.stats$mean, from = panel_params$x.range),
                    median = scales::rescale(df.stats$median, from = panel_params$x.range),
                    lower.standard.dev = scales::rescale(df.stats$lower.standard.dev, from = panel_params$x.range),
                    upper.standard.dev = scales::rescale(df.stats$upper.standard.dev, from = panel_params$x.range),
                    lower.confidence.int = scales::rescale(df.stats$lower.confidence.int, from = panel_params$x.range),
                    upper.confidence.int = scales::rescale(df.stats$upper.confidence.int, from = panel_params$x.range),
                    lower.quant = scales::rescale(df.stats$lower.quant, from = panel_params$x.range),
                    upper.quant = scales::rescale(df.stats$upper.quant, from = panel_params$x.range),
                    ci.line.length = scales::rescale(df.stats$ci.line.length, from = panel_params$y.range),
                    quant.line.length = scales::rescale(df.stats$quant.line.length, from = panel_params$y.range)
                    )
  }
}

#' Baseline grob
#'
#' This function allows you to create the baseline segmentGrob
#' @param coords the transformed coordinates and stats
#' @param vertical whether or not the plot is being plotter vertically (as
#' opposed to horizontally)
baselineGrob <- function(coords, vertical = T){
  if(vertical){
    grid::segmentsGrob(x0 = coords$x.scaled.0,
                       x1 = coords$x.scaled.0,
                       y0 = coords$min,
                       y1 = coords$max
    )
  } else{
    grid::segmentsGrob(x0 = coords$min,
                       x1 = coords$max,
                       y0 = coords$y.scaled.0,
                       y1 = coords$y.scaled.0)
  }
}

#' Standard deviation grob
#'
#' This function allows you to create the standard deviation rectGrob
#' @param coords the transformed coordinates and stats
#' @param vertical whether or not the plot is being plotter vertically (as
#' opposed to horizontally)
standardDevGrob <- function(coords, vertical = T){
  gp <- grid::gpar(fill="grey50", col = NA) # set aesthetics so they can be used in both

  if(vertical){
    grid::rectGrob(x = coords$x.scaled.0,
                   y = coords$mean,
                   height = coords$upper.standard.dev - coords$lower.standard.dev,
                   width = coords$x.scaled.1, # base this off of scaled max bin size
                   just = c("left", "centre"),
                   gp = gp
    )
  } else{
    grid::rectGrob(x = coords$mean,
                   y = coords$y.scaled.0,
                   width = coords$upper.standard.dev - coords$lower.standard.dev,
                   height = coords$y.scaled.1, # base this off of scaled max bin size
                   just = c("centre", "top"),
                   gp = gp
                   )
  }
}

#' Pick which error coordinate to plot
#'
#' @param coords the transformed coordinates and stats
#' @param conf.or.quant whether the confidence interval or quantiles are being
#' plotted
#' @param upper.or.lower whether it is the upper or lower bound being plotted

errorCoordinate <- function(coords, conf.or.quant, upper.or.lower){
  ifelse(conf.or.quant == "conf",
         ifelse(upper.or.lower == "upper",
                coords$upper.confidence.int,
                coords$lower.confidence.int
                ),
         ifelse(upper.or.lower == "upper",
                coords$upper.quant,
                coords$lower.quant
                )
         )
}

#' Error ticks grob
#'
#' This function allows you to create one of the linesGrob for the confidence
#' interval or quantile ticks
#' @param coords the transformed coordinates and stats
#' @param conf.or.quant whether the confidence interval or quantiles are being
#' plotted
#' @param upper.or.lower whether it is the upper or lower bound being plotted
#' @param vertical whether or not the plot is being plotter vertically (as
#' opposed to horizontally)
errorTicksGrob <- function(coords, conf.or.quant,
                           upper.or.lower, vertical = T,
                           length = 1
                           ){

  to.plot <- rep_len(errorCoordinate(coords = coords,
                                     conf.or.quant = conf.or.quant,
                                     upper.or.lower = upper.or.lower
                                     ),
                     2
                     )

  gp <- grid::gpar(col = ifelse(conf.or.quant == "conf",
                                "red",
                                "black"
                                ),
                   lwd = 2.5
                   ) # set aesthetics so they can be used in both

  if(vertical){
    grid::linesGrob(x = c(coords$x.scaled.0,
                          ifelse(length ==1,
                                 coords$x.scaled.1,
                                 coords$x.scaled.0 + (coords$x.scaled.1-coords$x.scaled.0)*length
                                 )
                          ),
                    y = to.plot,
                    gp = gp
                    )
  } else{
    grid::linesGrob(x = to.plot,
                    y = c(coords$y.scaled.0,
                          ifelse(length ==1,
                                 coords$y.scaled.1,
                                 coords$y.scaled.1*length
                                 )
                          ),
                    gp = gp
                    )
  }
}

#' Mean grob
#'
#' This function allows you to create the mean pointsGrob
#' @param coords the transformed coordinates and stats
#' @param mean.size the size you want the mean point to be
#' @param vertical whether or not the plot is being plotter vertically (as
#' opposed to horizontally)
meanGrob <- function(coords, mean.size = 4, vertical = T){
  if(vertical){
    grid::pointsGrob(x = coords$x.scaled.0,
                     y = coords$mean, # this should be adjustable and equal to half the height of the sd rectangle
                     pch = 23,
                     size = unit(mean.size, "mm")
                     )
  } else{
    grid::pointsGrob(x = coords$mean,
                     y = coords$y.scaled.0, # this should be adjustable and equal to half the height of the sd rectangle
                     pch = 23,
                     size = unit(mean.size, "mm")
                     )
  }
}

#' Median grob
#'
#' This function allows you to create the median pointsGrob
#' @param coords the transformed coordinates and stats
#' @param median.size the size you want the mean point to be
#' @param vertical whether or not the plot is being plotter vertically (as
#' opposed to horizontally)
medianGrob <- function(coords, median.size = 3.2, vertical = T){
  if(vertical){
    grid::pointsGrob(x = coords$x.scaled.0,
                     y = coords$median, # this should be adjustable and equal to half the height of the sd rectangle
                     pch = 16,
                     size = unit(median.size, "mm")
                     )
  } else{
    grid::pointsGrob(x = coords$median,
                     y = coords$y.scaled.0, # this should be adjustable and equal to half the height of the sd rectangle
                     pch = 16,
                     size = unit(median.size, "mm")
                     )
  }
}
