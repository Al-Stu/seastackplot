#### FUNCTIONS USED IN THE DRAW_PANEL_FUNCTION

#' Transform stats
#'
#' This function allows you to transform the stats into the ggplot graph's
#' coordinate system
#' @param transformed.coords transformed base coordinates
#' @param df.stats the calsulated statistics of the raw data (calculated by
#' dfStats)
#' @param panel_params the parameters of the plot panel
transformStats <- function(transformed.coords, df.stats, panel_params){
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
                  upper.confidence.int = scales::rescale(df.stats$upper.confidence.int, from = panel_params$y.range)
                  )
}

#' Baseline grob
#'
#' This function allows you to create the baseline segmentGrob
#' @param coords the transformed coordinates and stats
baselineGrob <- function(coords){
  grid::segmentsGrob(x0 = coords$x.scaled.0,
                     x1 = coords$x.scaled.0,
                     y0 = coords$min,
                     y1 = coords$max
                     )
}

#' Standard deviation grob
#'
#' This function allows you to create the standard deviation rectGrob
#' @param coords the transformed coordinates and stats
standardDevGrob <- function(coords){
  grid::rectGrob(x = coords$x.scaled.0,
                 y = coords$mean,
                 height = coords$upper.standard.dev - coords$lower.standard.dev,
                 width = coords$x.scaled.1, # base this off of scaled max bin size
                 just = c("left", "centre"),
                 gp=grid::gpar(fill="grey30",
                         col = NA
                         )
                 )
}

#' Confidence interval grob
#'
#' This function allows you to create one of the linesGrob for the confidence
#' interval ticks
#' @param coords the transformed coordinates and stats
#' @param upper.or.lower whether it is the upper or lower bound being plotted
confidenceIntGrob <- function(coords, upper.or.lower){
  y <- ifelse(upper.or.lower == "lower",
              rep_len(coords$lower.confidence.int,2),
              ifelse(upper.or.lower == "upper",
                     rep_len(coords$upper.confidence.int,2),
                     warning("incorrect valuw given for upper.or.lower, please replace"))
              )

  grid::linesGrob(x = c(coords$x.scaled.0, coords$x.scaled.1),
                  y = y,
                  gp = grid::gpar(col = "red", lwd = 2.5)
                  )
}

#' Mean grob
#'
#' This function allows you to create the mean pointsGrob
#' @param coords the transformed coordinates and stats
#' @param mean.size the size you want the mean point to be
meanGrob <- function(coords, mean.size = 4){
  grid::pointsGrob(y = coords$mean,
                   x = coords$x.scaled.0, # this should be adjustable and equal to half the height of the sd rectangle
                   pch = 23,
                   gp = grid::gpar(size = mean.size)
                   )
}

#' Median grob
#'
#' This function allows you to create the median pointsGrob
#' @param coords the transformed coordinates and stats
#' @param median.size the size you want the mean point to be
medianGrob <- function(coords, median.size = 3.2){
  grid::pointsGrob(y = coords$median,
                   x = coords$x.scaled.0, # this should be adjustable and equal to half the height of the sd rectangle
                   pch = 16,
                   gp = grid::gpar(size = median.size)
                   )
}
