#### SUPPORTING FUNCTIONS USED WITHIN SEASTACK PLOTS

## AUTHOR: Alice Stuart
## DATE STARTED: 07/09/2023
## DATE MODIFIED: 07/09/2023
## (based on code by Maja Ilic)

## stats functions ----
#' Standard error of the mean
#'
#' This function allows you to calculate the standard error of the mean (sem)
#' @param value the data you want to calculate the sem of
#' @param na.rm removes any NAs from value
sem <- function(value, na.rm = F) {

  if(na.rm == TRUE){
    value <- value[!is.na(value)]
  }

  sd(value)/sqrt(length(value))
}

#' Calculate stats of the dataset
#'
#' This function allows you to calculate all of the stats required for seastack
#' plots
#' @param value the data you want to calculate the stats of
#' @param bin.width the bin width that will be used to plot the data
dfStats <- function(value, bin.width){
  bin.bounds <- binBounds(value, bin.width)

  df.stats <- data.frame(mean = mean(value),
                         standard.dev = sd(value),
                         median = median(value),
                         SEM = sem(value, na.rm = T),
                         confidence.int = 1.96*sem(value, na.rm = T),
                         min = bin.bounds$min,
                         max = bin.bounds$max
                         ) %>%
    dplyr::mutate(lower.standard.dev = mean - standard.dev,
                  upper.standard.dev = mean + standard.dev,
                  lower.confidence.int = mean - confidence.int,
                  upper.confidence.int = mean + confidence.int
                  )
  df.stats
}

## binning functions ----
#' Calculate bin bounds
#'
#' This function allows you to calculate the upper and lower bounds of the bins
#' that include data (different to the maximum and minimum values)
#' @param value the data you want to calculate the stats of
#' @param bin.width the bin width that will be used to plot the data
binBounds <- function(value, bin.width){
  ## Compute the closest whole number bin interval smaller than the minimum value of value
  min <- floor(min(value, na.rm = T)/bin.width) * bin.width
  ## Compute the closest whole number bin interval larger than the maximum value of value
  max <- ceiling(max(value, na.rm = T)/bin.width) * bin.width

  ## return a data frame
  data.frame(min = min, max = max)
}

#' Calculate bin breaks
#'
#' This function allows you to calculate the breaks between bins
#' @param value the data you want to calculate the stats of
#' @param bin.width the bin width that will be used to plot the data
histBreaks <- function(value, bin.width){
  ## Compute the bin bounds
  bin.bounds <- binBounds(value, bin.width)

  ## sequence between them by the bin width (this is what's returned)
  seq(from = bin.bounds$min,
      to = bin.bounds$max,
      by = bin.width)
}
