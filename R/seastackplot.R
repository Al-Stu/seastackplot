#' geom_sea_stack
#'
#' Add seastack statistics to your vertical histogram! Uses standard ggplot
#' parameters on top of those described
#' @param data the data to be plotted
#' @param data.column the name of the column containing the data to be plotted
#' @param group.column the name of the column containing the group names for the data
#' @param data.label the desired title for the data axis, if left NULL (default), will be the value of data.column
#' @param orientation the orientation of the plot to be plotted
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
#' @export
sea_stack_plot <- function(data,
                           data.column = 'value',
                           group.column = 'group',
                           data.label = NULL,
                           bin.width = 1,
                           hist.colours,
                           mean.size = 4,
                           median.size = 3.2,
                           orientation = 'vertical',
                           show.mean = T,
                           show.median = F,
                           show.standard.dev = T,
                           show.confidence.int = F
                           ){
  ## select only the needed columns
  df <- data %>%
    dplyr::select(dplyr::all_of(data.column), dplyr::all_of(group.column))

  ## rename group and data column so they can be referred to later
  if(data.column != 'value'){
    df <- df %>%
      dplyr::rename(value = dplyr::all_of(data.column))
  }

  if(group.column != 'group'){
    df <- df %>%
      dplyr::rename(group = dplyr::all_of(group.column))
  }

  ## set the data label
  if(is.null(data.label)){
    data.label <- ifelse(data.column == 'value',
                         'Value',
                         data.column
                         )
  }

  ## compute the histogram bins
  hist.breaks <- histBreaks(value = df$value, bin.width = bin.width)

  if(orientation == 'vertical'){
    figure <- ggplot(df, aes(y = value)) +
      facet_wrap(~ group, nrow = 1) +
      geom_histogram(breaks = hist.breaks,
                     orientation = 'y',
                     aes(fill = group), # have this here so it doesn't override the default fills of everything else
                     colour = NA # **** IF YOU LEAVE THIS OUT IT'LL PLOT A LINE ALONG THE WHOLE X-AXIS !!!! WHICH WILL OBSCURE THE BASELINE !!!!
                     ) +
      geom_seastack_stats(bin.width = bin.width,
                          mean.size = mean.size,
                          median.size = median.size,
                          show.mean = show.mean,
                          show.median = show.median,
                          show.standard.dev = show.standard.dev,
                          show.confidence.int = show.confidence.int
                          ) +
      scale_x_reverse() +
      theme_seastack() +
      labs(x = "Counts", y = data.label)
  } else if(orientation == 'horizontal'){
    figure <- ggplot(df, aes(x = value)) +
      facet_wrap(~ group, nrow = 1) +
      geom_histogram(breaks = hist.breaks,
                     orientation = 'x',
                     aes(fill = group), # have this here so it doesn't override the default fills of everything else
                     colour = NA # **** IF YOU LEAVE THIS OUT IT'LL PLOT A LINE ALONG THE WHOLE X-AXIS !!!! WHICH WILL OBSCURE THE BASELINE !!!!
      ) +
      geom_horizontal_seastack_stats(bin.width = bin.width,
                          mean.size = mean.size,
                          median.size = median.size,
                          show.mean = show.mean,
                          show.median = show.median,
                          show.standard.dev = show.standard.dev,
                          show.confidence.int = show.confidence.int
      ) +
      theme_seastack() +
      labs(x = data.label, y = "Counts")
  } else{
    warning('unrecognised value for orientation, please use either \"horizontal\" or \"vertical\"')
  }

  figure
}
