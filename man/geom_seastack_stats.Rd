% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_seastack.R
\name{geom_seastack_stats}
\alias{geom_seastack_stats}
\title{geom_sea_stack}
\usage{
geom_seastack_stats(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  bin.width = 1,
  orientation = "vertical",
  mean.size = 4,
  median.size = 3.2,
  show.standard.dev = T,
  show.confidence.int = F,
  show.mean = T,
  show.median = F,
  ...
)
}
\arguments{
\item{bin.width}{the bin width being used to plot}

\item{orientation}{"vertical" or "horizontal" whether or not the plot is being
plotted vertically (default) or horizontally}

\item{mean.size}{the size of the mean diamond}

\item{median.size}{the size of the median point}

\item{show.standard.dev}{whether the standard deviation rectangle should be
plotted}

\item{show.confidence.int}{whether the confidence interval should be plotted}

\item{show.mean}{whether the mean should be plotted}

\item{show.median}{whether the median should be plotted}
}
\description{
add seastack statistics to your vertical histogram! uses standard ggplot
parameters on top of those described
}
