% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_year_comp.R
\name{plot_year_comp}
\alias{plot_year_comp}
\title{plot_year_comp}
\usage{
plot_year_comp(
  df,
  type = c("median", "smooth", "smoothmedian")[1],
  xlim = c(0, 365),
  vline_pos = NULL,
  xlab = "Day of Year",
  strip_text_y = NULL,
  annotations = TRUE,
  alpha = 0.2,
  ...
)
}
\arguments{
\item{df}{output of \code{parse_year_comp()}}

\item{type}{one of "median","smooth","smoothmedian"
\itemize{
 \item median - plot the rolling median calculated by \code{parse_year_comp()}
 \item smooth - plot a loess smooth through the daily median values
 \item smoothmedian - plot a loess smooth through the rolling median values
}}

\item{xlim}{x axis limits via \code{ggplot2::coord_cartesian()}}

\item{vline_pos}{numeric vector. add vertical lines at the given day of year}

\item{xlab}{x axis label}

\item{strip_text_y}{override the strip text on the y axis}

\item{annotations}{logical, show plot subtitles? default true}

\item{alpha}{control the alpha of the underlying timeseries}

\item{...}{}
}
\description{
plot the output of \code{parse_year_comp()}
}
\author{
W. S. Drysdale
}
