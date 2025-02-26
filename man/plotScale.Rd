% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps_plotScale.R
\name{plotScale}
\alias{plotScale}
\title{Plot scale bar on a map}
\usage{
plotScale(
  length = 100,
  units = "m",
  col = "gray50",
  side = "left",
  inset = c(0.015, 0.025),
  whisk.len = 0.02,
  lwd = 1.2
)
}
\arguments{
\item{length}{A numeric value specifying the length of the scale bar in the
units defined by \code{units}. Defaults to 100.}

\item{units}{A character string specifying the units for the scale bar.
Either "m" (meters) or "km" (kilometers). Defaults to "m".}

\item{col}{A character string specifying the color of the scale bar and text.
Defaults to "gray50".}

\item{side}{A character string indicating the side where the scale bar will
be drawn. Either "left" or "right". Defaults to "left".}

\item{inset}{A numeric vector of length 2 specifying the inset distances for
the margins as fractions of the plot region.}

\item{whisk.len}{A numeric value specifying the length of the whiskers at
the ends of the scale bar.}

\item{lwd}{A numeric value specifying the width of the scale line.}
}
\value{
This function adds a scale bar to the current plot.
}
\description{
This function draws a scale bar on a map made with a UTM projection.
}
