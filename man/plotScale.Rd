% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotScale.R
\name{plotScale}
\alias{plotScale}
\title{Add a Scale Bar to a Map}
\usage{
plotScale(
  length = 100,
  units = "m",
  col = "gray50",
  side = "left",
  lwd = 1.2,
  inset = c(0.015, 0.025),
  whisk.len = 0.02
)
}
\arguments{
\item{length}{A numeric value specifying the length of the scale bar. The
length is in the units defined by the \code{units} parameter.}

\item{units}{A character string specifying the units for the scale bar. It
can be either "m" (meters) or "km" (kilometers). Defaults to "m".}

\item{col}{A character string specifying the color of the scale bar and the
text. Defaults to \code{"gray50"}.}

\item{side}{A character string indicating which side of the plot the scale
bar will be drawn on. Either "left" or "right". Defaults to "left".}

\item{lwd}{A numeric value specifying the width of the scale bar line.}

\item{inset}{A numeric vector of length 2 specifying the inset distances for
the margins as fractions of the plot region. The first value controls the
horizontal position, and the second controls the vertical position.}

\item{whisk.len}{A numeric value specifying the length of the whiskers at the
ends of the scale bar.}
}
\value{
This function adds a scale bar to the current plot. It does not
return any value.
}
\description{
This function adds a scale bar to a map plotted in a UTM projection. The
scale bar is drawn with specified units and dimensions.
}
\examples{
# Example usage
plot(1:500, 1:500, type = "n", asp = 1)
plotScale(length = 100, units = "m", side = "right")
plotScale(length = 0.5, units = "km", side = "left", inset = c(0.05, 0.05))

}
