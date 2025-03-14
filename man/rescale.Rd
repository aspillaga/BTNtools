% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescale.R
\name{rescale}
\alias{rescale}
\title{Rescale Variables to a New Range}
\usage{
rescale(values, new.range, orig.range = NULL)
}
\arguments{
\item{values}{A numeric vector of values to be rescaled.}

\item{new.range}{A numeric vector of length 2 specifying the minimum and
maximum values of the new range.}

\item{orig.range}{(Optional) A numeric vector of length 2 specifying the
original range of the values. If \code{NULL} (default), the function uses the
range of \code{values} (i.e., \code{range(values)}).}
}
\value{
A numeric vector of the same length as \code{values}, with the values
rescaled to the specified range (\code{new.range}).
}
\description{
This function rescales a numeric vector to a specified new range. The
rescaling is based on either the range of the input values or a user-provided
original range. It applies a linear transformation to scale the values
proportionally to the desired range.
}
\examples{
# Rescale values to a range of [0, 1]
values <- c(10, 20, 30, 40, 50)
rescaled_values <- rescale(values, new.range = c(0, 1))
print(rescaled_values)

# Rescale values to a custom range, using a provided original range
custom_range <- c(5, 50)
rescaled_range <- rescale(values, new.range = c(-1, 1),
                          orig.range = custom_range)
print(rescaled_range)

}
