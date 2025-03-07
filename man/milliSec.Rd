% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/milliSec.R
\name{milliSec}
\alias{milliSec}
\title{Extract Milliseconds from Time Stamps}
\usage{
milliSec(time.stamps)
}
\arguments{
\item{time.stamps}{A vector of detection time stamps in \code{POSIXct} format.}
}
\value{
A numeric vector of milliseconds (0 to 1 range).

A numeric vector of millisecond values, with each value representing
the fractional part of the second in the corresponding \code{time.stamps}.
}
\description{
This function extracts the millisecond component from detection time stamps
in \code{POSIXct} format. It calculates the fraction of the second that the
detection time represents, providing the millisecond component as a value
between 0 and 1.
}
\examples{
# Example of extracting milliseconds from time stamps
time_stamps <- as.POSIXct(c("2025-02-26 12:00:00.123",
                            "2025-02-26 12:00:01.456"))
ms <- milliSec(time_stamps)
print(ms)  # Expected output: 0.123, 0.456

}
