% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timechunks.R
\name{chunkSeq}
\alias{chunkSeq}
\title{Generate a regular time sequence based on time chunks}
\usage{
chunkSeq(time.stamps, chunks, time.int = "5 secs")
}
\arguments{
\item{time.stamps}{A vector of time stamps in \code{POSIXct} format.}

\item{chunks}{A numeric vector with chunk IDs, as returned by \code{timeChunks}.
Chunks identified with \code{NA} will be excluded from the sequence.}

\item{time.int}{A character string specifying the time interval for
generating the sequence. Defaults to \code{"5 secs"}.}
}
\value{
A \code{data.table} containing a sequence of regular time stamps
for each chunk.
}
\description{
This function generates a sequence of regular time intervals for each
chunk of time-stamped data, ensuring coverage of the entire chunk. It is
useful for aggregating or analyzing data at uniform time intervals.
}
\examples{
date_time <- as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 00:05:00",
                          "2023-01-01 00:20:00", "2023-01-01 03:00:00",
                          "2023-01-01 03:45:00", "2023-01-01 07:00:00"))
chunks <- timeChunks(date_time, time.thresh = 60, units = "mins")
chunkSeq(date_time, chunks, time.int = "10 mins")

}
