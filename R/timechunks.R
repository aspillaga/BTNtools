#' Divide time stamps into time chunks
#'
#' This function identifies contiguous chunks of time based on gaps between
#' consecutive time stamps. A new chunk is created whenever the time gap between
#' time stamps exceeds a specified threshold.
#'
#' @param time.stamps A vector of time stamps in `POSIXct` format.
#' @param time.thres A numeric value specifying the time interval used to define
#'    gaps between chunks. If the time difference between consecutive detections
#'    exceeds this threshold, a new chunk is started.
#' @param units A character string specifying the time units for `time.thresh`.
#'    Defaults to `"mins"`.
#'
#' @return A numeric vector assigning each time stamp to a chunk. The sequence
#'    starts at 1 and increments whenever a gap larger than `time.thresh` is
#'    found.
#'
#' @examples
#' date_time <- as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 00:05:00",
#'                           "2023-01-01 00:20:00", "2023-01-01 03:00:00",
#'                           "2023-01-01 03:45:00", "2023-01-01 07:00:00"))
#' timeChunks(date_time, time.thresh = 60, units = "mins")
#'
#'
#' @export
timeChunks <- function(time.stamps, time.thresh = 60, units = "mins") {
  t_diff <- difftime(time.stamps[-1], time.stamps[-length(time.stamps)],
                     units = units)
  chunks <- cumsum(c(1, ifelse(t_diff > time.thresh, 1, 0)))
  return(chunks)
}



#' Generate a regular time sequence based on time chunks
#'
#' This function generates a sequence of regular time intervals for each
#' chunk of time-stamped data, ensuring coverage of the entire chunk. It is
#' useful for aggregating or analyzing data at uniform time intervals.
#'
#' @param time.stamps A vector of time stamps in `POSIXct` format.
#' @param chunks A numeric vector with chunk IDs, as returned by `timeChunks`.
#'    Chunks identified with `NA` will be excluded from the sequence.
#' @param time.int A character string specifying the time interval for
#'    generating the sequence. Defaults to `"5 secs"`.
#'
#' @return A `data.table` containing a sequence of regular time stamps
#'    for each chunk.
#'
#' @examples
#' date_time <- as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 00:05:00",
#'                           "2023-01-01 00:20:00", "2023-01-01 03:00:00",
#'                           "2023-01-01 03:45:00", "2023-01-01 07:00:00"))
#' chunks <- timeChunks(date_time, time.thresh = 60, units = "mins")
#' chunkSeq(date_time, chunks, time.int = "10 mins")
#'
#' @export
chunkSeq <- function(time.stamps, chunks, time.int = "5 secs") {

  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.", call. = FALSE)
  }
  require(data.table)  # Ensure data.table is loaded

  # Convert to data.table and preserve original order
  dt <- data.table:: data.table(date_time = time.stamps,
                                chunks = chunks)[!is.na(chunks)]
  dt <- dt[, .(min = lubridate::ceiling_date(min(date_time), time.int),
               max = lubridate::floor_date(max(date_time), time.int)),
           by = chunks]
  if (any(dt$max < dt$min)) {
    dt$max[dt$max < dt$min] <- dt$min[dt$max < dt$min]
  }

  seq <- rbindlist(lapply(1:nrow(dt), function(i) {
    data.table(chunk = dt$chunks[i],
               times = seq.POSIXt(dt$min[i], dt$max[i], time.int))
  }))

  return(seq)
}

