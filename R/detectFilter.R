#' Identify Valid and False or Spurious Detections in Acoustic Telemetry Data
#'
#' This function identifies valid and false detections in acoustic telemetry
#' data based on a predefined time interval (default: 24 hours). A false
#' detection is defined as an isolated detection occurring within the specified
#' time interval. The function can analyze detections across all receivers or
#' separately for each receiver deployment (if the `rec.id` argument is
#' provided).
#'
#' @param tag.id A vector containing the ID codes of tagged individuals or
#'    transmitters.
#' @param time.stamp A vector of detection time stamps in `POSIXt` format.
#' @param rec.id (Optional) A vector containing the receiver IDs for each
#'    detection. If provided, false detections will be identified within each
#'    receiver separately.
#' @param time.int A numeric value specifying the time interval used to define
#'    false detections.
#' @param units A character string specifying the time units for `time.int`.
#'    Defaults to `"hours"`.
#' @param return.both Logical. If `TRUE`, the function returns a data frame
#'    containing false detection indicators for both the entire receiver
#'    array and individual receivers (if `rec.id` is provided). If
#'    `FALSE` (default), only a single logical vector is returned.
#'
#' @return A logical vector or data frame indicating whether each detection is
#'    valid (`TRUE`) or false (`FALSE`).
#'     - If `rec.id` is not provided, the function returns a single logical
#'       vector corresponding to the entire receiver array.
#'     - If `rec.id` is provided and `return_both = FALSE`, the function returns
#'       only the logical vector for individual receivers.
#'     - If `rec.id` is provided and `return_both = TRUE`, the function returns
#'       a data frame with two logical vectors: one for the entire array and one
#'       for individual receivers.
#'
#' @examples
#' # Generate a random dataset
#' set.seed(900)
#' time_stamp <- Sys.time() + c(1:24, 70:71, 200, 300:320, 500, 701:720) * 3600
#' tag_id <- factor(sample(c("i1", "i2"), length(time_stamp), replace = TRUE))
#' rec_id <- factor(sample(c("r1", "r2"), length(time_stamp), replace = TRUE))
#'
#' # Identify false detections for the entire receiver array
#' f1 <- detectFilter(tag_id, time_stamp)
#' plot(time_stamp, tag_id, pch = (1:2)[rec_id])
#' points(time_stamp[!f1], tag_id[!f1], col = 2, cex = 2)
#'
#' # Identify false detections within individual receivers
#' f2 <- detectFilter(tag_id, time_stamp, rec_id, time.int = 1, units = "days",
#'                    return.both = TRUE)
#' plot(time_stamp, tag_id, pch = (1:2)[rec_id])
#' points(time_stamp[!f2$receiver], tag_id[!f2$receiver], col = 4, cex = 2.4)
#' points(time_stamp[!f2$array], tag_id[!f2$array], col = 2, cex = 2)
#'
#' @importFrom data.table ':=' .N .SD
#'
#' @export
#'
#'
detectFilter <- function(tag.id, time.stamp, rec.id = NULL, time.int = 24,
                         units = "hours", return.both = FALSE) {

  # Load required package
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.", call. = FALSE)
  }
  library(data.table)  # Ensure data.table is loaded

  # Validate inputs
  if (!inherits(time.stamp, "POSIXt")) {
    stop("Time stamps must be in POSIXlt or POSIXct format.", call. = FALSE)
  }
  if (length(tag.id) != length(time.stamp)) {
    stop("'tag.id' and 'time.stamp' must have the same length.", call. = FALSE)
  }
  if (!is.null(rec.id) && length(rec.id) != length(tag.id)) {
    stop("'rec.id' must have the same length as 'tag.id'.", call. = FALSE)
  }

  # Convert to data.table and preserve original order
  dt <- data.table::data.table(indx = seq_along(tag.id), tag.id, time.stamp,
                               rec.id)

  # Identify false detections for the entire receiver array
  dt[, array := filtSubset(time.stamp, time.int, units), by = tag.id]

  # Identify false detections within individual receivers (if rec.id provided)
  if (!is.null(rec.id)) {
    dt[, receiver := filtSubset(time.stamp, time.int, units),
       by = .(tag.id, rec.id)]
  }

  # Restore original order
  data.table::setorder(dt, indx)

  # Return output based on 'return_both' argument
  if (!is.null(rec.id)) {
    if (return.both) {
      return(dt[, .(array, receiver)])
    } else {
      return(dt$receiver)
    }
  } else {
    return(dt$array)
  }
}


# HELPER FUNCTIONS #############################################################

#' Identify Valid Detections in a Subset of Data
#'
#' This helper function identifies valid detections in a subset of acoustic
#' telemetry data. A valid detection is one that is not isolated beyond a given
#' time interval (`time.int`) from its neighbors. This function is used within
#' `detectFilter()`.
#'
#' @param time.stamp A vector of timestamps (`POSIXt` format) representing the
#'    detections of a single transmitter or subset for an individual receiver.
#' @param time.int A numeric value specifying the time interval threshold.
#'    Defaults to 24.
#' @param units A character string specifying the time units for `time.int`.
#'    Defaults to `"hours"`.
#'
#' @return A logical vector of the same length as `time.stamp`, where `TRUE`
#'    indicates a valid detection and `FALSE` indicates an isolated (potentially
#'    false) detection.
#'
#'
filtSubset <- function(time.stamp, time.int = 24, units = "hours") {

  # Sort time stamps and get order index
  order_indx <- order(time.stamp)
  time_order <- time.stamp[order_indx]

  # Time intervals between ordered detections
  diff <- as.numeric(difftime(time_order[-1], time_order[-length(time_order)],
                                     units = units))

  # Identify false detections (isolated detections outside time.int)
  filt <- !(c(Inf, diff) > time.int & c(diff, Inf) > time.int)

  # Return to original order
  result <- logical(length(time.stamp))
  result[order_indx] <- filt

  return(result)

}

