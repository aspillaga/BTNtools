#' Extract Milliseconds from Time Stamps
#'
#' This function extracts the millisecond component from detection time stamps
#' in `POSIXct` format. It calculates the fraction of the second that the
#' detection time represents, providing the millisecond component as a value
#' between 0 and 1.
#'
#' @param time.stamps A vector of detection time stamps in `POSIXct` format.
#'
#' @return A numeric vector of milliseconds (0 to 1 range).
#' @return A numeric vector of millisecond values, with each value representing
#'    the fractional part of the second in the corresponding `time.stamps`.
#'
#' @examples
#' # Example of extracting milliseconds from time stamps
#' time_stamps <- as.POSIXct(c("2025-02-26 12:00:00.123",
#'                             "2025-02-26 12:00:01.456"))
#' ms <- milliSec(time_stamps)
#' print(ms)  # Expected output: 0.123, 0.456
#'
#' @export
#'
#'
milliSec <- function(time.stamps) {
  requireNamespace("lubridate", quietly = TRUE)
  ms <- as.numeric(time.stamps - lubridate::floor_date(time.stamps, "seconds"))
  return(ms)
}
