#' Unwrap Time Drift in Acoustic Telemetry Data
#'
#' This function reconstructs the continuous time drift pattern from a vector
#' of millisecond values that range between 0 and 1. Since the observed drift
#' is cyclical (0-1 range), the function unwraps it by correcting for
#' discontinuities.The correction accounts for the signal emission granularity
#' of the transmitter, which refers to the precision of the signal emission
#' intervals. The function estimates the mean slope of the drift using time
#' differences between consecutive detections. It only considers pairs of
#' detections with small drift changes (less than half the granularity) and
#' those occurring within one hour of each other.
#'
#' @param ms A numeric vector of millisecond values, ranging from 0 to 1.
#' @param time.stamps A `POSIXct` vector of detection time stamps.
#' @param granularity A numeric value specifying the signal emission
#'    granularity. Defaults to `1` for Thelma Biotel transmitters. Use `0.5` for
#'    Lotek transmitters.
#'
#' @return A numeric vector representing the unwrapped, continuous time drift.
#'
#' @export
#'
#'
unwrapDrift <- function(ms, time.stamps, granularity = 1) {

  # Create a vector to store the drift
  n <- length(ms)
  drift <- numeric(n)
  drift[1] <- ms[1]

  # Calculate differences of drift and time between consecutive detections
  diff_ms <- diff(ms)
  diff_t <- as.numeric(difftime(time.stamps[-1], time.stamps[-n],
                                units = "secs"))

  # Calculate the mean slope of the time drift, but only taking into account
  # consecutive detections with small drifts (smaller than half the
  # granularity). Also, avoid calculating slopes with detections separated by
  # more than 1 h.
  indx <- which(abs(diff_ms) < granularity/2 & diff_t < 3600)
  mean_slope <- mean(diff_ms[indx] / diff_t[indx], na.rm = TRUE, trim = 0.25)

  # Correct each the drift of each detection based on the mean slope
  for (i in 2:n) {

    # Expected drift from the previous detection based on the mean slope
    expected_drift <- drift[i - 1] + diff_t[i - 1] * mean_slope

    # Calculate the correction factor (n times the granularity)
    c <- round((expected_drift - ms[i])/granularity)

    # Correct time drift
    drift[i] <- ms[i] + c * granularity
  }

  # Set the first drift value to 0 and return drift pattern
  drift <- drift - drift[1]
  return(drift)
}
