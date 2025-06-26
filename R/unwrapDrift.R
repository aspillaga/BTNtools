#' Unwrap Time Drift in Acoustic Telemetry Data
#'
#' This function reconstructs the continuous time drift pattern from a vector
#' of millisecond values that range between 0 and 1. Since the observed drift
#' is cyclical (0-1 range), the function unwraps it by correcting for
#' discontinuities.The correction accounts for the signal emission granularity
#' of the transmitter, which refers to the precision of the signal emission
#' intervals.
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

  # Create a vector to store the unwrapped drift values
  n <- length(ms)
  drift <- numeric(n)
  drift[1] <- ms[1]

  # Compute differences in drift and time between consecutive detections
  diff_ms <- diff(ms)
  diff_t <- as.numeric(difftime(time.stamps[-1], time.stamps[-n],
                                units = "secs"))

  # Identify valid detection pairs for slope estimation (consecutive detections
  # with small drifts - smaller than half the granularity - and detections
  # separated by less than 1h).
  indx <- which(abs(diff_ms) < granularity/2 & diff_ms != 0 & diff_t < 3600)
  dens <- density(diff_ms[indx] / diff_t[indx])
  slope <- dens$x[which.max(dens$y)]

  # Reconstruct the continuous drift by correcting wrapped values
  for (i in 2:n) {

    # Predict expected drift based on previous drift and estimated slope
    expected_drift <- drift[i - 1] + diff_t[i - 1] * slope

    # Compute the correction factor: number of granularity units to shift
    c <- round((expected_drift - ms[i])/granularity)

    # Apply the correction to unwrap the current drift value
    drift[i] <- ms[i] + c * granularity
  }

  # Offset the drift so that the first value is zero
  drift <- drift - drift[1]
  return(drift)
}
