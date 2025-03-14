# FUNCTIONS TO ESTIMATE TRAJECTORY PARAMETERS ##################################

#' Calculate step length between consecutive positions
#'
#' This function computes the step lengths (distance in meters) between
#' consecutive positions in a movement trajectory using UTM coordinates.
#'
#' @param x A numeric vector of x-coordinates (UTM eastings, in meters).
#' @param y A numeric vector of y-coordinates (UTM northings, in meters).
#'
#' @return A numeric vector of step lengths (meters), representing the distance
#'         traveled from the previous coordinate. The first value is NA.
#'
#' @examples
#' x <- c(500000, 500010, 500020)
#' y <- c(4500000, 4500005, 4500015)
#' stepLength(x, y)
#'
#' @export
#'
#'
stepLength <- function(x, y) {
  dist <- sqrt(diff(x)^2 + diff(y)^2)
  return(c(NA, dist))
}


#' Calculate speed from movement trajectory
#'
#' This function calculates the speed (in meters per second) between
#' consecutive positions in a movement trajectory using UTM coordinates.
#'
#' @param x A numeric vector of x-coordinates (UTM eastings, in meters).
#' @param y A numeric vector of y-coordinates (UTM northings, in meters).
#' @param time.stamps A POSIXct vector of timestamps corresponding to each
#'    position.
#'
#' @return A numeric vector of speeds (m/s), with NA as the first value.
#'
#' @examples
#' x <- c(500000, 500010, 500020)
#' y <- c(4500000, 4500005, 4500015)
#' date_time <- as.POSIXct(c("2025-01-01 12:00:00", "2025-01-01 12:00:10",
#'                           "2025-01-01 12:00:20"))
#' speedCalc(x, y, date_time)
#'
#' @export
#'
#'
speedCalc <- function(x, y, time.stamps) {
  dist <- stepLength(x, y)
  t_diff <- difftime(time.stamps[-1], time.stamps[-length(time.stamps)],
                     units = "secs")
  speed <- dist / c(NA, as.numeric(t_diff))
  return(speed)
}


#' Calculate movement bearing
#'
#' This function calculates the bearing (movement direction) between consecutive
#' positions using UTM coordinates, using North (0°) as a reference.
#'
#' @param x A numeric vector of x-coordinates (UTM eastings, in meters).
#' @param y A numeric vector of y-coordinates (UTM northings, in meters).
#' @param rad Logical. If TRUE, returns the bearing in radians; otherwise, in
#'    degrees. Defaults to FALSE (degrees).
#'
#' @return A numeric vector of bearings, with NA as the first value.
#'
#' @examples
#' x <- c(500000, 500010, 500020)
#' y <- c(4500000, 4500005, 4500015)
#' bearing(x, y)
#'
#' @export
#'
#'
bearing <- function(x, y, rad = FALSE) {
  diff_x <- diff(x)
  diff_y <- diff(y)
  angle <- atan2(y = diff_x, x = diff_y)
  angle[diff_x == 0 & diff_y == 0] <- NA
  if (!rad) angle <- angle * 180/pi
  return(c(NA, angle))
}


#' Calculate turning angles
#'
#' This function computes the turning angle (change in movement direction)
#' between consecutive steps
#'
#' This function calculates the turning angle between consecutive movement
#' steps in a trajectory using UTM coordinates.The turning angle is defined as
#' the difference between the bearing of the previous step (from the preceding
#' position to the current position) and the bearing of the following step (from
#' the current position to the next position). A positive angle indicates a
#' right turn, while a negative angle indicates a left turn. The first and last
#' positions will return `NA` since they do not have both a preceding and a
#' following step.
#'
#' @param x A numeric vector of x-coordinates (UTM eastings, in meters).
#' @param y A numeric vector of y-coordinates (UTM northings, in meters).
#' @param rad Logical. If TRUE, returns the turning angle in radians; otherwise,
#'    in degrees. Defaults to FALSE (degrees).
#'
#' @return A numeric vector of turning angles, with NA for the first and last
#'    values.
#'
#' @examples
#' x <- c(500000, 500010, 500020, 500015, 500020)
#' y <- c(4500000, 4500005, 4500015, 4500020, 4500025)
#' turnAng(x, y)
#'
#' @export
#'
#'
turnAng <- function(x, y, rad = FALSE) {
  b <- bearing(x, y, rad = FALSE)
  b1 <- c(NA, b)[cummax(seq_along(b) * (!is.na(b))) + 1]
  angle <- c(b1[-1] - b1[-length(b1)], NA)
  angle[which(angle <= -180)] <- 360 + angle[which(angle <= -180)]
  angle[which(angle > 180)] <- angle[which(angle > 180)] - 360
  angle[is.na(c(b[-1], NA))] <- NA
  if (rad) {
    angle <- angle * pi/180
  }
  return(c(angle))
}


# TRAJECTORY FILTER ############################################################

#' Filter movement trajectories based on speed and turning angles
#'
#' This function filters movement trajectories by removing unrealistic
#' locations based on speed thresholds and trajectory spikes. Spikes are
#' defined as consecutive long steps with sharp turning angles, which may
#' indicate erroneous position fixes.
#'
#' @param x A numeric vector of x-coordinates (easting) in UTM.
#' @param y A numeric vector of y-coordinates (northing) in UTM.
#' @param time.stamp A vector of timestamps in `POSIXct` format, corresponding
#'    to each location.
#' @param max.speed A numeric value specifying the maximum allowed speed (m/s).
#'    Locations that result in speeds above this threshold will be removed.
#'    Defaults to `Inf` (effectively no filtering).
#' @param max.turn.step A numeric value specifying the minimum step length
#'    (meters) to be considered in trajectory spike detection. Steps longer
#'    than this threshold will be used in the turning angle filter. Defaults
#'    to `15` meters.
#' @param max.turn.ang A numeric value (in degrees) specifying the minimum
#'    turning angle that defines a trajectory spike. A spike is detected when
#'    two consecutive steps are longer than `max.turn.step` and the turning
#'    angle is more acute than `max.turn.ang`. Defaults to `15` degrees.
#' @param rounds An integer specifying the number of filtering iterations.
#'    Defaults to `1`.
#'
#' @return A logical vector of the same length as `x` and `y`, indicating
#'    whether each location is retained (`TRUE`) or removed (`FALSE`).
#'
#' @examples
#' x <- c(500000, 500010, 500020,  500018, 500020, 500030, 500040)
#' y <- c(4500000, 4500010, 4500025, 4500042, 4500026, 4500030, 4500045)
#' time.stamp <- as.POSIXct(c("2025-01-01 12:00:00", "2025-01-01 12:00:05",
#'                             "2025-01-01 12:00:10", "2025-01-01 12:00:15",
#'                             "2025-01-01 12:00:20"), tz = "UTC")
#' filt <- trackFilt(x, y, time.stamp, max.speed = 5, max.turn.ang = 20,
#'                   max.turn.step = 10)
#' plot(x, y, col = factor(filt, levels = c(TRUE, FALSE)), asp = 1, type = "b")
#'
#' @export
#'
#'
trackFilt <- function(x, y, time.stamp, max.speed = Inf, max.turn.step = 15,
                      max.turn.ang = 15, rounds = 1) {

  indx <- rep(TRUE, length(x))

  # Repeat the filtering as many times as "rounds"
  for (i in 1:rounds) {

    # 1. Remove trajectory spikes (consecutive long steps with acute angles)

    # Identify consecutive movements with long step lengths
    step_length <- stepLength(x[indx], y[indx])
    # Fill first NA value (added by stepLength)
    if (is.na(step_length[1])) step_length[1] <- Inf
    step_indx <- (step_length > max.turn.step &
                    c(step_length[-1], Inf) > max.turn.step)

    # Identify acute turning angles
    turn_ang <- turnAng(x[indx], y[indx])
    turn_ang[is.na(turn_ang)] <- 0
    angle_indx <- abs(turn_ang) > (180 - max.turn.ang)

    indx[indx][angle_indx & step_indx] <- FALSE

    # 2. Remove locations that cause large speeds
    speed <- speedCalc(x[indx], y[indx], time.stamp[indx])
    indx[indx][speed > max.speed] <- FALSE

  }

  return(indx)

}
