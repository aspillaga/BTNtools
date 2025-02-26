#' Calculate the Speed of Sound in Water Using the Mackenzie Equation
#'
#' This function estimates the speed of sound in water using a simplified version
#' of the Mackenzie equation (Mackenzie, 1981). The calculation is based on
#' temperature, salinity, and depth.
#'
#' @param temp Numeric vector or single value of temperature (°C).
#' @param sal Numeric vector or single value of salinity (PSU).
#' @param depth Numeric vector or single value of depth (m).
#'
#' @return A numeric vector representing the speed of sound in water (m/s).
#'
#' @references
#' Mackenzie, K.V. (1981). Nine-term equation for the sound speed in the oceans.
#' *Journal of the Acoustical Society of America*, 70(3), 807-812.
#' \doi{10.1121/1.386920}
#'
#' @examples
#'
#' # Calculate sound speed for given temperature, salinity, and depth
#' soundSpeed(temp = 21, sal = 38, depth = 20)
#'
#' # Vectorized example
#' temps <- c(10, 15, 20, 25)
#' sound_speeds <- soundSpeed(temp = temps, sal = 35, depth = 50)
#' print(sound_speeds)
#'
#' @export
#'
#'
soundSpeed <- function(temp = 21, sal = 38, depth = 20) {
  c <- (1448.96 + 4.591 * temp - 0.05304 * temp^2 + 0.000237 * temp^3 +
          1.340 * (sal - 35) + 0.0163 * depth)
  return(c)
}
