#' Select the Most Abundant Value in a Vector
#'
#' This function identifies and returns the most abundant value in a vector.
#' It calculates the frequency of each value and selects the one with the
#' maximum count. If there is a tie (i.e., multiple values have the same
#' maximum count), one of the tied values is selected randomly. This function
#' can be useful for selecting the receiver or station with the largest number
#' of detections within a time interval.
#'
#' @param values A vector of values, usually station names or receiver IDs.
#'
#' @return The most abundant value from the input vector.
#'
#' @examples
#' station_ids <- c("st1", "st2", "st2", "st3", "st3")
#' mainVal(station_ids)
#'
#' @export
#'
#'
mainVal <- function(values) {
  tab <- table(values)
  return(sample(names(tab[tab == max(tab, na.rm = TRUE)]), 1))
}
