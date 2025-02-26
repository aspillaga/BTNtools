#' Rescale Variables to a New Range
#'
#' This function rescales a numeric vector to a specified new range. The
#' rescaling is based on either the range of the input values or a user-provided
#' original range. It applies a linear transformation to scale the values
#' proportionally to the desired range.
#'
#' @param values A numeric vector of values to be rescaled.
#' @param new.range A numeric vector of length 2 specifying the minimum and
#'    maximum values of the new range.
#' @param orig.range (Optional) A numeric vector of length 2 specifying the
#'    original range of the values. If `NULL` (default), the function uses the
#'    range of `values` (i.e., `range(values)`).
#'
#' @return A numeric vector of the same length as `values`, with the values
#'    rescaled to the specified range (`new.range`).
#'
#' @examples
#' # Rescale values to a range of [0, 1]
#' values <- c(10, 20, 30, 40, 50)
#' rescaled_values <- rescale(values, new.range = c(0, 1))
#' print(rescaled_values)
#'
#' # Rescale values to a custom range, using a provided original range
#' custom_range <- c(5, 50)
#' rescaled_range <- rescale(values, new.range = c(-1, 1),
#'                           orig.range = custom_range)
#' print(rescaled_range)
#'
#' @export
#'
#'
rescale <- function(values, new.range, orig.range = NULL) {

  if (is.null(orig.range)) {
    orig.range <- range(values, na.rm = TRUE)
    if(orig.range[1] == orig.range[2]) orig.range[2] <- orig.range[1] + 1
  }
  rescaled <- (new.range[1] + (values - orig.range[1]) *
                 (new.range[2] - new.range[1]) /
                 (orig.range[2] - orig.range[1]))
  return(rescaled)
}
