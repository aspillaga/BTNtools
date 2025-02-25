#' Rescale variables
#'
#' This function rescales a numeric vector to a specified new range. The
#' rescaling is based on either the range of the input values or a
#' user-provided original range.
#'
#' @param values A numeric vector of values to be rescaled.
#' @param new.range A numeric vector of length 2 specifying the minimum and
#'                  maximum values of the new range.
#' @param orig.range A numeric vector of length 2 specifying the original range
#'                   of the values. If NULL (default), the function uses the
#'                   range of `values`.
#' @return A numeric vector with rescaled values.
#' @export
rescale <- function(values, new.range, orig.range = NULL) {

  if (is.null(orig.range)) {
    max <- max(values, na.rm = TRUE)
    orig.range <- c(0, ifelse(max == 0, 1, max))
  }
  rescaled <- (new.range[1] + (values - orig.range[1]) *
                 (new.range[2] - new.range[1]) /
                 (orig.range[2] - orig.range[1]))
  return(rescaled)
}
