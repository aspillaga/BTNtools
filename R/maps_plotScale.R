#' Add a Scale Bar to a Map
#'
#' This function adds a scale bar to a map plotted in a UTM projection. The
#' scale bar is drawn with specified units and dimensions.
#'
#' @param length A numeric value specifying the length of the scale bar. The
#'    length is in the units defined by the `units` parameter.
#' @param units A character string specifying the units for the scale bar. It
#'    can be either "m" (meters) or "km" (kilometers). Defaults to "m".
#' @param col A character string specifying the color of the scale bar and the
#'    text. Defaults to `"gray50"`.
#' @param side A character string indicating which side of the plot the scale
#'    bar will be drawn on. Either "left" or "right". Defaults to "left".
#' @param lwd A numeric value specifying the width of the scale bar line.
#' @param inset A numeric vector of length 2 specifying the inset distances for
#'    the margins as fractions of the plot region. The first value controls the
#'    horizontal position, and the second controls the vertical position.
#' @param whisk.len A numeric value specifying the length of the whiskers at the
#'    ends of the scale bar.
#'
#' @return This function adds a scale bar to the current plot. It does not
#'    return any value.
#'
#' @examples
#' # Example usage
#' plot(1:500, 1:500, type = "n", asp = 1)
#' plotScale(length = 100, units = "m", side = "right")
#' plotScale(length = 0.5, units = "km", side = "left", inset = c(0.05, 0.05))
#'
#' @export
#'
#'
plotScale <- function(length = 100, units = "m", col = "gray50", side = "left",
                      lwd = 1.2, inset = c(0.015, 0.025), whisk.len = 0.02) {
  usr <- par("usr")
  fig <- par("fig")

  l <- ifelse(units == "km", length * 1000, length)

  if (side == "right") {
    pos_x <- usr[1] + inset[1] * diff(usr[1:2]) / diff(fig[1:2])
  } else {
    pos_x <- usr[2] - inset[1] * diff(usr[1:2]) / diff(fig[1:2]) - l
  }
  pos_y <- usr[3] + inset[2] * diff(usr[3:4]) / diff(fig[3:4])


  arrows(x0 = pos_x, y0 = pos_y, x1 = pos_x + l, y1 = pos_y,
         col = col, lwd = lwd, angle = 90, length = whisk.len, code = 3)
  text(pos_x + l/2, pos_y, pos = 3, labels = paste(length, units),
       col = col)
}

