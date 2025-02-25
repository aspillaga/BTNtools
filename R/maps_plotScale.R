#' Plot scale bar on a map
#'
#' This function draws a scale bar on a map made with a UTM projection.
#'
#' @param length A numeric value specifying the length of the scale bar in the
#'               units defined by `units`. Defaults to 100.
#' @param units A character string specifying the units for the scale bar.
#'              Either "m" (meters) or "km" (kilometers). Defaults to "m".
#' @param col A character string specifying the color of the scale bar and text.
#'            Defaults to "gray50".
#' @param side A character string indicating the side where the scale bar will
#'             be drawn. Either "left" or "right". Defaults to "left".
#' @param inset A numeric vector of length 2 specifying the inset distances for
#'              the margins as fractions of the plot region.
#' @param whisk.len A numeric value specifying the length of the whiskers at
#'                  the ends of the scale bar.
#' @param lwd A numeric value specifying the width of the scale line.
#'
#' @return This function adds a scale bar to the current plot.
#' @export
plotScale <- function(length = 100, units = "m", col = "gray50", side = "left",
                      inset = c(0.015, 0.025), whisk.len = 0.02, lwd = 1.2) {
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

