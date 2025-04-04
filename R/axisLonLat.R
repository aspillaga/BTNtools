#' Add Latitude and Longitude Axes to a Projected Map
#'
#' Adds latitude and longitude axes (in degrees and optionally minutes) to an
#' existing map plotted using a projected coordinate system (e.g., UTM).
#'
#' @param side Integer vector indicating the sides of the plot where the axes
#'   should be drawn. Possible values are 1 (bottom), 2 (left), 3 (top), and
#'   4 (right). Default is `c(1, 2)` (bottom and left).
#' @param map.crs A `crs` object or any valid input to `sf::st_crs()` defining
#'   the coordinate reference system of the current plot.
#' @param by Numeric. Interval for longitude and latitude labels, in minutes.
#'   If `lat` and `lon` are not provided, `by` is used to generate them.
#' @param lat Optional numeric vector of latitudes (in degrees) at which to draw
#'   horizontal axis labels.
#' @param lon Optional numeric vector of longitudes (in degrees) at which to
#'   draw vertical axis labels.
#' @param minutes Logical. If `TRUE` (default), axis labels show degrees and
#'   minutes. If `FALSE`, only decimal degrees are shown.
#' @param ... Additional arguments passed to the `axis()` function (e.g. `line`,
#'   `tcl`, `lwd`, etc.).
#'
#' @details This function reads the current plot extent using `par("usr")`,
#'   transforms it to geographic coordinates (longitude/latitude, WGS84),
#'   and generates a graticule using `sf::st_graticule()`.
#'
#' @return No value is returned. The function adds axes to the current plot.
#'
#' @export
#'
#'
axisLonLat <- function(side = c(1, 2), map.crs, by = NULL, lat = NULL,
                       lon = NULL, minutes = TRUE, ...) {

  requireNamespace("sf", quietly = TRUE)

  # Map projection
  if (class(map.crs) != "crs") map.crs <- sf::st_crs(map.crs)

  ext_plot <- par("usr")
  names(ext_plot) <- c("xmin", "xmax", "ymin", "ymax")
  ext_plot <- sf::st_bbox(ext_plot, crs = map.crs)

  ext_t <- sf::st_transform(ext_plot, crs = st_crs("+proj=longlat +datum=WGS84"))

  if (is.null(lat) & is.null(lon) & !is.null(by)) {
    lon <- seq(floor(ext_t[1]), ceiling(ext_t[3]), by = by/60)
    lat <- seq(floor(ext_t[2]), ceiling(ext_t[4]), by = by/60)
  }

  g <- sf::st_graticule(st_bbox(ext_plot), crs = map.crs, lat = lat, lon = lon)

  if (minutes) {
    deg <- floor(abs(g$degree))
    min <- round((abs(g$degree) - deg) * 60, 2)
  } else {
    deg <- round(abs(g$degree), 5)
    min <- rep(0, length(g$degree))
  }

  type <- g$type
  type[type == "E" & g$degree < 0] <- "W"
  type[type == "N" & g$degree < 0] <- "S"
  g$degree_label <- paste0(deg, "*degree",
                           ifelse(min != 0, paste0("*", min, "*minute"), ""),
                           "*", type)

  if (1 %in% side ) {
    axis(1, at = g$x_start[g$type == "E"],
         labels = parse(text = g$degree_label[g$type == "E"]), ...)
  }

  if (3 %in% side) {
    axis(3, at = g$x_end[g$type == "E"],
         labels = parse(text = g$degree_label[g$type == "E"]), ...)
  }

  if (2 %in% side) {
    axis(2, at = g$y_start[g$type == "N"],
         labels = parse(text = g$degree_label[g$type == "N"]), ...)
  }

  if (4 %in% side) {
    axis(4, at = g$y_end[g$type == "N"],
         labels = parse(text = g$degree_label[g$type == "N"]), ...)
  }

}
