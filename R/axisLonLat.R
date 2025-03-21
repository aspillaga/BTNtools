#' Add Latitude and Longitude Axes to a UTM Map
#'
#' This function adds axes to a map, displaying coordinates in latitude and
#' longitude format (degrees and minutes), for a map projected in UTM.
#'
#' @param map.crs A character string specifying the Proj4 code or an object of
#'    class `CRS` of the plotted  map projection .
#' @param xlim A numeric vector of length 2 specifying the limits of the X-axis.
#'    If `NULL` (the default), the function uses the current plot limits.
#' @param ylim A numeric vector of length 2 specifying the limits of the Y-axis.
#'    If `NULL` (the default), the function uses the current plot limits.
#' @param by A numeric value specifying the interval for the axis labels, in
#'    degrees.
#' @param side An integer (1, 2, 3, or 4) indicating which side of the plot the
#'    axis should be drawn on. If `NULL` (the default), the function will draw
#'    axes on both the X and Y sides of the plot.
#' @param line A numeric value specifying the line position of the axis labels.
#'    Default is 0, which places the labels at the default position. Positive
#'    values move the labels outward, and negative values move them inward.
#' @param tcl A numeric value specifying the tick length.
#'
#' @return This function adds latitude and longitude axis labels to the
#' current plot. It does not return any value.
#'
#' @export
#'
#'
axisLonLat <- function(map.crs, xlim = NULL, ylim = NULL, by = 0.5, side = NULL,
                       line = 0, tcl = -0.5) {

  requireNamespace("sp", quietly = TRUE)

  # Map projection
  if (class(map.crs) == "crs") map.crs <- as(map.crs, "CRS")
  if (class(map.crs) != "CRS") map.crs <- sp::CRS(map.crs)

  # Projection for axis
  proj_grid <- sp::CRS("+proj=longlat +datum=WGS84")

  # X and Y axis limits (if not provided)
  if (is.null(xlim) | is.null(ylim)) {
    xlim <- par("usr")[1:2]
    ylim <- par("usr")[3:4]
  }

  # Project X and Y limits to the axis projection
  lim_coord <- sp::SpatialPoints(cbind(x=xlim, y = ylim), proj4string = map.crs)
  lim_proj <- sp::spTransform(lim_coord, proj_grid)

  grid_coord <- sp::coordinates(lim_proj)
  grid_min <- (grid_coord - floor(grid_coord)) * 60 # Minuts
  grid_min[1, ] <- floor(grid_min[1, ])
  grid_min[2, ] <- ceiling(grid_min[2, ])

  grid_gm <- grid_min + floor(grid_coord) * 60

  # Sequence of labels
  seq_x <- seq(grid_gm[1, 1], grid_gm[2, 1], by = by)
  seq_x <- pretty(seq_x/60, 8)
  seq_y <- seq(grid_gm[1, 2], grid_gm[2, 2], by = by)
  seq_y <- pretty(seq_y / 60, 5)

  # Project label position
  grid_proj <- sp::gridlines(lim_proj, easts = seq_x, norths = seq_y,
                             ndiscr = 50)
  at_proj <- sp::gridat(lim_proj, easts = seq_x, norths = seq_y, offset = 0.3)

  grid_map <- sp::spTransform(grid_proj, map.crs)
  at_map <- sp::spTransform(at_proj, map.crs)
  at_coord <- sp::coordinates(at_map)

  indx <- which(at_map$pos == 1)

  # Generate labels
  labels_x <- sp::coordinates(at_proj)[indx, 1]
  deg_x <- floor(labels_x)
  min_x <- round((labels_x - deg_x) * 60, 1)
  labels_x <- paste0(deg_x, " *degree",
                    ifelse(min_x > 0, paste0(" *", min_x, " *minute"), ""),
                    " *E")

  labels_y <- sp::coordinates(at_proj)[-indx, 2]
  deg_y <- floor(labels_y)
  min_y <- round((labels_y - deg_y) * 60, 1)
  labels_y <- paste0(deg_y, " *degree",
                    ifelse(min_y > 0, paste0("* ", min_y, " *minute"), ""),
                    " *N")

  # Plot axes
  at_map$labels <- c(labels_x, labels_y)

  if (is.null(side)) {
    axis(1, at = at_coord[indx, 1], lwd = 0, lwd.ticks = 0.5,
         labels = rep("", length(at_coord[indx, 1])), line = 0,
         tcl = tcl)
    axis(1, at = at_coord[indx, 1], lwd = 0, line = line,
         labels = parse(text = as.character(at_map$labels))[indx])

    axis(2, at = at_coord[-indx, 2], lwd = 0, lwd.ticks = 0.5,
         labels = rep("", length(at_coord[-indx, 2])), line = 0,
         tcl = tcl)
    axis(2, at = at_coord[-indx, 2], lwd = 0, line = line,
         labels = parse(text = as.character(at_map$labels))[-indx])

  } else {

    if (side %in% c(1, 3)) {
      axis(side, at = at_coord[indx, 1], lwd = 0, lwd.ticks = 0.5,
           labels = rep("", length(at_coord[indx, 1])),
           line = 0, tcl = tcl)
      axis(side, at = at_coord[indx, 1], lwd = 0, line = line,
           labels = parse(text = as.character(at_map$labels))[indx])

    } else {

      axis(side, at = at_coord[-indx, 2], lwd = 0, lwd.ticks = 0.5,
           labels = rep("", length(at_coord[-indx, 2])),
           line = 0, tcl = tcl)
      axis(side, at = at_coord[-indx, 2], lwd = 0, line = line,
           labels = parse(text = as.character(at_map$labels))[-indx])
    }
  }
}
