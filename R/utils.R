#' Find the intersections between a line and a rectangle.
#' @param p1 A point c(x, y)
#' @param p2 A point c(x, y)
#' @param b A rectangle c(x1, y1, x2, y2)
intersect_line_rectangle <- function(p1, p2, b) {
  slope <- (p2[2] - p1[2]) / (p2[1] - p1[1])
  intercept <- p2[2] - p2[1] * slope
  retval <- list()

  for (x in c(b[1], b[3])) {
    y <- slope * x + intercept
    if (b[2] <= y && y <= b[4]) {
      retval[[length(retval) + 1]] <- c(x, y)
    }
  }

  for (y in c(b[2], b[4])) {
    x <- (y - intercept) / slope
    if (b[1] <= x && x <= b[3]) {
      retval[[length(retval) + 1]] <- c(x, y)
    }
  }

  retval <- unique(retval)

  which.intersection <- which.min(sapply(retval, function(a) {
    euclid(a, p1)
  }))

  retval[[which.intersection]]
}

#' Test if a box overlaps any point in a list of points.
overlaps_points <- function(b, ps) {
  for (p in ps) {
    if (point_within_box(p, b)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Test if a box is within xlim and ylim boundaries.
box_within_bounds <- function(b, xlim, ylim) {
  b[1] > xlim[1] &&
  b[3] < xlim[2] &&
  b[2] > ylim[1] &&
  b[4] < ylim[2]
}

#' Test if a box overlaps any box in a list of boxes.
overlaps_boxes <- function(a, boxes) {
  for (b in boxes) {
    if (overlaps(a, b)) {
      return(TRUE)
    }
  }
  return(FALSE)
}
