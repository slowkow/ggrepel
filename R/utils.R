#' Find the intersections between a line and a rectangle.
#' @param p1 A point like \code{c(x, y)}
#' @param p2 A point like \code{c(x, y)}
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
