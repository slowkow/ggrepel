# euclid <- function(p1, p2) {
#   sqrt((p2[2] - p1[2]) ^ 2 + (p2[1] - p1[1]) ^ 2)
# }

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

# put_within_bounds <- function(b, xlim, ylim) {
#   if (b[1] < xlim[1]) {
#     d <- abs(b[1] - xlim[1])
#     b[1] <- b[1] + d
#     b[3] <- b[3] + d
#   } else if (b[3] > xlim[2]) {
#     d <- abs(b[3] - xlim[2])
#     b[1] <- b[1] - d
#     b[3] <- b[3] - d
#   }
#   if (b[2] < ylim[1]) {
#     d <- abs(b[2] - ylim[1])
#     b[2] <- b[2] + d
#     b[4] <- b[4] + d
#   } else if (b[4] > ylim[2]) {
#     d <- abs(b[4] - ylim[2])
#     b[2] <- b[2] - d
#     b[4] <- b[4] - d
#   }
#   return(b)
# }

# #' Test if a point is within the boundaries of a box.
# point_within_box <- function(p, b) {
#   p[1] >= b[1] &&
#   p[1] <= b[3] &&
#   p[2] >= b[2] &&
#   p[2] <= b[4]
# }

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

# #' Test if a box overlaps another box.
# overlaps <- function(a, b) {
#   b[1] <= a[3] &&
#   b[2] <= a[4] &&
#   b[3] >= a[1] &&
#   b[4] >= a[2]
# }

#' Test if a box overlaps any box in a list of boxes.
overlaps_boxes <- function(a, boxes) {
  for (b in boxes) {
    if (overlaps(a, b)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# This is 3-4 times slower than the simple loop above.
# overlaps_boxes <- function(a, boxes) {
#   xs1 <- sapply(boxes, "[", 1)
#   ys1 <- sapply(boxes, "[", 2)
#   xs2 <- sapply(boxes, "[", 3)
#   ys2 <- sapply(boxes, "[", 4)
#   sum(xs1 <= a[3] & ys1 <= a[4] & xs2 >= a[1] & ys2 >= a[2]) > 0
# }

# centroid <- function(b) {
#   c((b[1] + b[3]) / 2, (b[2] + b[4]) / 2)
# }

# #' Compute the force upon point a from point b.
# compute_force <- function(a, b, force = 1e-6) {
#   a <- a + rnorm(2, 0, force)
#   d <- max(euclid(a, b), 1e-2)
#   v <- (a - b) / d
#   force * v / (d ^ 2)
# }
