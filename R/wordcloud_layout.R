# This code is inspired by wordcloud:
#   https://cran.r-project.org/web/packages/wordcloud/index.html

#' Adjust the layout of a list of potentially overlapping boxes.
wordcloud_layout <- function(boxes,
                           xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                           tstep = 0.01, rstep = 0.01, maxiter = 10000) {
  n <- length(boxes)
  sdx <- max(sd(sapply(boxes, "[[", "x1"), na.rm = TRUE), 1)
  sdy <- max(sd(sapply(boxes, "[[", "y1"), na.rm = TRUE), 1)

  newboxes <- lapply(1:n, c)
  centroids <- lapply(boxes, centroid)

  # Order by x,y coordinates.
  # box.order <- order(sapply(boxes, "[[", "x1"), sapply(boxes, "[[", "y1"))

  # Order by decreasing number of overlaps, x coordinate, y coordinate.
  box.order <- order(
    -sapply(1:n, function(i) {
      sum(sapply(boxes[-i], function(a) overlaps(a, boxes[[i]])))
    }),
    sapply(boxes, "[[", "x1"),
    sapply(boxes, "[[", "y1")
  )

  for (i in box.order) {
    r <- 0
    theta <- i / n * 2 * pi

    b <- boxes[[i]]
    b <- put_within_bounds(b, xlim, ylim)
    x1 <- b[1]
    y1 <- b[2]
    x2 <- b[3]
    y2 <- b[4]

    attempts <- 0
    while (TRUE) {
      if (
        attempts > maxiter ||
        (!overlaps_boxes(b, newboxes) && !overlaps_points(b, centroids))
      ) {
        newboxes[[i]] <- b
        # cat(i, ' attempts ', attempts, '\n')
        break
      } else {
        theta <- theta + tstep
        r <- r + rstep * tstep / (2 * pi)
        dx <- sdx * r * cos(theta)
        dy <- sdy * r * sin(theta)
        b <- c(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
        b <- put_within_bounds(b, xlim, ylim)
        attempts <- attempts + 1
      }
    }
  }
  data.frame(
    "x" = sapply(newboxes, function(b) (b[1] + b[3]) / 2),
    "y" = sapply(newboxes, function(b) (b[2] + b[4]) / 2)
  )
}

# # This turns out to be quite slow.
# # Eventually try using an R-tree:
# # https://github.com/imbcmdth/RTree/blob/master/src/rtree.js
# #' Test if a box overlaps any box in a list of boxes.
# overlaps_boxes.ir <- function(b, xr, yr) {
# #   xhits <- subjectHits(findOverlaps(IRanges(b[1], b[3]), xr))
# #   yhits <- subjectHits(findOverlaps(IRanges(b[2], b[4]), yr))
#   xhits <- which(IRanges::start(xr) <= b[3] & IRanges::end(xr) >= b[1])
#   yhits <- which(IRanges::start(yr) <= b[4] & IRanges::end(yr) >= b[2])
#   length(intersect(xhits, yhits)) > 0
# }
#
# #' Adjust the layout of a list of potentially overlapping boxes.
# position_boxes.ir <- function(boxes,
#                           xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
#                           tstep = 0.01, rstep = 0.01, maxiter = 10000) {
#   n <- length(boxes)
#   sdx <- max(sd(sapply(boxes, "[[", "x1"), na.rm = TRUE), 1)
#   sdy <- max(sd(sapply(boxes, "[[", "y1"), na.rm = TRUE), 1)
#
#   newboxes <- lapply(1:n, c)
#   xr <- IRanges()
#   yr <- IRanges()
#   centroids <- lapply(boxes, centroid)
#
#   # Order by decreasing number of overlaps, x coordinate, y coordinate.
#   box.order <- order(
#     -sapply(1:n, function(i) {
#       sum(sapply(boxes[-i], function(a) overlaps(a, boxes[[i]])))
#     }),
#     sapply(boxes, "[[", "x1"),
#     sapply(boxes, "[[", "y1")
#   )
#
#   for (i in box.order) {
#     r <- 0
#     theta <- i / n * 2 * pi
#
#     b <- boxes[[i]]
#     b <- put_within_bounds(b, xlim, ylim)
#     x1 <- b[1]
#     y1 <- b[2]
#     x2 <- b[3]
#     y2 <- b[4]
#
#     attempts <- 0
#     while (TRUE) {
#       if (
#         attempts > maxiter ||
#         (!overlaps_boxes.ir(b * 1e9, xr, yr) && !overlaps_points(b, centroids))
#       ) {
#         xr <- c(xr, IRanges(b[1] * 1e9, b[3] * 1e9))
#         yr <- c(yr, IRanges(b[2] * 1e9, b[4] * 1e9))
#         newboxes[[i]] <- b
#         cat(i, ' attempts ', attempts, '\n')
#         break
#       } else {
#         theta <- theta + tstep
#         r <- r + rstep * tstep / (2 * pi)
#         dx <- sdx * r * cos(theta)
#         dy <- sdy * r * sin(theta)
#         b <- c(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
#         b <- put_within_bounds(b, xlim, ylim)
#         attempts <- attempts + 1
#       }
#     }
#   }
#   data.frame(
#     "x" = sapply(newboxes, function(b) (b[1] + b[3]) / 2),
#     "y" = sapply(newboxes, function(b) (b[2] + b[4]) / 2)
#   )
# }
