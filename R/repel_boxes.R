#' Adjust the layout of a list of potentially overlapping boxes.
repel_boxes_R <- function(boxes,
                        xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                        force = 1e-6, maxiter = 1e4) {
  n <- length(boxes)
  iter <- 0
  any.overlaps <- TRUE

  # height over width
  ratios <- lapply(boxes, function(b)  (b[4] - b[2]) / (b[3] - b[1]))
  original.centroids <- lapply(boxes, centroid)

  if (is.na(force)) {
    force <- mean(sapply(boxes, function(b) {
      mean(c(b[4] - b[2], b[3] - b[1]))
    })) / 2e4
  }
  cat("force = ", force, "\n")

  mx <- 0
  my <- 0

  while (any.overlaps && iter < maxiter) {
    iter <- iter + 1
    any.overlaps <- FALSE

    for (i in 1:n) {
      f <- c(0, 0)
      ci <- centroid(boxes[[i]])

      for (j in 1:n) {
        cj <- centroid(boxes[[j]])

        if (i == j) {
          if (point_within_box(original.centroids[[i]], boxes[[i]])) {
            any.overlaps <- TRUE
            f <- f + compute_force(ci, original.centroids[[i]], force)
          }
        } else {
          if (overlaps(boxes[[i]], boxes[[j]])) {
            any.overlaps <- TRUE
            f <- f + compute_force(ci, cj, force)
          }
          if (point_within_box(original.centroids[[j]], boxes[[i]])) {
            any.overlaps <- TRUE
            f <- f + compute_force(ci, original.centroids[[j]], force)
          }
        }
      }

      # Pull toward the label's point.
      if (!any.overlaps) {
        f <- f + compute_force(original.centroids[[i]], ci, force)
      }

      f[1] <- f[1] * ratios[[i]]

      if (f[1] > mx) mx <- f[1]
      if (f[2] > my) my <- f[2]
      boxes[[i]] <- boxes[[i]] + f
      boxes[[i]] <- put_within_bounds(boxes[[i]], xlim, ylim)
    }
  }
  cat("iter ", iter, "\n")
  cat("max force x ", mx, "\n")
  cat("max force y ", my, "\n")

  data.frame(
    "x" = sapply(boxes, function(b) (b[1] + b[3]) / 2),
    "y" = sapply(boxes, function(b) (b[2] + b[4]) / 2)
  )
}
