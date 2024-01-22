#' Name ggplot grid object
#' Convenience function to name grid objects
#'
#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

.pt <- 72.27 / 25.4

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Return a boolean vector of non-empty items.
#'
#' @param xs Vector with a mix of "expression" items, "character" items,
#'  and items from other classes.
#' @return Boolean vector indicating which items are not empty.
#' @noRd
not_empty <- function(xs) {
  sapply(seq_along(xs), function(i) {
    if (is.expression(xs[i])) {
      return(length(nchar(xs[i])) > 0)
    } else {
      return(xs[i] != "")
    }
  })
}

#' Return a unit version of the argument.
#'
#' @param x Number or unit object.
#' @return unit(x, "lines") if number or the unchanged argument if it's already
#'  a unit object.
#' @noRd
to_unit <- function(x) {
  # don't change arg if already unit
  if (is.unit(x)) {
    return(x)
  }

  # NA used to exclude points from repulsion calculations
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  unit(x, "lines")
}

#' Parse takes a vector of n lines and returns m expressions.
#' See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#'
#' parse(text = c("alpha", "", "gamma"))
#' #> expression(alpha, gamma)
#'
#' parse_safe(text = c("alpha", "", "gamma"))
#' #> expression(alpha, NA, gamma)
#'
#' @noRd
parse_safe <- function(text) {
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

#' Exclude data points outside the panel ranges
#' @noRd
exclude_outside <- function(data, panel_scales) {
  if ("x.range" %in% names(panel_scales)) {
    xr <- panel_scales$x.range
    yr <- panel_scales$y.range
    ix <- inside(data$x, xr) & inside(data$y, yr)
    data <- data[ix,,drop=FALSE]
  } else if ("x_range" %in% names(panel_scales)) {
    xr <- panel_scales$x_range
    yr <- panel_scales$y_range
    ix <- inside(data$x, xr) & inside(data$y, yr)
    data <- data[ix,,drop=FALSE]
  }
  data
}

#' Exclude data points outside the panel ranges
#' @noRd
inside <- function(x, bounds) {
  is.infinite(x) | (x <= bounds[2] & x >= bounds[1])
}
