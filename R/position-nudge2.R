#' Nudge points a fixed distance
#'
#' `position_nudge` is generally useful for adjusting the position of
#' items on discrete scales by a small amount. Nudging is built in to
#' [geom_text()] because it's so useful for moving labels a small
#' distance from what they're labelling.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move.
#' @noRd
position_nudge2 <- function(x = 0, y = 0) {
  ggproto(NULL, PositionNudge2,
    x = x,
    y = y
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @noRd
PositionNudge2 <- ggproto("PositionNudge2", Position,
  x = 0,
  y = 0,

  setup_params = function(self, data) {
    list(x = self$x, y = self$y)
  },

  compute_layer = function(data, params, panel) {
    x_orig <- data$x
    y_orig <- data$y
    # transform only the dimensions for which non-zero nudging is requested
    if (any(params$x != 0)) {
      if (any(params$y != 0)) {
        data <- transform_position(data, function(x) x + params$x, function(y) y + params$y)
      } else {
        data <- transform_position(data, function(x) x + params$x, NULL)
      }
    } else if (any(params$y != 0)) {
      data <- transform_position(data, NULL, function(y) y + params$y)
    }
    data$nudge_x <- data$x
    data$nudge_y <- data$y
    data$x <- x_orig
    data$y <- y_orig
    data
  }
)
