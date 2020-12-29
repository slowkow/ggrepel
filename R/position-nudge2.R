# COPIED FROM 'ggrepel' unchanged except for being exported
# This is only temporary patch until 'ggrepel' is updated
#
#' Nudge points a fixed distance
#'
#' `position_nudge_repel` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. Nudging is built in to
#' [geom_text_repel()] because it is useful for consistently moving labels some
#' distance from what they're labelling.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move.
#' @export
position_nudge_repel <- function(x = 0, y = 0) {
  ggproto(NULL, PositionNudgeRepel,
    x = x,
    y = y
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeRepel <- ggproto("PositionNudgeRepel", Position,
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
