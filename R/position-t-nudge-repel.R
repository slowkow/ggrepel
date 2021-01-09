#' Transverse nudge labels a fixed distance from points
#'
#' `position_t_nudge_repel` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' [position_nudge_repel()] is that the nudging is away from from a smooth
#' line along the data points.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`,
#' @param ... Additional arguments passed to `smooth.spline()`.
#' @note Nudging direction is away from a smooth spline fitted to the data
#'   points. This works only if the observations fall roughly on a curve that is
#'   montonic in `y`.
#' @export
#' @examples
#' set.seed(16532)
#' df <- data.frame(
#'   x = -10:10,
#'   z = (-10:10)^2,
#'   y = letters[1:21]
#' )
#'
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = 1, y = 12)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = -0.5, y = -6)) +
#'   scale_y_continuous(expand = expansion(mult = 0.35))
#'
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = 0.5, y = 7,
#'                                                     x_center = mean,
#'                                                     y_center = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, -z)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = -0.5, y = -7,
#'                                                     x_center = mean,
#'                                                     y_center = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, -z)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = 0.5, y = 7,
#'                                                     x_center = mean,
#'                                                     y_center = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, z - 40)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = 0.5, y = 7,
#'                                                     x_center = mean,
#'                                                     y_center = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(z, sqrt(z))) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = 7, y = 1,
#'                                                     x_center = mean,
#'                                                     y_center = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(z, sqrt(z))) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_t_nudge_repel(x = -5, y = -0.5,
#'                                                     x_center = mean,
#'                                                     y_center = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
position_t_nudge_repel <- function(x = 0,
                                   y = 0,
                                   ...) {
  ggproto(NULL, PositionTNudgeRepel,
    x = x,
    y = y
  )
}

#' @rdname ggrepel-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionTNudgeRepel <- ggproto("PositionTNudgeRepel", Position,
  x = 0,
  y = 0,

  setup_params = function(self, data) {
    list(x = self$x,
         y = self$y)
  },

  compute_layer = function(self, data, params, layout) {
    if (nrow(data) < 4) {
      # TODO implement alternative approach
      stop("'position_t_nudge_repel()' requires at lest for observations in 'data'.")
    }
    x_orig <- data$x
    y_orig <- data$y
    sm.spline <- smooth.spline(data$x, data$y)
    sm.deriv <- predict(sm.spline, x = data$x, deriv = 1)
    # compute x and y nudge for each point
    # this code sort of gets the job sort of done, but looks too complicated!!
    angle <- atan2(sm.deriv$y, 1) + 0.5 * pi * sign(sm.deriv$y)
    x_nudge <- params$x * cos(angle) * sign(angle)
    y_nudge <- params$y * sin(angle) * sign(angle)
    # transform both dimensions
    data <- transform_position(data,
                               function(x) x + x_nudge,
                               function(y) y + y_nudge)
    data$nudge_x <- data$x
    data$nudge_y <- data$y
    data$x <- x_orig
    data$y <- y_orig
    data
  }
)
