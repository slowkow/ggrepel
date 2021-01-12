#' Transverse nudge labels a fixed distance from points
#'
#' `position_nudge_repel_t` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' [position_nudge_repel()] is that the nudging is away from from a smooth
#' line along the data points.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`,
#' @param method One of "spline" or "linear".
#' @param direction One of "none", or "split".
#' @note Nudging direction is away from a smooth spline or linear regression
#'   fitted to the data points. This works only if the observations fall roughly
#'   on a curve or straight line that is montonic in `y`.
#' @export
#' @examples
#'
#' set.seed(16532)
#' df <- data.frame(
#'   x = -10:10,
#'   y = (-10:10)^2,
#'   yy = (-10:10)^2 + rnorm(21, 0, 4),
#'   l = letters[1:21]
#' )
#'
#' # Point on a line or curve
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 1, y = 12)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = -0.5, y = -6)) +
#'   scale_y_continuous(expand = expansion(mult = 0.35))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 0.5, y = 7)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, -y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = -0.5, y = -7)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, -y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 0.5, y = 7)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, y - 40)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 0.5, y = 7)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(y, sqrt(y))) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 7, y = 1)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(y, sqrt(y))) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = -5, y = -0.5)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' # Points scattered near a curve or line, we use 'direction = "split"'
#' ggplot(df, aes(x)) +
#'   geom_point(aes(y = yy)) +
#'   geom_line(aes(y = y)) +
#'   geom_text_repel(aes(y = yy, label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 1, y = 12,
#'                                                     direction = "split")) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm") +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 3, y = 3,
#'                                                     direction = "split")) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm") +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 3, y = 3,
#'                                                     method = "linear",
#'                                                     direction = "split")) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(y, -yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm") +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 3, y = 3,
#'                                                     method = "linear",
#'                                                     direction = "split")) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
position_nudge_repel_t <- function(x = 0,
                                   y = 0,
                                   method = "spline",
                                   direction = "none") {
  ggproto(NULL, PositionTNudgeRepel,
    x = x,
    y = y,
    method = method,
    direction = direction
  )
}

#' @rdname ggrepel-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionTNudgeRepel <- ggproto("PositionTNudgeRepel", Position,
  x = 0,
  y = 0,
  method = "spline",
  direction = "none",

  setup_params = function(self, data) {
    list(x = self$x,
         y = self$y,
         method = self$method,
         direction = self$direction
         )
  },

  compute_layer = function(self, data, params, layout) {
    x_orig <- data$x
    y_orig <- data$y
    if (nrow(data) < 4 || params$method == "linear") {
      mf <- lm(y ~ x, data = data)
      curve <- predict(mf)
      sm.deriv <- coef(mf)[2]
      if (params$method != "linear") {
        message("Fitting a linear regression as n < 4")
      }
    } else if (params$method == "spline") {
      sm.spline <- smooth.spline(data$x, data$y)
      curve <- predict(sm.spline, x = data$x, deriv = 0)$y
      sm.deriv <- predict(sm.spline, x = data$x, deriv = 1)$y
    } else {
      stop("Method \"", params$method, "\"not recognized")
    }
    # compute x and y nudge for each point
    # this code sort of gets the job sort of done, but looks too complicated!!
    angle <- atan2(sm.deriv, 1) + 0.5 * pi * sign(sm.deriv)
    x_nudge <- params$x * cos(angle) * sign(angle)
    y_nudge <- params$y * sin(angle) * sign(angle)
    if (params$direction == "split") {
      x_nudge <- ifelse(data$y >= curve, x_nudge, -x_nudge)
      y_nudge <- ifelse(data$y >= curve, y_nudge, -y_nudge)
    } else if (params$direction != "none") {
      warning("Ignoring unrecognized direction \"", params$direction, "\".")
    }
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
