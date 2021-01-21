#' Nudge labels a variable distance from points
#'
#' `position_nudge_repel_t` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' [position_nudge_repel()] is that the nudging is away from from a line or
#' curve fitted to the data points or supplied as coefficients. While
#' [position_nudge_repel()] is most useful for "round-shaped", vertically- or
#' horizontally elongated clouds of points, [position_nudge_repel_t()] is most
#' suitable when observations have a linear or curvilinear relationship
#' between _x_ and _y_ values.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`.
#' @param xy_relative Nudge relative to _x_ and _y_ data expanse, ignored
#'   unless `x` and `y` are both `NA`s.
#' @param abline a vector of length two giving the intercept and slope.
#' @param method One of `"spline"`, `"lm"` or `"auto"`.
#' @param formula A model formula for [lm()] when `method = "lm"`. Ignored
#'   otherwise.
#' @param direction One of "none", or "split".
#' @param line_nudge A positive multiplier >= 1, increasing nudging
#'   away from the curve or line compared to nudging from points.
#' @details The default ammount of nudging is 3% of the spread of the data along
#'   _x_ and _y_ axes, which in most cases is good. In most cases it is best to
#'   apply nudging along a direction perpendicular to the line or curve, if this
#'   is the aim, passing an argument to only one of `x`, `y` or `xy_relative`
#'   will be enough. When `direction = "split"` nudging is away from an implicit
#'   line or curve on either side with positive nudging. The line of curve can
#'   be smooth spline or linear regression fitted on-the-fly to the data points,
#'   or a straight line defined by its coefficients passed to `abline`. The
#'   fitting is well defined only if the observations fall roughly on a curve or
#'   straight line that is monotonic in `y`. By means of `line_nudge` one can
#'   increment nudging away from the line or curve compared to away from the
#'   points, which is useful for example to keep labels outside of a confidence
#'   band. Direction defaults to `"split"` when `line_nudge > 1`, and otherwise
#'   to `"none"`.
#'
#'   This position function is implemented as a panel function and supports
#'   grouping: the spline or linear model are fitted per group wihin each
#'   plot panel.
#'
#' @note For `method = "lm"` only model formulas corresponding to polynomials
#'   with no missing terms are supported. If using [poly()], `raw = TRUE` is
#'   required.
#'
#'   In practice, `x` and `y` should have the same sign for nudging to work
#'   correctly.
#'
#'   This position is most useful when labeling points conforming a cloud along
#'   an arbitrary curve or line.
#'
#' @seealso [ggplot::position_nudge()], [ggrepel::position_nudge_repel()].
#'
#' @export
#' @examples
#'
#' set.seed(16532)
#' df <- data.frame(
#'   x = -10:10,
#'   y = (-10:10)^2,
#'   yy = (-10:10)^2 + rnorm(21, 0, 4),
#'   yyy = (-10:10) + rnorm(21, 0, 4),
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
#' ggplot(df, aes(x, x * 2 + 5)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 1, y = 2,
#'                                                     abline = c(5, 2))) +
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
#' ggplot(df, aes(x, yyy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm") +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   xlim = c(-12, 12),
#'                   position = position_nudge_repel_t(x = 0.5, y = 0.5,
#'                                                     line_nudge = 12,
#'                                                     direction = "split")) +
#'   scale_y_continuous(expand = expansion(mult = 0.15)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
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
#' ggplot(df, aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm") +
#'   geom_text_repel(aes(label = l),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel_t(x = 3, y = 3,
#'                                                     abline = c(0, 1),
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
                                   xy_relative = c(0.03, 0.03),
                                   abline = NULL,
                                   method = NULL,
                                   formula = y ~ x,
                                   direction = NULL,
                                   line_nudge = 1) {
  # set defaults
  if (!is.null(abline)) {
    method <- "abline"
  } else {
    abline <- rep(NA_real_, 2) # to ensure that a list member is created
  }

  if (is.null(method)) {
    method <- "auto" # decided later based on nrow(data)
  }

  if (method == "linear") {
    method <- "lm"
  }

  if (is.null(direction)) {
    if (line_nudge > 1) {
      direction <- "split"
    } else {
      direction <- "none"
    }
  }

  if (length(xy_relative) == 1) {
    xy_relative <- rep(xy_relative, 2)
  }

  stopifnot(length(xy_relative) == 2)

  ggproto(NULL, PositionTNudgeRepel,
          x = x,
          y = y,
          xy_relative = xy_relative,
          abline = abline,
          method = method,
          formula = formula,
          direction = direction,
          line_nudge = line_nudge
  )
}

#' @rdname ggrepel-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionTNudgeRepel <- ggproto("PositionTNudgeRepel", Position,
  x = 0,
  y = 0,
  xy_relative = c(0.03, 0.03),
  abline = rep(NA_real_, 2),
  method = "spline",
  formula = y ~ x,
  direction = "none",
   line_nudge = 1,

   setup_params = function(self, data) {
     list(x = self$x,
          y = self$y,
          xy_relative = self$xy_relative,
          abline = self$abline,
          method = self$method,
          formula = self$formula,
          direction = self$direction,
          line_nudge = self$line_nudge
     )
   },

   compute_panel = function(data, params, scales) {
     x_orig <- data$x
    y_orig <- data$y
    # set parameter defaults that depend on the scale
    x_range <- scales$x$dimension()
    y_range <- scales$y$dimension()
    x_spread <- x_range[2] - x_range[1]
    y_spread <- y_range[2] - y_range[1]
    xy.range.ratio <- x_spread / y_spread

    if (all(is.na(params$x)) & all(is.na(params$y))) {
      params$x <- params$xy_relative[1] * x_spread
      params$y <- params$xy_relative[2] * y_spread
    } else if (xor(all(is.na(params$x)), all(is.na(params$y)))) {
      if (is.na(params$x)) {
        params$x <- params$y * xy.range.ratio
      } else {
        params$y <- params$x / xy.range.ratio
      }
    }

    if (params$method == "auto") {
      if (nrow(data) < 5) {
        params$method <- "lm"
      } else {
        params$method <- "spline"
      }
    }

    # compute lines or curves and their derivatives
    if (params$method == "abline") {
      if (is.numeric(params$abline) && length(params$abline) == 2) {
        curve <- params$abline[1] + params$abline[2] * data$x
        # ensure same length in all cases
        sm.deriv <- rep(params$abline[2], nrow(data))
      } else {
        stop("'abline' should be a numeric vector of length 2")
      }
    } else if (params$method %in% c("lm", "spline")) {
      # we need to handle grouping by ourselves as compute_group does not work
      curve <- sm.deriv <- numeric(nrow(data))
      for (group in unique(data$group)) {
        in.grp <- data$group == group
        if (nrow(data[in.grp, ]) < 4 || params$method == "lm") {
          mf <- lm(formula = params$formula, data = data[in.grp, ])
          curve[in.grp] <- predict(mf)
          coef.poly <- polynom::polynomial(coef(mf))
          deriv.poly <- deriv(coef.poly)
          sm.deriv[in.grp] <- predict(deriv.poly, data[in.grp, "x"])
          if (params$method != "lm") {
            message("Fitting a linear regression as n < 4")
          }
        } else if (params$method == "spline") {
          sm.spline <- smooth.spline(data[in.grp, "x"], data[in.grp, "y"])
          curve[in.grp] <- predict(sm.spline, x = data[in.grp, "x"], deriv = 0)$y
          sm.deriv[in.grp] <- predict(sm.spline, x = data[in.grp, "x"], deriv = 1)$y
        }
      }
    } else {
      stop("Method \"", params$method, "\"not recognized")
    }

    # compute x and y nudge for each point
    # By changing the sign we ensure consistent positions in opposite slopes
    angle.rotation <- ifelse(sm.deriv > 0, -0.5 * pi, +0.5 * pi)
    # scaling is needed to conpute the angle on the plot
    angle <- atan2(sm.deriv * xy.range.ratio, 1) + angle.rotation
    x_nudge <- params$x * cos(angle) * ifelse(sm.deriv > 0, -1, +1)
    y_nudge <- params$y * sin(angle) * ifelse(sm.deriv > 0, -1, +1)

    if (params$direction == "split") {
      # sign depends on position relative to the line or curve
      x_nudge <- ifelse(data$y >= curve, x_nudge, -x_nudge)
      y_nudge <- ifelse(data$y >= curve, y_nudge, -y_nudge)
    } else if (params$direction != "none") {
      warning("Ignoring unrecognized direction \"", params$direction, "\".")
    }

    if (params$line_nudge > 1) {
      # nudging further away from line or curve than from points
      adj_y_nudge <- y_nudge * params$line_nudge - (data$y - curve)
      adj_x_nudge <- x_nudge * adj_y_nudge / y_nudge
      y_nudge <- ifelse(sign(y_nudge) == sign(adj_y_nudge) &
                          abs(y_nudge) < abs(adj_y_nudge),
                        adj_y_nudge,
                        y_nudge)
      x_nudge <- ifelse(sign(y_nudge) == sign(adj_y_nudge) &
                          abs(x_nudge) >= abs(adj_x_nudge),
                        adj_x_nudge,
                        x_nudge)
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
