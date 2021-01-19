#' Nudge labels a fixed distance from points
#'
#' `position_nudge_repel` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. Nudging is built in to
#' [geom_text_repel()] because it is useful for consistently moving labels some
#' distance from what they're labeling.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`,
#' @param center_x,center_y The coordinates of the virtual origin out from which
#'   nudging radiates or splits in opposite directions. A numeric vector of
#'   length 1 or of the same length as rows there are in `data`, or a function
#'   returning either of these vectors computed from the variable in data
#'   mapped to `x` or `y`, respectively.
#' @param direction One of "none", "radial", or "split". Which of these three
#'   values is the default depends on the values passed to the other parameters.
#'
#' @details Positive values as arguments to `x` and `y` are added to the
#'   original position along either axis. If no arguments are passed to
#'   `center_x`, `center_y` or `direction`, the nudging is applied as is, as is
#'   the case if `direction = "none"`. If non-`NULL` arguments are passed to
#'   both `center_x` and `center_y`, `direction = "radial"` is assumed. In this
#'   case, if `x` and/or `y` positive nudging is applied radially outwards from
#'   the center, while if negative, inwards towards the center. When a
#'   non-`NULL` argument is passed only to one of `center_x` or `center_y`,
#'   `direction = "split"` is assumed. In this case when the initial location of
#'   the point is to the left of `center_x`, `-x` is used instead of `x` for
#'   nudging, and when the initial location of the point is to the below of
#'   `center_y`, `-y` is used instead of `y` for nudging. If non-`NULL` arguments
#'   are passed to both `center_x` and `center_y`, and `direction` is passed
#'   `"split"` as argument, then the split as described above is applied to
#'   both _x_ and _y_ coordinates.
#'
#' @note Some situations are handled as special cases. When `direction = "split"`
#'   or `direction = "radial"`, observations at exactly the _center_ are nudged
#'   using `x` and `y` unchanged. When `direction = "split"`, and both
#'   `center_x` and `center_y` have been supplied, segments are drawn at eight
#'   different possible angles. When segments are
#'   exactly horizontal or vertical they would be shorter than when drawn at
#'   the other four angles, in which case `x` or `y` are extended to
#'   ensure these segments are of the same lengths as those at other angles.
#'
#'   This position is most useful when labelling points forming a cloud or
#'   along vertical or horizontal lines or "divides".
#'
#' @export
#'
#' @examples
#' # Plain nudging
#' df <- data.frame(
#'   x = c(1,3,2,5),
#'   y = c("a","c","d","c")
#' )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(
#'     aes(label = y),
#'     min.segment.length = 0,
#'     position = position_nudge_repel(x = 0.1, y = 0.15)
#'   )
#'
#' # The values for x and y can be vectors
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   nudge_x = 0.1,
#'                   nudge_y = 0.15)
#'
#' # "split" nudging
#'
#' df <- data.frame(
#'   x = c(1,3,2,5,4,2.5),
#'   y = c("a","c","d","c","b","a")
#' )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.12,
#'                                                   y = 0.2,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.12,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(y = 0.2,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.12,
#'                                                   y = 0.2,
#'                                                   center_y = 2,
#'                                                   center_x = 1.5,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.12,
#'                                                   y = 0.2,
#'                                                   center_y = 2))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.2,
#'                                                   center_x = 2.5)) +
#'   scale_x_continuous(expand = expansion(mult = 0.1))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.12,
#'                                                   y = 0.12,
#'                                                   center_x = median,
#'                                                   center_y = median,
#'                                                   direction = "split"))
#'
#' # "Radial" nudging
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.12,
#'                                                   y = 0.2,
#'                                                   direction = "radial"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = -0.12,
#'                                                   y = -0.2,
#'                                                   direction = "radial"))
#'
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
#'                   position = position_nudge_repel(x = -1,
#'                                                   y = -6,
#'                                                   center_x = mean,
#'                                                   center_y = max)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 1,
#'                                                   y = 6,
#'                                                   center_x = mean,
#'                                                   center_y = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
#' max_and_tenth <- function(x) {1.1 * max(x)}
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 1,
#'                                                   y = 6,
#'                                                   center_x = mean,
#'                                                   center_y = max_and_tenth)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
position_nudge_repel <-
  function(x = 0,
           y = 0,
           center_x = NULL,
           center_y = NULL,
           direction = NULL) {

    if (is.null(direction)) {
      # Set default for 'direction' based on other arguments
      if (is.null(center_x) && is.null(center_y)) {
        direction <- "none"
      } else if (xor(is.null(center_x), is.null(center_y))) {
        direction <- "split"
      } else {
        direction <- "radial"
      }
    }

    if (direction != "none") {
      # Set center if is missing and direction requires it
      if (is.null(center_x)) {
        center_x <- mean
      }
      if (is.null(center_y)) {
        center_y <- mean
      }
    }

    ggproto(NULL, PositionNudgeRepel,
            x = x,
            y = y,
            center_x = center_x,
            center_y = center_y,
            direction = direction
    )
  }

#' @rdname ggrepel-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeRepel <- ggproto("PositionNudgeRepel", Position,
  x = 0,
  y = 0,
  center_x = mean,
  center_y = mean,
  direction = "none",

  setup_params = function(self, data) {
    if (is.function(self$center_x)) {
      x_ctr <- self$center_x(data$x)
    } else if(is.numeric(self$center_x)) {
      x_ctr <- self$center_x[1]
    } else {
      x_ctr <- -Inf # ensure all observations are to the right
    }
    if (is.function(self$center_y)) {
      y_ctr <- self$center_y(data$y)
    } else if(is.numeric(self$center_y)) {
      y_ctr <- self$center_y[1]
    } else {
      y_ctr <- -Inf # ensure all observations are above
    }

    list(x = self$x,
         y = self$y,
         x_ctr = x_ctr,
         y_ctr = y_ctr,
         direction = self$direction)
  },

  compute_layer = function(self, data, params, layout) {
    x_orig <- data$x
    y_orig <- data$y
    # Based on the value of 'direction' we adjust the nudge for each point
    if (params$direction == "radial") {
      # compute x and y nudge for each point
      x_dist <- as.numeric(data$x - params$x_ctr)
      y_dist <- as.numeric(data$y - params$y_ctr)
      angle <- ifelse(y_dist == 0 & x_dist == 0,
                      atan2(params$y, params$x),
                      atan2(y_dist, x_dist))
      x_nudge <- params$x * cos(angle)
      y_nudge <- params$y * sin(angle)
    } else if (params$direction == "split") {
      if (length(self$x) == 1L && length(self$y) == 1L) {
        # ensure horizontal and vertical segments have same length as all others
        segment_length <- sqrt(self$x^2 + self$y^2)
        xx <- rep(self$x, nrow(data))
        xx <- ifelse(data$y == params$y_ctr, segment_length * sign(xx), xx)
        yy <- rep(self$y, nrow(data))
        yy <- ifelse(data$x == params$x_ctr, segment_length * sign(yy), yy)
      }
      x_nudge <- xx * sign(data$x - params$x_ctr)
      y_nudge <- yy * sign(data$y - params$y_ctr)
    } else {
      if (params$direction != "none") {
        warning("Ignoring unrecognized direction \"", direction, "\".")
      }
      x_nudge <- params$x
      y_nudge <- params$y
    }
    # transform only the dimensions for which non-zero nudging is requested
    ## Does this speed up execution enough to be worthwhile avoiding + 0 operations??
    if (any(x_nudge != 0)) {
      if (any(y_nudge != 0)) {
        data <- transform_position(data,
                                   trans_x = function(x) x + x_nudge,
                                   trans_y = function(y) y + y_nudge)
      } else {
        data <- transform_position(data,
                                   trans_x = function(x) x + x_nudge,
                                   trans_y = NULL)
      }
    } else if (any(y_nudge != 0)) {
      data <- transform_position(data,
                                 trans_x = NULL,
                                 trans_y = function(y) y + y_nudge)
    }
    data$nudge_x <- data$x
    data$nudge_y <- data$y
    data$x <- x_orig
    data$y <- y_orig
    data
  }
)
