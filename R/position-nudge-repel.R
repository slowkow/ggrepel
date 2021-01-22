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
#' @param direction One of "none", "radial", or "split". A value of "none"
#'   replicates the behavior of [ggplot2::position_nudge]. Which of these three
#'   values is the default depends on the values passed to the other parameters.
#' @param obey_grouping A logical flag indicating whether to obey or not groupings
#'   of the observations. By default, grouping is obeyed when both of the
#'   variables mapped to _x_ and _y_ are continuous numeric and ignored
#'   otherwise.
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
#'   This position function is implemented as a panel function and supports
#'   grouping: by default and when `center_x` or `center_y` are passed a
#'   function, the functions are applied by default per group wihin each
#'   plot panel.
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
#' @seealso [ggplot::position_nudge()], [ggrepel::position_nudge_repel()].
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
#'     position = position_nudge_repel(x = 0.075, y = 0.1)
#'   )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   nudge_x = 0.075,
#'                   nudge_y = 0.1)
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
#'                   position = position_nudge_repel(x = 0.06,
#'                                                   y = 0.1,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.1,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(y = 0.12,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.06,
#'                                                   y = 0.1,
#'                                                   center_y = 2,
#'                                                   center_x = 1.5,
#'                                                   direction = "split"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.06,
#'                                                   y = 0.1,
#'                                                   center_y = 2))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.12,
#'                                                   center_x = 2.5)) +
#'   scale_x_continuous(expand = expansion(mult = 0.1))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 0.06,
#'                                                   y = 0.1,
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
#'                   position = position_nudge_repel(x = 0.07,
#'                                                   y = 0.12,
#'                                                   direction = "radial"))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = -0.07,
#'                                                   y = -0.12,
#'                                                   direction = "radial"))
#'
#' # this position function is compatible with "normal" ggplot2 geoms
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text(aes(label = y),
#'             position = position_nudge_repel(x = 0.07,
#'                                             y = 0.12,
#'                                             direction = "radial"))
#'
#' # other use cases for "radial" nudging
#'
#' df <- data.frame(
#'   x = -10:10,
#'   z = (-10:10)^2,
#'   y = letters[1:21]
#' )
#'
#' ggplot(df, aes(x, z)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = -1,
#'                                                   y = -5,
#'                                                   center_x = mean,
#'                                                   center_y = max)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, z)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 1,
#'                                                   y = 5,
#'                                                   center_x = mean,
#'                                                   center_y = max)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
#' max_and_tenth <- function(x) {1.1 * max(x)}
#' ggplot(df, aes(x, z)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_nudge_repel(x = 1,
#'                                                   y = 5,
#'                                                   center_x = mean,
#'                                                   center_y = max_and_tenth)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
#' # this position function is compatible with "normal" ggplot2 geoms
#'
#' max_and_tenth <- function(x) {1.1 * max(x)}
#' ggplot(df, aes(x, z)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(aes(label = y),
#'             position = position_nudge_repel(x = 0.8,
#'                                             y = 4,
#'                                             center_x = mean,
#'                                             center_y = max_and_tenth)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
position_nudge_repel <-
  function(x = 0,
           y = 0,
           center_x = NULL,
           center_y = NULL,
           direction = NULL,
           obey_grouping = NULL) {

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

    if (is.null(obey_grouping)) {
      # default needs to be set in panel_fucntion when we have access to data
      obey_grouping <- NA
    }

    ggproto(NULL, PositionNudgeRepel,
            x = x,
            y = y,
            center_x = center_x,
            center_y = center_y,
            direction = direction,
            obey_grouping = obey_grouping
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
  obey_grouping = NA,

  setup_params = function(self, data) {

    list(x = self$x,
         y = self$y,
         center_x = self$center_x,
         center_y = self$center_y,
         direction = self$direction,
         obey_grouping = self$obey_grouping)
  },

  compute_panel = function(data, params, scales) {

    x_orig <- data$x
    y_orig <- data$y
    # we handle grouping by ourselves
    if (is.na(params$obey_grouping)) {
      if (inherits(data$x, "mapped_discrete") ||
          inherits(data$y, "mapped_discrete") ||
          params$direction == "none") {
        # we ignore grouping as position_nudge() does
        params$obey_grouping <- FALSE
      } else {
        # we respect groups
        params$obey_grouping <- TRUE
      }
    }

    if (params$obey_grouping) {
      # one group at a time
      groups <- unique(data$group)
    } else {
      # all at once
      groups <- 1
    }
    # Based on the value of 'direction' we adjust the nudge for each point
    x_nudge <- y_nudge <- numeric(nrow(data))
    for (group in groups) {
      if (params$obey_grouping) {
        # selector for rows in current group
        in.grp <- data$group == group
      } else {
        # selector for all rows
        in.grp <- TRUE
      }
      # compute focal center by group
      if (is.function(params$center_x)) {
        x_ctr <- params$center_x(as.numeric(data[in.grp, "x"]))
      } else if(is.numeric(params$center_x)) {
        x_ctr <- params$center_x[1]
      } else {
        x_ctr <- -Inf # ensure all observations are to the right
      }
      if (is.function(params$center_y)) {
        y_ctr <- params$center_y(as.numeric(data[in.grp, "y"]))
      } else if(is.numeric(params$center_y)) {
        y_ctr <- params$center_y[1]
      } else {
        y_ctr <- -Inf # ensure all observations are above
      }

      if (params$direction == "radial") {
        # compute x and y nudge for each point
        x_dist <- as.numeric(data[in.grp, "x"]) - x_ctr
        y_dist <- as.numeric(data[in.grp, "y"]) - y_ctr
        angle <- atan2(y_dist, x_dist) + pi / 2
        if (params$x == 0) {
          angle <- ifelse(cos(angle) == 0, 0, angle)
        }
        if (params$y == 0) {
          angle <- ifelse(sin(angle) == 0, pi / 2, angle)
        }
        x_nudge[in.grp] <- params$x * sin(angle)
        y_nudge[in.grp] <- -params$y * cos(angle)
      } else if (params$direction == "split") {
        if (length(params$x) == 1L && length(params$y) == 1L) {
          # ensure horizontal and vertical segments have same length as others
          segment_length <- sqrt(params$x^2 + params$y^2)
          xx <- rep(params$x, nrow(data[in.grp, ]))
          xx <- ifelse(data[in.grp, "y"] == y_ctr, segment_length * sign(xx), xx)
          yy <- rep(params$y, nrow(data[in.grp, ]))
          yy <- ifelse(data[in.grp, "x"] == x_ctr, segment_length * sign(yy), yy)
        }
        x_nudge[in.grp] <- xx * sign(as.numeric(data[in.grp, "x"]) - x_ctr)
        y_nudge[in.grp] <- yy * sign(as.numeric(data[in.grp, "y"]) - y_ctr)
      } else {
        if (params$direction != "none") {
          warning("Ignoring unrecognized direction \"", direction, "\".")
        }
        x_nudge[in.grp] <- params$x
        y_nudge[in.grp] <- params$y
      }
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
    data$x_orig <- x_orig
    data$y_orig <- y_orig
    data
  }
)
