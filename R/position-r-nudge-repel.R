#' Nudge labels a fixed distance from points
#'
#' `position_r_nudge_repel` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' [position_nudge_repel()] is that the nudging is radial and away from a focus
#' point, which by default is the centroid of the data as computed using
#' [base::mean()]. A different function or a numeric vector can be used to
#' override this default.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`,
#' @param x_center,y_center The coordinates of the virtual origin out from which
#'   nudging radiates, a numeric vector of length 1 or of the same length as
#'   rows there are in `data`, or a function returning either of these computed
#'   from the variable in data.
#'
#' @note Positive values as arguments to `x` and `y` nudge
#'   radialy outwards from the centre, while negative values nudge inwards
#'   towards the center. The segment length equal to `y` for vertical
#'   segments and equal to `x` for horizontal segments. The length of those
#'   at intermediate angles varies smoothly between these two maximum and
#'   minimum lengths.
#' @export
#' @examples
#' df <- data.frame(
#'   x = c(1,3,2,5),
#'   z = c(4,8,5,6),
#'   y = c("a","c","d","c")
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
#'                   position = position_r_nudge_repel(x = 0.3, y = 0.3)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_r_nudge_repel(x = 0.3,
#'                                                     y = 0.6,
#'                                                     x_center = min,
#'                                                     y_center = min)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_r_nudge_repel(x = 0.5,
#'                                                     y = 0.5,
#'                                                     x_center = 2.5,
#'                                                     y_center = 6)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
#' ggplot(df, aes(x, z)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_r_nudge_repel(x = 0.3, y = 0.6,
#'                                                     x_center = 3,
#'                                                     y_center = 6)) +
#'   scale_x_continuous(expand = expansion(mult = 0.15)) +
#'   scale_y_continuous(expand = expansion(mult = 0.15))
#'
position_r_nudge_repel <- function(x = 0,
                                   y = 0,
                                   x_center = mean,
                                   y_center = mean) {
  ggproto(NULL, PositionRNudgeRepel,
    x = x,
    y = y,
    x_center = x_center,
    y_center = y_center
  )
}

#' @rdname ggrepel-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionRNudgeRepel <- ggproto("PositionRNudgeRepel", Position,
  x = 0,
  y = 0,
  x_center = mean,
  y_center = mean,

  setup_params = function(self, data) {
    if (is.function(self$x_center)) {
      x_ctr <- self$x_center(data$x)
    } else if(is.numeric(self$x_center)) {
      x_ctr <- self$x_center[1]
    }
    if (is.function(self$y_center)) {
      y_ctr <- self$y_center(data$y)
    } else if(is.numeric(self$y_center)) {
      y_ctr <- self$y_center[1]
    }

#    if (length(self$x) == 1L && length(self$y) == 1L) {
      # Compute tan from x and y
      segment_length <- sqrt(self$x^2 + self$y^2)
      print(segment_length)
#    }

    list(x = self$x,
         y = self$y,
         x_ctr = x_ctr,
         y_ctr = y_ctr,
         segment_length = segment_length)
  },

  compute_layer = function(self, data, params, layout) {
    x_orig <- data$x
    y_orig <- data$y
    # compute x and y nudge for each point
    x_dist <- as.numeric(data$x - params$x_ctr)
    y_dist <- as.numeric(data$y - params$y_ctr)
    angle <- ifelse(y_dist == 0 & x_dist == 0,
                    atan2(params$y, params$x),
                    atan2(y_dist, x_dist))
    x_nudge <- params$x * cos(angle)
    y_nudge <- params$y * sin(angle)
    print(x_nudge)
    print(y_nudge)
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
