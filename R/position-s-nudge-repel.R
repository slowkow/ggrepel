#' Nudge labels a fixed distance from points
#'
#' `position_s_nudge_repel` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' [position_nudge_repel()] is that the sign of the applied nudge depends
#' on which side of a divide a given point is. By default the split as at
#' the centroid of the data as computed using [base::mean()]. A different
#' function or a numeric vector can be used to override this default.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`,
#' @param x_split,y_split Divides at which the sign of nudge values changes, a
#'   numeric vector of length 1 or of the same length as rows there are in
#'   `data`, or a function returning either of these computed from the variable
#'   in data.
#' @export
#' @examples
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
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_s_nudge_repel(x = 0.12, y = 0.12))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_s_nudge_repel(x = -0.12,
#'                                                     y = 0.12,
#'                                                     x_split = 1.5))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = y),
#'                   min.segment.length = 0,
#'                   position = position_s_nudge_repel(x = 0.2,
#'                                                     x_split = 2.5)) +
#'   scale_x_continuous(expand = expansion(mult = 0.1))
#'
position_s_nudge_repel <- function(x = 0,
                                 y = 0,
                                 x_split = mean,
                                 y_split = mean) {
  ggproto(NULL, PositionSNudgeRepel,
    x = x,
    y = y,
    x_split = x_split,
    y_split = y_split
  )
}

#' @rdname ggrepel-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionSNudgeRepel <- ggproto("PositionSNudgeRepel", Position,
  x = 0,
  y = 0,
  x_split = mean,
  y_split = mean,

  setup_params = function(self, data) {
    if (is.function(self$x_split)) {
      x_splt <- self$x_split(data$x)
    } else if(is.numeric(self$x_split)) {
      x_splt <- self$x_split[1]
    }
    if (is.function(self$y_split)) {
      y_splt <- self$y_split(data$y)
    } else if(is.numeric(self$y_split)) {
      y_splt <- self$y_split[1]
    }

    if (length(self$x) == 1L && length(self$y) == 1L) {
      # ensure horizontal and vertical segments have same length as all others
      segment_length <- sqrt(self$x^2 + self$y^2)
      xx <- rep(self$x, nrow(data))
      xx <- ifelse(data$y == y_splt, segment_length * sign(xx), xx)
      yy <- rep(self$y, nrow(data))
      yy <- ifelse(data$x == x_splt, segment_length * sign(yy), yy)
    }

    list(x = xx,
         y = yy,
         x_splt = x_splt,
         y_splt = y_splt)
  },

  compute_layer = function(self, data, params, layout) {
    x_orig <- data$x
    y_orig <- data$y
    # transform only the dimensions for which non-zero nudging is requested
    if (any(params$x != 0)) {
      if (any(params$y != 0)) {
        data <- transform_position(data,
                                   function(x) x + params$x * sign(x - params$x_splt),
                                   function(y) y + params$y * sign(y - params$y_splt))
      } else {
        data <- transform_position(data,
                                   function(x) x + params$x * sign(x - params$x_splt),
                                   NULL)
      }
    } else if (any(params$y != 0)) {
      data <- transform_position(data,
                                 NULL,
                                 function(y) y + params$y * sign(y - params$y_splt))
    }
    data$nudge_x <- data$x
    data$nudge_y <- data$y
    data$x <- x_orig
    data$y <- y_orig
    data
  }
)
