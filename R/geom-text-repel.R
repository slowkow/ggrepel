#' Repulsive textual annotations.
#'
#' \code{geom_text_repel} adds text directly to the plot.
#' \code{geom_label_repel} draws a rectangle underneath the text, making it
#' easier to read. The text labels repel away from each other and away from
#' the data points.
#'
#' These geoms are based on \code{\link[ggplot2]{geom_text}} and
#' \code{\link[ggplot2]{geom_label}}. See the documentation for those
#' functions for more details. Differences from those functions are noted
#' here.
#'
#' Text labels have height and width, but they are physical units, not data
#' units. The amount of space they occupy on that plot is not constant in data
#' units: when you resize a plot, labels stay the same size, but the size of
#' the axes changes. The text labels are repositioned after resizing a plot.
#'
#' @section \code{geom_label_repel}:
#' Currently \code{geom_label_repel} does not support the \code{rot} argument
#' and is considerably slower than \code{geom_text_repel}. The \code{fill}
#' aesthetic controls the background colour of the label.
#'
#' @section Alignment with \code{hjust} or \code{vjust}:
#' The arguments \code{hjust} and \code{vjust} are supported, but they only
#' control the initial positioning, so repulsive forces may disrupt alignment.
#' Alignment with \code{hjust} will be preserved if labels only move up and down
#' by using \code{direction="y"}. For \code{vjust}, use \code{direction="x"}.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}} or
#'   \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There are
#'   three types of arguments you can use here:
#'
#'   \itemize{
#'     \item Aesthetics: to set an aesthetic to a fixed value, like
#'        \code{colour = "red"} or \code{size = 3}.
#'     \item Other arguments to the layer, for example you override the
#'       default \code{stat} associated with the layer.
#'     \item Other arguments passed on to the stat.
#'   }
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label.
#' @param xlim,ylim Limits for the x and y axes. Text labels will be constrained
#'   to these limits. By default, text labels are constrained to the entire plot
#'   area.
#' @param box.padding Amount of padding around bounding box, as unit or number.
#'   Defaults to 0.25. (Default unit is lines, but other units can be specified
#'   by passing \code{unit(x, "units")}).
#' @param point.padding Amount of padding around labeled point, as unit or
#'   number. Defaults to 0. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param segment.size Width of line segment connecting the data point to
#'   the text label, in mm.
#' @param segment.colour,segment.color Colour of the line segment. Defaults to the same colour
#'   as the text. In the unlikely event you specify both US and UK spellings of colour, the
#'   US spelling will take precedence.
#' @param segment.alpha Transparency of the line segment. Defaults to the same
#'   transparency as the text.
#' @param min.segment.length Skip drawing segments shorter than this, as unit or
#'   number. Defaults to 0.5. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param segment.curvature A numeric value giving the amount of curvature.
#'   Negative values produce left-hand curves, positive values produce
#'   right-hand curves, and zero produces a straight line.
#' @param segment.angle A numeric value between 0 and 180, giving an amount to
#'   skew the control points of the curve. Values less than 90 skew the curve
#'   towards the start point and values greater than 90 skew the curve towards
#'   the end point.
#' @param segment.ncp The number of control points used to draw the curve. More
#'   control points creates a smoother curve.
#' @param arrow specification for arrow heads, as created by \code{\link[grid]{arrow}}
#' @param force Force of repulsion between overlapping text labels. Defaults
#'   to 1.
#' @param force_pull Force of attraction between a text label and its
#'   corresponding data point. Defaults to 1.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 2000.
#' @param direction "both", "x", or "y" -- direction in which to adjust position of labels
#' @param seed Random seed passed to \code{\link[base]{set.seed}}. Defaults to
#'   \code{NA}, which means that \code{set.seed} will not be called.
#'
#' @examples
#'
#' p <- ggplot(mtcars,
#'   aes(wt, mpg, label = rownames(mtcars), colour = factor(cyl))) +
#'   geom_point()
#'
#' # Avoid overlaps by repelling text labels
#' p + geom_text_repel()
#' # Labels with background
#' p + geom_label_repel()
#'
#' \dontrun{
#' p + geom_text_repel(family = "Times New Roman",
#'   box.padding = 0.5)
#'
#' # Add aesthetic mappings
#' p + geom_text_repel(aes(alpha=wt, size=mpg))
#' p + geom_label_repel(aes(fill=factor(cyl)), colour="white", segment.colour="black")
#'
#' # Draw all line segments
#' p + geom_text_repel(min.segment.length = 0)
#'
#' # Omit short line segments (default behavior)
#' p + geom_text_repel(min.segment.length = 0.5)
#'
#' # Omit all line segments
#' p + geom_text_repel(segment.colour = NA)
#'
#' # Repel just the labels and totally ignore the data points
#' p + geom_text_repel(point.padding = NA)
#'
#' # Hide some of the labels, but repel from all data points
#' mtcars$label <- rownames(mtcars)
#' mtcars$label[1:15] <- ""
#' p + geom_text_repel(data = mtcars, aes(wt, mpg, label = label))
#'
#' # Nudge the starting positions
#' p + geom_text_repel(nudge_x = ifelse(mtcars$cyl == 6, 1, 0),
#'                     nudge_y = ifelse(mtcars$cyl == 6, 8, 0))
#'
#' # Change the text size
#' p + geom_text_repel(aes(size = wt))
#' # Scale height of text, rather than sqrt(height)
#' p + geom_text_repel(aes(size = wt)) + scale_radius(range = c(3,6))
#'
#' # You can display expressions by setting parse = TRUE.  The
#' # details of the display are described in ?plotmath, but note that
#' # geom_text_repel uses strings, not expressions.
#' p + geom_text_repel(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
#'   parse = TRUE)
#'
#' # Add a text annotation
#' p +
#'   geom_text_repel() +
#'   annotate(
#'     "text", label = "plot mpg vs. wt",
#'     x = 2, y = 15, size = 8, colour = "red"
#'   )
#'
#' # Add arrows
#' p +
#'   geom_point(colour = "red") +
#'   geom_text_repel(
#'     arrow = arrow(length = unit(0.02, "npc")),
#'     box.padding = 1
#'   )
#'
#' }
#' @export
geom_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.25,
  point.padding = 1e-6,
  segment.colour = NULL,
  segment.color = NULL,
  segment.size = 0.5,
  segment.alpha = NULL,
  min.segment.length = 0.5,
  segment.curvature = 0,
  segment.angle = 90,
  segment.ncp = 1,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.1,
  max.iter = 2000,
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both","y","x"),
  seed = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge2(nudge_x, nudge_y)
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      box.padding = to_unit(box.padding),
      point.padding = to_unit(point.padding),
      segment.colour = segment.color %||% segment.colour,
      segment.size = segment.size,
      segment.alpha = segment.alpha,
      min.segment.length = to_unit(min.segment.length),
      segment.curvature = segment.curvature,
      segment.angle = segment.angle,
      segment.ncp = segment.ncp,
      arrow = arrow,
      force = force,
      force_pull = force_pull,
      max.time = max.time,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      xlim = xlim,
      ylim = ylim,
      direction = match.arg(direction),
      seed = seed,
      ...
    )
  )
}

#' GeomTextRepel
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextRepel <- ggproto("GeomTextRepel", Geom,
  required_aes = c("x", "y", "label"),

  non_missing_aes = c("point.size"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2,
    hjust = 0.5, vjust = 0.5, point.size = 1
  ),

  draw_panel = function(
    data, panel_scales, coord,
    parse = FALSE,
    na.rm = FALSE,
    box.padding = 0.25,
    point.padding = 1e-6,
    segment.colour = NULL,
    segment.size = 0.5,
    segment.alpha = NULL,
    min.segment.length = 0.5,
    segment.curvature = 0,
    segment.angle = 90,
    segment.ncp = 1,
    arrow = NULL,
    force = 1,
    force_pull = 1,
    max.time = 0.1,
    max.iter = 2000,
    nudge_x = 0,
    nudge_y = 0,
    xlim = c(NA, NA),
    ylim = c(NA, NA),
    direction = "both",
    seed = NA
  ) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }
    if (!length(which(not_empty(lab)))) {
      return()
    }

    # position_nudge2() should have added these columns.
    for (this_dim in c("x", "y")) {
      this_nudge <- sprintf("nudge_%s", this_dim)
      if (!this_nudge %in% colnames(data)) {
        data[[this_nudge]] <- data[[this_dim]]
      }
    }
    # Transform the nudges to the panel scales.
    nudges <- data.frame(x = data$nudge_x, y = data$nudge_y)
    nudges <- coord$transform(nudges, panel_scales)

    # Transform the raw data to the panel scales.
    data <- coord$transform(data, panel_scales)

    # The nudge is relative to the data.
    data$nudge_x <- nudges$x - data$x
    data$nudge_y <- nudges$y - data$y

    # Transform limits to panel scales.
    limits <- data.frame(x = xlim, y = ylim)
    limits <- coord$transform(limits, panel_scales)

    # Fill NAs with defaults.
    limits$x[is.na(limits$x)] <- c(0, 1)[is.na(limits$x)]
    limits$y[is.na(limits$y)] <- c(0, 1)[is.na(limits$y)]

    # Convert hjust and vjust to numeric if character
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    ggname("geom_text_repel", gTree(
      limits = limits,
      data = data,
      lab = lab,
      box.padding = to_unit(box.padding),
      point.padding = to_unit(point.padding),
      segment.colour = segment.colour,
      segment.size = segment.size,
      segment.alpha = segment.alpha,
      segment.curvature = segment.curvature,
      segment.angle = segment.angle,
      segment.ncp = segment.ncp,
      min.segment.length = to_unit(min.segment.length),
      arrow = arrow,
      force = force,
      force_pull = force_pull,
      max.time = max.time,
      max.iter = max.iter,
      direction = direction,
      seed = seed,
      cl = "textrepeltree"
    ))
  },

  draw_key = draw_key_text
)

#' grid::makeContent function for the grobTree of textRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.textrepeltree <- function(x) {

  # The padding around each bounding box.
  box_padding_x <- convertWidth(x$box.padding, "native", valueOnly = TRUE)
  box_padding_y <- convertHeight(x$box.padding, "native", valueOnly = TRUE)

  # The padding around each point.
  if (is.na(x$point.padding)) {
    x$point.padding = unit(0, "lines")
  }
  point_padding_x <- convertWidth(x$point.padding, "native", valueOnly = TRUE)
  point_padding_y <- convertHeight(x$point.padding, "native", valueOnly = TRUE)

  # Do not create text labels for empty strings.
  valid_strings <- which(not_empty(x$lab))
  invalid_strings <- which(!not_empty(x$lab))
  ix <- c(valid_strings, invalid_strings)
  x$data <- x$data[ix,]
  x$lab <- x$lab[ix]

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(seq_along(valid_strings), function(i) {
    row <- x$data[i, , drop = FALSE]
    tg <- textGrob(
      x$lab[i],
      row$x, row$y, default.units = "native",
      rot = row$angle,
      gp = gpar(
        fontsize   = row$size * .pt,
        fontfamily = row$family,
        fontface   = row$fontface,
        lineheight = row$lineheight
      )
    )
    gw <- convertWidth(grobWidth(tg), "native", TRUE)
    gh <- convertHeight(grobHeight(tg), "native", TRUE)
    c(
      "x1" = row$x - gw *       row$hjust - box_padding_x + row$nudge_x,
      "y1" = row$y - gh *       row$vjust - box_padding_y + row$nudge_y,
      "x2" = row$x + gw * (1 - row$hjust) + box_padding_x + row$nudge_x,
      "y2" = row$y + gh * (1 - row$vjust) + box_padding_y + row$nudge_y 
    )
  })

  # Make the repulsion reproducible if desired.
  if (is.null(x$seed) || !is.na(x$seed)) {
      set.seed(x$seed)
  }

  # The points are represented by circles.
  point_size <- convertWidth(to_unit(x$data$point.size), "native", valueOnly = TRUE) / 10

  # browser()
  # Repel overlapping bounding boxes away from each other.
  repel <- repel_boxes2(
    data_points     = as.matrix(x$data[,c("x","y")]),
    point_size      = point_size,
    point_padding_x = point_padding_x,
    point_padding_y = point_padding_y,
    boxes           = do.call(rbind, boxes),
    xlim            = range(x$limits$x),
    ylim            = range(x$limits$y),
    hjust           = x$data$hjust %||% 0.5,
    vjust           = x$data$vjust %||% 0.5,
    force_push      = x$force * 1e-6,
    force_pull      = x$force_pull * 1e-2,
    max_time        = x$max.time,
    max_iter        = x$max.iter,
    direction       = x$direction
  )

  grobs <- lapply(seq_along(valid_strings), function(i) {
    row <- x$data[i, , drop = FALSE]
    # browser()
    makeTextRepelGrobs(
      i,
      x$lab[i],
      # Position of text bounding boxes.
      x = unit(repel$x[i], "native"),
      y = unit(repel$y[i], "native"),
      # Position of original data points.
      x.orig = unit(row$x, "native"),
      y.orig = unit(row$y, "native"),
      rot = row$angle,
      box.padding = x$box.padding,
      point.size = point_size[i],
      point.padding = x$point.padding,
      segment.curvature = x$segment.curvature,
      segment.angle = x$segment.angle,
      segment.ncp = x$segment.ncp,
      text.gp = gpar(
        col = scales::alpha(row$colour, row$alpha),
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      ),
      segment.gp = gpar(
        col = scales::alpha(x$segment.colour %||% row$colour, x$segment.alpha %||% row$alpha),
        lwd = x$segment.size * .pt
      ),
      arrow = x$arrow,
      min.segment.length = x$min.segment.length,
      hjust = row$hjust,
      vjust = row$vjust
    )
  })
  # Put segment grobs before text grobs.
  grobs <- c(
    Filter(Negate(is.null), lapply(grobs, "[[", "segment")),
    Filter(Negate(is.null), lapply(grobs, "[[", "text"))
  )
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

makeTextRepelGrobs <- function(
  i,
  label,
  # Position of text bounding boxes.
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  # Position of original data points.
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  rot = 0,
  default.units = "npc",
  just = "center",
  box.padding = 0.25,
  point.size = 1,
  point.padding = 1e-6,
  segment.curvature = 0,
  segment.angle = 90,
  segment.ncp = 1,
  name = NULL,
  text.gp = gpar(),
  segment.gp = gpar(),
  vp = NULL,
  arrow = NULL,
  min.segment.length = 0.5,
  hjust = 0.5,
  vjust = 0.5
) {
  stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  hj <- resolveHJust(just, NULL)
  vj <- resolveVJust(just, NULL)

  t <- textGrob(
    label,
    x + 2 * (0.5 - hj) * box.padding,
    y + 2 * (0.5 - vj) * box.padding,
    rot = rot,
    just = c(hj, vj),
    gp = text.gp,
    name = sprintf("textrepelgrob%s", i)
  )

  x1 <- convertWidth(x - 0.5 * grobWidth(t), "native", TRUE)
  x2 <- convertWidth(x + 0.5 * grobWidth(t), "native", TRUE)
  y1 <- convertHeight(y - 0.5 * grobHeight(t), "native", TRUE)
  y2 <- convertHeight(y + 0.5 * grobHeight(t), "native", TRUE)

  point_pos <- c(
    convertWidth(x.orig, "native", TRUE),
    convertHeight(y.orig, "native", TRUE)
  )

  # Get the coordinates of the intersection between the line from the
  # original data point to the centroid and the rectangle's edges.
  extra_padding_x <- convertWidth(unit(0.25, "lines"), "native", TRUE) / 2
  extra_padding_y <- convertHeight(unit(0.25, "lines"), "native", TRUE) / 2
  text_box <- c(
    x1 - extra_padding_x, y1 - extra_padding_y,
    x2 + extra_padding_x, y2 + extra_padding_y
  )
  #int <- intersect_line_rectangle(point_pos, center, text_box)
  int <- select_line_connection(point_pos, text_box)

  # Check if the data point is inside the label box.
  point_inside <- FALSE
  if (text_box[1] <= point_pos[1] && point_pos[1] <= text_box[3] &&
      text_box[2] <= point_pos[2] && point_pos[2] <= text_box[4]) {
    point_inside <- TRUE
  }

  # # Nudge the original data point toward the label with point.padding.
  # point_padding_x <- convertWidth(point.padding, "native", TRUE) / 2
  # point_padding_y <- convertHeight(point.padding, "native", TRUE) / 2
  # point_padding <- point_padding_x > 0 & point_padding_y > 0
  # if (point_padding) {
  #   point_box <- c(
  #     point_pos[1] - point_padding_x, point_pos[2] - point_padding_y,
  #     point_pos[1] + point_padding_x, point_pos[2] + point_padding_y
  #   )
  #   point_pos <- intersect_line_rectangle(center, point_pos, point_box)
  # }

# {{ Trying out some new point padding code.
#   d1x <- abs(int[1] - point_pos[1])
#   d1y <- abs(int[2] - point_pos[2])
#   d1 <- sqrt(d1x * d1x + d1y * d1y)
#   if (d1 > 0) {
#     new_pos <- c(
#       # point_pos[1] - (as.numeric(point.size) / 10 + as.numeric(point.padding) / 10) * (dx / d1),
#       # point_pos[2] - (as.numeric(point.size) / 10 + as.numeric(point.padding) / 10) * (dy / d1)
#       # This one is pretty good!
#       # point_pos[1] + sign(int[1] - point_pos[1]) * 0.5 * d1 * (d1x / d1),
#       # point_pos[2] + sign(int[2] - point_pos[2]) * 0.5 * d1 * (d1y / d1)
#       # This is ok.
#       # point_pos[1] + sign(int[1] - point_pos[1]) * as.numeric(point.padding) * d1 * (d1x / d1),
#       # point_pos[2] + sign(int[2] - point_pos[2]) * as.numeric(point.padding) * d1 * (d1y / d1)
#       # This seems like the best?
#       point_pos[1] + 0.8 * sign(int[1] - point_pos[1]) * as.numeric(point.padding) * as.numeric(point.size) * (d1x / d1),
#       point_pos[2] + 0.8 * sign(int[2] - point_pos[2]) * as.numeric(point.padding) * as.numeric(point.size) * (d1y / d1)
#     )
#     d2x <- abs(int[1] - new_pos[1])
#     d2y <- abs(int[2] - new_pos[2])
#     d2 <- sqrt(d2x * d2x + d2y * d2y)
#     signs_match <- (
#       sign(int[1] - new_pos[1]) == sign(int[1] - point_pos[1]) &&
#       sign(int[2] - new_pos[2]) == sign(int[2] - point_pos[2])
#     )
#     if (d2 < d1 && signs_match) {
#       point_pos <- new_pos
#     }
#   }
# }}

  # Nudge the original data point toward the label with point.padding.
  point_padding_x <- convertWidth(point.padding, "native", TRUE) / 2
  point_padding_y <- convertHeight(point.padding, "native", TRUE) / 2
  point_padding <- point_padding_x > 0 & point_padding_y > 0
  if (point_padding) {
    point_box <- c(
      point_pos[1] - point_padding_x, point_pos[2] - point_padding_y,
      point_pos[1] + point_padding_x, point_pos[2] + point_padding_y
    )
    point_pos <- intersect_line_rectangle(int, point_pos, point_box)
  }

  # Compute the distance between the data point and the edge of the text box.
  dx <- abs(int[1] - point_pos[1])
  dy <- abs(int[2] - point_pos[2])
  d <- sqrt(dx * dx + dy * dy)
  # Scale the unit vector by the minimum segment length.
  if (d > 0) {
    mx <- convertWidth(min.segment.length, "native", TRUE)
    my <- convertHeight(min.segment.length, "native", TRUE)
    min.segment.length <- sqrt((mx * dx / d) ^ 2 + (my * dy / d) ^ 2)
  }

  grobs <- list(text = t)

  if (!point_inside && d > 0 && euclid(int, point_pos) > min.segment.length) {
    s <- curveGrob(
      x1 = int[1],
      y1 = int[2],
      x2 = point_pos[1],
      y2 = point_pos[2],
      default.units = "native",
      curvature = segment.curvature,
      angle = segment.angle,
      ncp = segment.ncp,
      gp = segment.gp,
      name = sprintf("segmentrepelgrob%s", i),
      arrow = arrow
    )
    grobs[["segment"]] <- s
  }

  grobs
}

# copied from ggplot2
compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

# copied from ggplot2
just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
