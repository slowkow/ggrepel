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
#' Currently \code{geom_label_repel} does not support the \code{rot} parameter
#' and is considerably slower than \code{geom_text_repel}. The \code{fill}
#' aesthetic controls the background colour of the label.
#'
#' @section Alignment:
#' The repulsive geoms reposition text labels to avoid overlap, so the
#' following parameters are \strong{not supported}:
#'
#' \itemize{
#'   \item \code{hjust}
#'   \item \code{vjust}
#'   \item \code{position}
#'   \item \code{check_overlap}
#' }
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
#' @param arrow specification for arrow heads, as created by \code{\link[grid]{arrow}}
#' @param force Force of repulsion between overlapping text labels. Defaults
#'   to 1.
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
  mapping = NULL, data = NULL, stat = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.25,
  point.padding = 1e-6,
  segment.colour = NULL,
  segment.color = NULL,
  segment.size = 0.5,
  segment.alpha = NULL,
  min.segment.length = 0.5,
  arrow = NULL,
  force = 1,
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
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextRepel,
    position = "identity",
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
      arrow = arrow,
      force = force,
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

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2
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
    arrow = NULL,
    force = 1,
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
      lab <- parse(text = as.character(lab))
    }
    if (!length(which(not_empty(lab)))) {
      return()
    }

    # Transform the nudges to the panel scales.
    nudges <- data.frame(
      x = data$x + nudge_x,
      y = data$y + nudge_y
    )
    nudges <- coord$transform(nudges, panel_scales)

    # Transform the raw data to the panel scales.
    data <- coord$transform(data, panel_scales)

    # The nudge is relative to the data.
    nudges$x <- nudges$x - data$x
    nudges$y <- nudges$y - data$y

    # Transform limits to panel scales.
    limits <- data.frame(x = xlim, y = ylim)
    limits <- coord$transform(limits, panel_scales)

    # Fill NAs with defaults.
    limits$x[is.na(limits$x)] <- c(0, 1)[is.na(limits$x)]
    limits$y[is.na(limits$y)] <- c(0, 1)[is.na(limits$y)]

    ggname("geom_text_repel", gTree(
      limits = limits,
      data = data,
      lab = lab,
      nudges = nudges,
      box.padding = to_unit(box.padding),
      point.padding = to_unit(point.padding),
      segment.colour = segment.colour,
      segment.size = segment.size,
      segment.alpha = segment.alpha,
      min.segment.length = to_unit(min.segment.length),
      arrow = arrow,
      force = force,
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

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(valid_strings, function(i) {
    row <- x$data[i, , drop = FALSE]
    tg <- textGrob(
      x$lab[i],
      row$x, row$y, default.units = "native",
      rot = row$angle,
      gp = gpar(
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      )
    )
    gw <- convertWidth(grobWidth(tg), "native", TRUE) / 2
    gh <- convertHeight(grobHeight(tg), "native", TRUE) / 2
    c(
      "x1" = row$x - gw - box_padding_x + x$nudges$x[i],
      "y1" = row$y - gh - box_padding_y + x$nudges$y[i],
      "x2" = row$x + gw + box_padding_x + x$nudges$x[i],
      "y2" = row$y + gh + box_padding_y + x$nudges$y[i]
    )
  })

  # Make the repulsion reproducible if desired.
  if (is.null(x$seed) || !is.na(x$seed)) {
      set.seed(x$seed)
  }

  # Repel overlapping bounding boxes away from each other.
  repel <- repel_boxes(
    data_points = cbind(x$data$x, x$data$y),
    point_padding_x = point_padding_x,
    point_padding_y = point_padding_y,
    boxes = do.call(rbind, boxes),
    xlim = range(x$limits$x),
    ylim = range(x$limits$y),
    force = x$force * 1e-6,
    maxiter = x$max.iter,
    direction = x$direction
  )

  grobs <- lapply(seq_along(valid_strings), function(i) {
    xi <- valid_strings[i]
    row <- x$data[xi, , drop = FALSE]
    # browser()
    textRepelGrob(
      x$lab[xi],
      # Position of text bounding boxes.
      x = unit(repel$x[i], "native"),
      y = unit(repel$y[i], "native"),
      # Position of original data points.
      x.orig = unit(x$data$x[xi], "native"),
      y.orig = unit(x$data$y[xi], "native"),
      rot = row$angle,
      box.padding = x$box.padding,
      point.padding = x$point.padding,
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
      min.segment.length = x$min.segment.length
    )
  })
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

textRepelGrob <- function(
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
  point.padding = 1e-6,
  name = NULL,
  text.gp = gpar(),
  segment.gp = gpar(),
  vp = NULL,
  arrow = NULL,
  min.segment.length = 0.5
) {

  stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(
    label = label,
    # Position of text bounding boxes.
    x = x,
    y = y,
    # Position of original data points.
    x.orig = x.orig,
    y.orig = y.orig,
    rot = rot,
    just = just,
    box.padding = box.padding,
    point.padding = point.padding,
    name = name,
    text.gp = text.gp,
    segment.gp = segment.gp,
    vp = vp,
    cl = "textrepelgrob",
    arrow = arrow,
    min.segment.length = min.segment.length
  )
}

#' grid::makeContent function for textRepelGrob.
#'
#' @param x A grid grob.
#' @export
#' @noRd
makeContent.textrepelgrob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)

  t <- textGrob(
    x$label,
    x$x + 2 * (0.5 - hj) * x$box.padding,
    x$y + 2 * (0.5 - vj) * x$box.padding,
    rot = x$rot,
    just = c(hj, vj),
    gp = x$text.gp,
    name = "text"
  )

  x1 <- convertWidth(x$x - 0.5 * grobWidth(t), "native", TRUE)
  x2 <- convertWidth(x$x + 0.5 * grobWidth(t), "native", TRUE)
  y1 <- convertHeight(x$y - 0.5 * grobHeight(t), "native", TRUE)
  y2 <- convertHeight(x$y + 0.5 * grobHeight(t), "native", TRUE)

  point_pos <- c(
    convertWidth(x$x.orig, "native", TRUE),
    convertHeight(x$y.orig, "native", TRUE)
  )

  center <- centroid(c(x1, y1, x2, y2))

  # Get the coordinates of the intersection between the line from the
  # original data point to the centroid and the rectangle's edges.
  extra_padding_x <- convertWidth(unit(0.25, "lines"), "native", TRUE) / 2
  extra_padding_y <- convertHeight(unit(0.25, "lines"), "native", TRUE) / 2
  text_box <- c(
    x1 - extra_padding_x, y1 - extra_padding_y,
    x2 + extra_padding_x, y2 + extra_padding_y
  )
  int <- intersect_line_rectangle(point_pos, center, text_box)

  # Check if the data point is inside the label box.
  point_inside <- FALSE
  if (text_box[1] <= point_pos[1] && point_pos[1] <= text_box[3] &&
      text_box[2] <= point_pos[2] && point_pos[2] <= text_box[4]) {
    point_inside <- TRUE
  }

  # Nudge the original data point toward the label with point.padding.
  point_padding_x <- convertWidth(x$point.padding, "native", TRUE) / 2
  point_padding_y <- convertHeight(x$point.padding, "native", TRUE) / 2
  point_padding <- point_padding_x > 0 & point_padding_y > 0
  if (point_padding) {
    point_box <- c(
      point_pos[1] - point_padding_x, point_pos[2] - point_padding_y,
      point_pos[1] + point_padding_x, point_pos[2] + point_padding_y
    )
    point_pos <- intersect_line_rectangle(center, point_pos, point_box)
  }

  # Compute the distance between the data point and the edge of the text box.
  dx <- abs(int[1] - point_pos[1])
  dy <- abs(int[2] - point_pos[2])
  d <- sqrt(dx * dx + dy * dy)
  # Scale the unit vector by the minimum segment length.
  if (d > 0) {
    mx <- convertWidth(x$min.segment.length, "native", TRUE)
    my <- convertHeight(x$min.segment.length, "native", TRUE)
    min.segment.length <- sqrt((mx * dx / d) ^ 2 + (my * dy / d) ^ 2)
  }

  if (!point_inside && d > 0 && euclid(int, point_pos) > min.segment.length) {
    s <- segmentsGrob(
      x0 = int[1],
      y0 = int[2],
      x1 = point_pos[1],
      y1 = point_pos[2],
      default.units = "native",
      gp = x$segment.gp,
      name = "segment",
      arrow = x$arrow
    )
    setChildren(x, gList(s, t))
  } else {
    setChildren(x, gList(t))
  }
}
