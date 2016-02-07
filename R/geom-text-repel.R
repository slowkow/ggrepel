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
#' the axes changes. Currently, the text labels will not be repositioned upon
#' resizing a plot. This may change in future releases.
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
#'        \code{color = "red"} or \code{size = 3}.
#'     \item Other arguments to the layer, for example you override the
#'       default \code{stat} associated with the layer.
#'     \item Other arguments passed on to the stat.
#'   }
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label.
#' @param box.padding Amount of padding around bounding box. Defaults to
#'   \code{unit(0.25, "lines")}.
#' @param point.padding Amount of padding around labeled point. Defaults to
#'   \code{unit(0, "lines")}.
#' @param segment.color Color of the line segment connecting the data point to
#'   the text labe. Defaults to \code{#666666}.
#' @param segment.size Width of segment, in mm.
#' @param arrow specification for arrow heads, as created by \code{\link[grid]{arrow}}
#' @param force Force of repulsion between overlapping text labels. Defaults
#'   to 1.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 2000.
#'
#' @examples
#'
#' p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
#'
#' # Avoid overlaps by repelling text labels
#' p + geom_text_repel()
#' # Labels with background
#' p + geom_label_repel()
#'
#' \dontrun{
#' p + geom_text_repel(family = "Times New Roman",
#'   box.padding = unit(0.5, "lines"))
#'
#' # Add aesthetic mappings
#' p + geom_text_repel(aes(colour = factor(cyl)))
#' p + geom_label_repel(aes(fill = factor(cyl)), colour = "white", fontface = "bold")
#'
#' # Nudge the starting positions
#' p + geom_text_repel(nudge_x = ifelse(mtcars$cyl == 6, 1, 0),
#'                     nudge_y = ifelse(mtcars$cyl == 6, 8, 0))
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
#'   annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")
#'
#' # Add arrows
#' p +
#'   geom_point(colour = "red") +
#'   geom_text_repel(arrow = arrow(length = unit(0.02, "npc")), box.padding = unit(1, "lines"))
#' }
#' @export
geom_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity",
  parse = FALSE,
  ...,
  box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  segment.color = "#666666",
  segment.size = 0.5,
  arrow = NULL,
  force = 1,
  max.iter = 2000,
  nudge_x = 0,
  nudge_y = 0,
  na.rm = FALSE,
  show.legend = NA,
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
      box.padding = box.padding,
      point.padding = point.padding,
      segment.color = segment.color,
      segment.size = segment.size,
      arrow = arrow,
      force = force,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
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
    box.padding = unit(0.25, "lines"),
    point.padding = unit(1e-6, "lines"),
    segment.color = "#666666",
    segment.size = 0.5,
    arrow = NULL,
    force = 1,
    max.iter = 2000,
    nudge_x = 0,
    nudge_y = 0
  ) {
    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    # Get the x and y limits of the panel area.
    limits <- data.frame(x = panel_scales$x.range, y = panel_scales$y.range)
    limits <- coord$transform(limits, panel_scales)

    # Transform the nudges to the panel scales.
    nudges <- data.frame(
      x = data$x + nudge_x, y = data$y + nudge_y
      # x = rep_len(nudge_x, nrow(data)),
      # y = rep_len(nudge_y, nrow(data))
    )
    nudges <- coord$transform(nudges, panel_scales)

    # Transform the raw data to the panel scales.
    data <- coord$transform(data, panel_scales)

    # The nudge is relative to the data.
    nudges$x <- nudges$x - data$x
    nudges$y <- nudges$y - data$y

    ggname("geom_text_repel", gTree(
      limits = limits,
      data = data,
      lab = lab,
      nudges = nudges,
      box.padding = box.padding,
      point.padding = point.padding,
      segment.color = segment.color,
      segment.size = segment.size,
      arrow = arrow,
      force = force,
      max.iter = max.iter,
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
  pad.x <- convertWidth(x$box.padding, "native", valueOnly = TRUE)
  pad.y <- convertHeight(x$box.padding, "native", valueOnly = TRUE)

  # The padding around each point.
  pad.point.x <- convertWidth(x$point.padding, "native", valueOnly = TRUE)
  pad.point.y <- convertHeight(x$point.padding, "native", valueOnly = TRUE)

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(1:nrow(x$data), function(i) {
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
      "x1" = row$x - gw - pad.x + x$nudges$x[i],
      "y1" = row$y - gh - pad.y + x$nudges$y[i],
      "x2" = row$x + gw + pad.x + x$nudges$x[i],
      "y2" = row$y + gh + pad.y + x$nudges$y[i]
    )
  })

  # Repel overlapping bounding boxes away from each other.
  repel <- repel_boxes(
    data_points = cbind(x$data$x, x$data$y),
    pad_point_x = pad.point.x,
    pad_point_y = pad.point.y,
    boxes = do.call(rbind, boxes),
    xlim = range(x$limits$x),
    ylim = range(x$limits$y),
    force = x$force * 1e-6,
    maxiter = x$max.iter
  )

  grobs <- lapply(1:nrow(x$data), function(i) {
    row <- x$data[i, , drop = FALSE]
    textRepelGrob(
      x$lab[i],
      x = unit(repel$x[i], "native"),
      y = unit(repel$y[i], "native"),
      x.orig = unit(x$data$x[i], "native"),
      y.orig = unit(x$data$y[i], "native"),
      box.padding = x$box.padding,
      point.padding = x$point.padding,
      text.gp = gpar(
        col = row$colour,
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      ),
      segment.gp = gpar(
        col = x$segment.color,
        lwd = x$segment.size * .pt
      ),
      arrow = x$arrow
    )
  })
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

textRepelGrob <- function(
  label,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  default.units = "npc",
  just = "center",
  box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  name = NULL,
  text.gp = gpar(),
  segment.gp = gpar(),
  vp = NULL,
  arrow = NULL
) {

  stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(
    label = label,
    x = x,
    y = y,
    x.orig = x.orig,
    y.orig = y.orig,
    just = just,
    box.padding = box.padding,
    point.padding = point.padding,
    name = name,
    text.gp = text.gp,
    segment.gp = segment.gp,
    vp = vp,
    cl = "textrepelgrob",
    arrow = arrow
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
    just = c(hj, vj),
    gp = x$text.gp,
    name = "text"
  )

  x1 <- convertWidth(x$x - 0.5 * grobWidth(t), "native", TRUE)
  x2 <- convertWidth(x$x + 0.5 * grobWidth(t), "native", TRUE)
  y1 <- convertHeight(x$y - 0.5 * grobHeight(t), "native", TRUE)
  y2 <- convertHeight(x$y + 0.5 * grobHeight(t), "native", TRUE)

  orig <- c(
    convertWidth(x$x.orig, "native", TRUE),
    convertHeight(x$y.orig, "native", TRUE)
  )

  center <- centroid(c(x1, y1, x2, y2))

  # Get the coordinates of the intersection between the line from the
  # original data point to the centroid and the rectangle's edges.
  pad.x <- convertWidth(unit(0.25, "lines"), "native", TRUE) / 2
  pad.y <- convertHeight(unit(0.25, "lines"), "native", TRUE) / 2
  b <- c(x1 - pad.x, y1 - pad.y, x2 + pad.x, y2 + pad.y)
  int <- intersect_line_rectangle(orig, center, b)

  # Nudge the original data point toward the label with point.padding.
  pad.x <- convertWidth(x$point.padding, "native", TRUE) / 2
  pad.y <- convertHeight(x$point.padding, "native", TRUE) / 2
  b <- c(orig[1] - pad.x, orig[2] - pad.y, orig[1] + pad.x, orig[2] + pad.y)
  orig <- intersect_line_rectangle(center, orig, b)

  s <- segmentsGrob(
    x0 = int[1],
    y0 = int[2],
    x1 = orig[1],
    y1 = orig[2],
    default.units = "native",
    gp = x$segment.gp,
    name = "segment",
    arrow = x$arrow
  )

  setChildren(x, gList(s, t))
}
