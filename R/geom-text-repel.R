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
#'   \item \code{nudge_x}
#'   \item \code{nudge_y}
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
#'   \item Aesthetics: to set an aesthetic to a fixed value, like
#'      \code{color = "red"} or \code{size = 3}.
#'   \item Other arguments to the layer, for example you override the
#'     default \code{stat} associated with the layer.
#'   \item Other arguments passed on to the stat.
#'   }
#' @param box.padding Amount of padding around bounding box. Defaults to
#'   \code{unit(0.25, "lines")}.
#' @param point.padding Amount of padding around labeled point. Defaults to
#'   \code{unit(0, "lines")}.
#' @param segment.color Color of the line segment connecting the data point to
#'   the text labe. Defaults to \code{#666666}.
#' @param segment.size Width of segment, in mm.
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
#' }
#'
#' @export
geom_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity",
  parse = FALSE,
  ...,
  box.padding = unit(0.25, "lines"),
  point.padding = unit(0, "lines"),
  segment.color = "#666666",
  segment.size = 0.5,
  force = 1,
  max.iter = 2000,
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
      force = force,
      max.iter = max.iter,
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
    point.padding = unit(0, "lines"),
    segment.color = "#666666",
    segment.size = 0.5,
    force = 1,
    max.iter = 2000
  ) {
    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    # Get the x and y limits of the panel area.
    limits <- data.frame(x = panel_scales$x.range, y = panel_scales$y.range)
    limits <- coord$transform(limits, panel_scales)

    # Transform the raw data to the panel scales.
    data <- coord$transform(data, panel_scales)

    # The padding around each bounding box.
    pad.x <- convertWidth(box.padding, "npc", valueOnly = TRUE)
    pad.y <- convertHeight(box.padding, "npc", valueOnly = TRUE)

    # Create a dataframe with x1 y1 x2 y2
    boxes <- lapply(1:nrow(data), function(i) {
      row <- data[i, , drop = FALSE]
      tg <- textGrob(
        lab[i],
        row$x, row$y, default.units = "native",
        rot = row$angle,
        gp = gpar(
          col = alpha(row$colour, row$alpha),
          fontsize = row$size * .pt,
          fontfamily = row$family,
          fontface = row$fontface,
          lineheight = row$lineheight
        ),
        check.overlap = FALSE
      )
      c(
        "x1" = row$x + convertWidth(grobX(tg, "west"), "npc", TRUE) - pad.x,
        "y1" = row$y - convertHeight(grobHeight(tg), "npc", TRUE) / 2 - pad.y,
        "x2" = row$x + convertWidth(grobX(tg, "east"), "npc", TRUE) + pad.x,
        "y2" = row$y + convertHeight(grobHeight(tg), "npc", TRUE) / 2 + pad.y
      )
    })

    # Fudge factor to make each box slightly wider. This is useful when the
    # user adds a legend to the plot, causing all the labels to squeeze
    # together.
    fudge.width <- abs(max(limits$x) - min(limits$x)) / 80
    boxes <- lapply(boxes, function(b) {
      # fudge.width <- abs(b['x2'] - b['x1']) / 10
      b['x1'] <- b['x1'] - fudge.width
      b['x2'] <- b['x2'] + fudge.width
      b
    })

    # Repel overlapping bounding boxes away from each other.
    repel <- repel_boxes(
      do.call(rbind, boxes),
      xlim = range(limits$x),
      ylim = range(limits$y),
      force = force * 1e-6,
      maxiter = max.iter
    )

    grobs <- lapply(1:nrow(data), function(i) {
      row <- data[i, , drop = FALSE]
      textRepelGrob(
        lab[i],
        x = unit(repel$x[i], "native"),
        y = unit(repel$y[i], "native"),
        x.orig = unit(data$x[i], "native"),
        y.orig = unit(data$y[i], "native"),
        box.padding = box.padding,
        point.padding = point.padding,
        text.gp = gpar(
          col = row$colour,
          fontsize = row$size * .pt,
          fontfamily = row$family,
          fontface = row$fontface,
          lineheight = row$lineheight
        ),
        segment.gp = gpar(
          col = segment.color,
          lwd = segment.size * .pt
        )
      )
    })
    class(grobs) <- "gList"

    ggname("geom_text_repel", grobTree(children = grobs))
  },

  draw_key = draw_key_text
)

textRepelGrob <- function(
  label,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  default.units = "npc",
  just = "center",
  box.padding = unit(0.25, "lines"),
  point.padding = unit(0, "lines"),
  name = NULL,
  text.gp = gpar(),
  segment.gp = gpar(),
  vp = NULL
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
    cl = "textrepelgrob"
  )
}

#' grid::makeContent function for textRepelGrob.
#'
#' @param x A grid grob.
#' @export
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

  d <- (center - orig)
  d <- d / euclid(center, orig)
  orig <- orig + convertWidth(x$point.padding, "native", TRUE) * d

  pad.x <- convertWidth(x$box.padding, "native", TRUE) / 2
  pad.y <- convertHeight(x$box.padding, "native", TRUE) / 2
  b <- c(x1 - pad.x, y1 - pad.y, x2 + pad.x, y2 + pad.y)

  if (!point_within_box(orig, b)) {

    # Get the coordinates of the intersection between the line from the
    # original data point to the centroid and the rectangle's edges.
    int <- intersect_line_rectangle(orig, center, b)

    s <- segmentsGrob(
      x0 = int[1],
      y0 = int[2],
      x1 = orig[1],
      y1 = orig[2],
      default.units = "native",
      gp = x$segment.gp,
      name = "segment"
    )

    return(setChildren(x, gList(s, t)))
  }

  return(setChildren(x, gList(t)))
}
