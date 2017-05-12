#' Repulsive points.
#'
#' \code{geom_point_repel} adds points directly to the plot after moving them
#' from the starting position.
#'
#' These geoms are based on \code{\link[ggplot2]{geom_point}} and
#' \code{\link[ggplot2]{geom_label}}. See the documentation for those
#' functions for more details. Differences from those functions are noted
#' here.
#'
#' @section \code{geom_point_repel}:

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
#' @param force Force of repulsion between overlapping text labels. Defaults
#'   to 1.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 2000.
#'
#' @examples
#'
#' df <- data.frame(x=runif(100), y=runif(100), color = sample(c("A","B")), 100, replace=TRUE)
#' p <- ggplot(df, aes(x=x,y=y)) + xlim(-20,20)
#'
#' # Normal Point Plotting
#' p + geom_point(aes(color=color))
#'
#' # Using geom_point_repel()
#' p + geom_point_repel(aes(color=color),force=200, size=5)
#'
#' @export
geom_point_repel <- function(
  mapping = NULL, data = NULL, stat = "identity",
  parse = FALSE,
  ...,
  box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  force = 5,
  max.iter = 2000,
  nudge_x = 0,
  nudge_y = 0,
  na.rm = FALSE,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointRepel,
    position = "identity",
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomPointRepel
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointRepel <- ggproto("GeomPointRepel", Geom,
                         required_aes = c("x", "y"),
                         non_missing_aes = c("size", "shape"),
                         default_aes = aes(
                           shape = 19, colour = "black", size = 1.5, fill = NA,
                           alpha = 1, stroke = 0.5,  angle = 0,
                           family = "", fontface = 1, lineheight = 1.2
                         ),

                         draw_panel = function(
                           data, panel_scales, coord,
                           parse = FALSE,
                           na.rm = FALSE,
                           box.padding = unit(0.25, "lines"),
                           point.padding = unit(1e-6, "lines"),
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
                           )
                           nudges <- coord$transform(nudges, panel_scales)

                           # Transform the raw data to the panel scales.
                           data <- coord$transform(data, panel_scales)

                           # The nudge is relative to the data.
                           nudges$x <- nudges$x - data$x
                           nudges$y <- nudges$y - data$y

                           ggname("geom_point_repel", gTree(
                             limits = limits,
                             data = data,
                             lab = lab,
                             nudges = nudges,
                             box.padding = box.padding,
                             point.padding = point.padding,
                             force = force,
                             max.iter = max.iter,
                             cl = "pointrepeltree"
                           ))
                         },

                         draw_key = draw_key_point
)

#' grid::makeContent function for the grobTree of pointRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.pointrepeltree <- function(x) {

  # The padding around each bounding box.
  pad.x <- convertWidth(x$box.padding, "native", valueOnly = TRUE)
  pad.y <- convertHeight(x$box.padding, "native", valueOnly = TRUE)

  # The padding around each point.
  pad.point.x <- convertWidth(x$point.padding, "native", valueOnly = TRUE)
  pad.point.y <- convertHeight(x$point.padding, "native", valueOnly = TRUE)

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(1:nrow(x$data), function(i) {
    row <- x$data[i, , drop = FALSE]
    tg <- grid::pointsGrob(
      x= row$x, y= row$y, default.units = "native"
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
    point_padding_x = pad.point.x,
    point_padding_y = pad.point.y,
    boxes = do.call(rbind, boxes),
    xlim = range(x$limits$x),
    ylim = range(x$limits$y),
    force = x$force * 1e-6,
    maxiter = x$max.iter
  )

  grobs <- lapply(1:nrow(x$data), function(i) {
    row <- x$data[i, , drop = FALSE]
    # browser()
    pointRepelGrob(
      x$lab[i],
      x = unit(repel$x[i], "native"),
      y = unit(repel$y[i], "native"),
      x.orig = unit(x$data$x[i], "native"),
      y.orig = unit(x$data$y[i], "native"),
      box.padding = x$box.padding,
      point.padding = x$point.padding,
      point.gp = gpar(
        col  = scales::alpha(row$colour, row$alpha),
        fill = scales::alpha(row$fill, row$alpha)
      )
    )
  })
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

pointRepelGrob <- function(
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  default.units = "npc",
  just = "center",
  box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  name = NULL,
  point.gp = gpar(),
  vp = NULL,
  arrow = NULL
) {

  #stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(
    x = x,
    y = y,
    x.orig = x.orig,
    y.orig = y.orig,
    just = just,
    box.padding = box.padding,
    point.padding = point.padding,
    name = name,
    point.gp = point.gp,
    vp = vp,
    cl = "pointrepelgrob",
    arrow = arrow
  )
}

#' grid::makeContent function for pointRepelGrob.
#'
#' @param x A grid grob.
#' @export
#' @noRd
makeContent.pointrepelgrob <- function(x) {
  pnt <- grid::pointsGrob(
    x$x,
    x$y,
    gp = x$point.gp,
    name = "points"
  )

  setChildren(x, gList(pnt))
}
