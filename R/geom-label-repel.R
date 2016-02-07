#' @rdname geom_text_repel
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#' @export
geom_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity",
  parse = FALSE,
  ...,
  box.padding = unit(0.25, "lines"),
  label.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  label.r = unit(0.15, "lines"),
  label.size = 0.25,
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
    geom = GeomLabelRepel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      box.padding  = box.padding,
      label.padding = label.padding,
      point.padding  = point.padding,
      label.r = label.r,
      label.size = label.size,
      segment.color = segment.color,
      segment.size = segment.size,
      arrow = arrow,
      na.rm = na.rm,
      force = force,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      ...
    )
  )
}

#' GeomLabelRepel
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLabelRepel <- ggproto(
  "GeomLabelRepel", Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
    colour = "black", fill = "white", size = 3.88, angle = 0,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(
    self, data, panel_scales, coord,
    parse = FALSE,
    na.rm = FALSE,
    box.padding = unit(0.25, "lines"),
    label.padding = unit(0.25, "lines"),
    point.padding = unit(1e-6, "lines"),
    label.r = unit(0.15, "lines"),
    label.size = 0.25,
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
      lab <- parse(text = as.character(lab))
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

    ggname("geom_label_repel", gTree(
      limits = limits,
      data = data,
      lab = lab,
      nudges = nudges,
      box.padding = box.padding,
      label.padding = label.padding,
      point.padding = point.padding,
      label.r = label.r,
      label.size = label.size,
      segment.color = segment.color,
      segment.size = segment.size,
      arrow = arrow,
      force = force,
      max.iter = max.iter,
      cl = "labelrepeltree"
    ))
  },

  draw_key = draw_key_label
)

#' grid::makeContent function for the grobTree of textRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.labelrepeltree <- function(x) {

  # The padding around each bounding box.
  pad.x <- convertWidth(x$box.padding, "npc", valueOnly = TRUE)
  pad.y <- convertHeight(x$box.padding, "npc", valueOnly = TRUE)

  # The padding around each point.
  pad.point.x <- convertWidth(x$point.padding, "native", valueOnly = TRUE)
  pad.point.y <- convertHeight(x$point.padding, "native", valueOnly = TRUE)

  # Create a dataframe with x y width height
  boxes <- lapply(1:nrow(x$data), function(i) {
    row <- x$data[i, , drop = FALSE]
    t <- textGrob(
      x$lab[i],
      unit(row$x, "native") + x$label.padding,
      unit(row$y, "native") + x$label.padding,
      gp = gpar(
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      ),
      name = "text"
    )
    r <- roundrectGrob(
      row$x, row$y, default.units = "native",
      width = grobWidth(t) + 2 * x$label.padding,
      height = grobHeight(t) + 2 * x$label.padding,
      r = x$label.r,
      gp = gpar(
      col = row$colour,
      fill = alpha(row$fill, row$alpha),
        lwd = x$label.size * .pt
      ),
      name = "box"
    )
    gw <- convertWidth(grobWidth(r), "native", TRUE) / 2
    gh <- convertHeight(grobHeight(r), "native", TRUE) / 2
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
    labelRepelGrob(
      x$lab[i],
      x = unit(repel$x[i], "native"),
      y = unit(repel$y[i], "native"),
      x.orig = unit(x$data$x[i], "native"),
      y.orig = unit(x$data$y[i], "native"),
      box.padding = x$box.padding,
      label.padding = x$label.padding,
      point.padding = x$point.padding,
      r = x$label.r,
      text.gp = gpar(
        col = row$colour,
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      ),
      rect.gp = gpar(
        col = row$colour,
        fill = alpha(row$fill, row$alpha),
        lwd = x$label.size * .pt
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

labelRepelGrob <- function(
  label,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  default.units = "npc",
  just = "center",
  box.padding = unit(0.25, "lines"),
  label.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  name = NULL,
  text.gp = gpar(),
  rect.gp = gpar(fill = "white"),
  r = unit(0.1, "snpc"),
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
    label.padding = label.padding,
    point.padding = point.padding,
    r = r,
    name = name,
    text.gp = text.gp,
    rect.gp = rect.gp,
    segment.gp = segment.gp,
    vp = vp,
    cl = "labelrepelgrob",
    arrow = arrow
  )
}

#' grid::makeContent function for labelRepelGrob.
#'
#' @param x A grid grob.
#' @export
#' @noRd
makeContent.labelrepelgrob <- function(x) {
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

  r <- roundrectGrob(
    x$x + 2 * (0.5 - hj) * x$box.padding,
    x$y + 2 * (0.5 - vj) * x$box.padding,
    default.units = "native",
    width = grobWidth(t) + 2 * x$label.padding,
    height = grobHeight(t) + 2 * x$label.padding,
    just = c(hj, vj),
    r = x$r,
    gp = x$rect.gp,
    name = "box"
  )

  x1 <- convertWidth(x$x - 0.5 * grobWidth(r), "native", TRUE)
  x2 <- convertWidth(x$x + 0.5 * grobWidth(r), "native", TRUE)
  y1 <- convertHeight(x$y - 0.5 * grobHeight(r), "native", TRUE)
  y2 <- convertHeight(x$y + 0.5 * grobHeight(r), "native", TRUE)

  orig <- c(
    convertWidth(x$x.orig, "native", TRUE),
    convertHeight(x$y.orig, "native", TRUE)
  )

  center <- centroid(c(x1, y1, x2, y2))

  # Get the coordinates of the intersection between the line from the
  # original data point to the centroid and the rectangle's edges.
  int <- intersect_line_rectangle(orig, center, c(x1, y1, x2, y2))

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

  setChildren(x, gList(s, r, t))
}
