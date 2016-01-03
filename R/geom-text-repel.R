#' @export
geom_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", parse = FALSE, ...,
  nudge_x = 0, nudge_y = 0,
  label.padding = unit(0.25, "lines"),
  segment.color = "#666666",
  segment.size = 0.5,
  force = 1,
  max.iter = 10000,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
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
      label.padding = label.padding,
      segment.color = segment.color,
      segment.size = segment.size,
      force = force,
      max.iter = max.iter,
      ...
    )
  )
}

#' GeomTextRepel
#' @format NULL
#' @usage NULL
#' @export
GeomTextRepel <- ggproto("GeomTextRepel", Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(
    data, panel_scales, coord,
    parse = FALSE,
    na.rm = FALSE,
    label.padding = unit(0.25, "lines"),
    segment.color = "#666666",
    segment.size = 0.5,
    force = 1,
    max.iter = 10000
  ) {
    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    data <- coord$transform(data, panel_scales)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    pad.x <- convertWidth(label.padding, "npc", valueOnly = TRUE)
    pad.y <- convertHeight(label.padding, "npc", valueOnly = TRUE)

    # Create a dataframe with x y width height
    boxes <- lapply(1:nrow(data), function(i) {
      row <- data[i, , drop = FALSE]
      tg <- textGrob(
        lab[i],
        row$x, row$y, default.units = "native",
        hjust = row$hjust, vjust = row$vjust,
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

    ws <- repel_boxes(
      do.call(rbind, boxes),
      xlim = range(data$x), ylim = range(data$y),
      force = force * 1e-6, maxiter = max.iter
    )

#     cat("data$x", range(data$x), "\n")
#     cat("ws$x", range(ws$x), "\n")
#     cat("data$y", range(data$y), "\n")
#     cat("ws$y", range(ws$y), "\n")

#     grobs <- textGrob(
#       lab,
#       ws$x, ws$y, default.units = "native",
#       hjust = data$hjust, vjust = data$vjust,
#       rot = data$angle,
#       gp = gpar(
#         col = alpha(data$colour, data$alpha),
#         fontsize = data$size * .pt,
#         fontfamily = data$family,
#         fontface = data$fontface,
#         lineheight = data$lineheight
#       ),
#       check.overlap = FALSE
#     )
#
#     grobs

    grobs <- lapply(1:nrow(data), function(i) {
      row <- data[i, , drop = FALSE]
      textRepelGrob(
        lab[i],
        x = unit(ws$x[i], "native"),
        y = unit(ws$y[i], "native"),
        x.orig = unit(data$x[i], "native"),
        y.orig = unit(data$y[i], "native"),
        just = c(row$hjust, row$vjust),
        padding = label.padding,
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
  padding = unit(0.25, "lines"),
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
    padding = padding,
    name = name,
    text.gp = text.gp,
    segment.gp = segment.gp,
    vp = vp,
    cl = "textrepelgrob"
  )
}

#' @export
makeContent.textrepelgrob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)

  t <- textGrob(
    x$label,
    x$x + 2 * (0.5 - hj) * x$padding,
    x$y + 2 * (0.5 - vj) * x$padding,
    just = c(hj, vj),
    gp = x$text.gp,
    name = "text"
  )
#   x1 <- convertWidth(x$x - 0.5 * grobWidth(t), "native", TRUE)
#   x2 <- convertWidth(x$x + 0.5 * grobWidth(t), "native", TRUE)
#   y1 <- convertHeight(x$y - 0.5 * grobHeight(t), "native", TRUE)
#   y2 <- convertHeight(x$y + 0.5 * grobHeight(t), "native", TRUE)
#   xo <- convertWidth(x$x.orig, "native", TRUE)
#   yo <- convertHeight(x$y.orig, "native", TRUE)
#
#   if (!point_within_box(xo, yo, c(x1, x2, y1, y2))) {
#     s <- segmentsGrob(
#       x0 = x$x,
#       y0 = x$y,
#       x1 = x$x.orig,
#       y1 = x$y.orig,
#       default.units = "native",
#       gp = x$segment.gp,
#       name = "segment"
#     )
#     return(setChildren(x, gList(s, t)))
#   }
#
#   return(setChildren(x, gList(t)))

  x1 <- convertWidth(x$x - 0.5 * grobWidth(t), "native", TRUE)
  x2 <- convertWidth(x$x + 0.5 * grobWidth(t), "native", TRUE)
  y1 <- convertHeight(x$y - 0.5 * grobHeight(t), "native", TRUE)
  y2 <- convertHeight(x$y + 0.5 * grobHeight(t), "native", TRUE)
  xo <- convertWidth(x$x.orig, "native", TRUE)
  yo <- convertHeight(x$y.orig, "native", TRUE)

  center <- centroid(c(x1, y1, x2, y2))

  int <- intersect_line_rectangle(c(xo, yo), center, c(x1, y1, x2, y2))

  s <- segmentsGrob(
    x0 = int[1],
    y0 = int[2],
    x1 = x$x.orig,
    y1 = x$y.orig,
    default.units = "native",
    gp = x$segment.gp,
    name = "segment"
  )
  return(setChildren(x, gList(s, t)))
}
