#' @rdname geom_text_repel
#' @param label.padding Amount of padding around label, as unit or number.
#'   Defaults to 0.25. (Default unit is lines, but other units can be specified
#'   by passing \code{unit(x, "units")}).
#' @param label.r Radius of rounded corners, as unit or number. Defaults
#'   to 0.15. (Default unit is lines, but other units can be specified by
#'   passing \code{unit(x, "units")}).
#' @param label.size Size of label border, in mm.
#' @export
geom_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.25,
  label.padding = 0.25,
  point.padding = 1e-6,
  label.r = 0.15,
  label.size = 0.25,
  segment.colour = NULL,
  segment.color = NULL,
  segment.size = 0.5,
  segment.alpha = NULL,
  min.segment.length = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
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
    #position <- position_nudge(nudge_x, nudge_y)
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      box.padding  = to_unit(box.padding),
      label.padding = to_unit(label.padding),
      point.padding  = to_unit(point.padding),
      label.r = to_unit(label.r),
      label.size = label.size,
      segment.colour = segment.color %||% segment.colour,
      segment.size = segment.size,
      segment.alpha = segment.alpha,
      min.segment.length = to_unit(min.segment.length),
      arrow = arrow,
      na.rm = na.rm,
      force = force,
      force_pull = force_pull,
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
    alpha = NA, family = "", fontface = 1, lineheight = 1.2,
    hjust = 0.5, vjust = 0.5
  ),

  draw_panel = function(
    self, data, panel_scales, coord,
    parse = FALSE,
    na.rm = FALSE,
    box.padding = 0.25,
    label.padding = 0.25,
    point.padding = 1e-6,
    label.r = 0.15,
    label.size = 0.25,
    segment.colour = NULL,
    segment.size = 0.5,
    segment.alpha = NULL,
    min.segment.length = 0.5,
    arrow = NULL,
    force = 1,
    force_pull = 1,
    nudge_x = 0,
    nudge_y = 0,
    xlim = c(NA, NA),
    ylim = c(NA, NA),
    max.iter = 2000,
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

    # Transform the nudges to the panel scales.
    nudges <- data.frame(
      x = data$x + rep_len(nudge_x, length.out = nrow(data)),
      y = data$y + rep_len(nudge_y, length.out = nrow(data))
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

    # Convert hjust and vjust to numeric if character
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    ggname("geom_label_repel", gTree(
      limits = limits,
      data = data,
      lab = lab,
      nudges = nudges,
      box.padding = to_unit(box.padding),
      label.padding = to_unit(label.padding),
      point.padding = to_unit(point.padding),
      label.r = to_unit(label.r),
      label.size = label.size,
      segment.colour = segment.colour,
      segment.size = segment.size,
      segment.alpha = segment.alpha,
      min.segment.length = to_unit(min.segment.length),
      arrow = arrow,
      force = force,
      force_pull = force_pull,
      max.iter = max.iter,
      direction = direction,
      seed = seed,
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
  box_padding_x <- convertWidth(x$box.padding, "npc", valueOnly = TRUE)
  box_padding_y <- convertHeight(x$box.padding, "npc", valueOnly = TRUE)

  # The padding around each point.
  if (is.na(x$point.padding)) {
    x$point.padding = unit(0, "lines")
  }
  point_padding_x <- convertWidth(x$point.padding, "native", valueOnly = TRUE)
  point_padding_y <- convertHeight(x$point.padding, "native", valueOnly = TRUE)

  # Do not create text labels for empty strings.
  valid_strings <- which(not_empty(x$lab))
  invalid_strings <- which(!not_empty(x$lab))

  # Create a dataframe with x y width height
  boxes <- lapply(valid_strings, function(i) {
    row <- x$data[i, , drop = FALSE]
    hj <- x$data$hjust[i]
    vj <- x$data$vjust[i]
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
        col = scales::alpha(row$colour, row$alpha),
        fill = scales::alpha(row$fill, row$alpha),
        lwd = x$label.size * .pt
      ),
      name = "box"
    )
    gw <- convertWidth(grobWidth(r), "native", TRUE)
    gh <- convertHeight(grobHeight(r), "native", TRUE)
    c(
      "x1" = row$x - gw * hj  - box_padding_x + x$nudges$x[i],
      "y1" = row$y - gh * vj - box_padding_y + x$nudges$y[i],
      "x2" = row$x + gw * (1 - hj) + box_padding_x + x$nudges$x[i],
      "y2" = row$y + gh * (1 - vj) + box_padding_y + x$nudges$y[i]
    )
    })

  # Make the repulsion reproducible if desired.
  if (is.null(x$seed) || !is.na(x$seed)) {
      set.seed(x$seed)
  }

  points_valid_first <- cbind(c(x$data$x[valid_strings],
                                x$data$x[invalid_strings]),
                              c(x$data$y[valid_strings],
                                x$data$y[invalid_strings]))

  # Repel overlapping bounding boxes away from each other.
  repel <- repel_boxes(
    data_points = points_valid_first,
    point_padding_x = point_padding_x,
    point_padding_y = point_padding_y,
    boxes = do.call(rbind, boxes),
    xlim = range(x$limits$x),
    ylim = range(x$limits$y),
    hjust = x$data$hjust %||% 0.5,
    vjust = x$data$vjust %||% 0.5,
    force_push = x$force * 1e-6,
    force_pull = x$force_pull * 1e-2,
    maxiter = x$max.iter,
    direction = x$direction
  )

  grobs_nested <- lapply(seq_along(valid_strings), function(i) {
    xi <- valid_strings[i]
    row <- x$data[xi, , drop = FALSE]
    makeLabelRepelGrobs(
      i,
      x$lab[xi],
      x = unit(repel$x[i], "native"),
      y = unit(repel$y[i], "native"),
      x.orig = unit(x$data$x[xi], "native"),
      y.orig = unit(x$data$y[xi], "native"),
      box.padding = x$box.padding,
      label.padding = x$label.padding,
      point.padding = x$point.padding,
      r = x$label.r,
      text.gp = gpar(
        col = scales::alpha(row$colour, row$alpha),
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      ),
      rect.gp = gpar(
        col = scales::alpha(row$colour, row$alpha),
        fill = scales::alpha(row$fill, row$alpha),
        lwd = x$label.size * .pt
      ),
      segment.gp = gpar(
        col = scales::alpha(x$segment.colour %||% row$colour, x$segment.alpha %||% row$alpha),
        lwd = x$segment.size * .pt
      ),
      arrow = x$arrow,
      min.segment.length = x$min.segment.length,
      hjust = x$data$hjust[i],
      vjust = x$data$vjust[i]
    )
  })
  # Get list of segment, rect and text grobs from grobs_nested
  # All the segment grobs come first,
  # then rect grob 1, text grob 1, rect grob 2, text grob 2, ...
  grobs <- arrange_label_repel_grobs(grobs_nested)
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

makeLabelRepelGrobs <- function(
  i,
  label,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  default.units = "npc",
  just = "center",
  box.padding = 0.25,
  label.padding = 0.25,
  point.padding = 1e-6,
  name = NULL,
  text.gp = gpar(),
  rect.gp = gpar(fill = "white"),
  r = unit(0.1, "snpc"),
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
    just = c(hj, vj),
    gp = text.gp,
    name = sprintf("textrepelgrob%s", i)
  )

  r <- roundrectGrob(
    x + 2 * (0.5 - hj) * box.padding,
    y + 2 * (0.5 - vj) * box.padding,
    default.units = "native",
    width = grobWidth(t) + 2 * label.padding,
    height = grobHeight(t) + 2 * label.padding,
    just = c(hj, vj),
    r = r,
    gp = rect.gp,
    name = sprintf("rectrepelgrob%s", i)
  )

  x1 <- convertWidth(x - 0.5 * grobWidth(r), "native", TRUE)
  x2 <- convertWidth(x + 0.5 * grobWidth(r), "native", TRUE)
  y1 <- convertHeight(y - 0.5 * grobHeight(r), "native", TRUE)
  y2 <- convertHeight(y + 0.5 * grobHeight(r), "native", TRUE)

  point_pos <- c(
    convertWidth(x.orig, "native", TRUE),
    convertHeight(y.orig, "native", TRUE)
  )

  center <- centroid(c(x1, y1, x2, y2), hjust, vjust)

  # Get the coordinates of the intersection between the line from the
  # original data point to the centroid and the rectangle's edges.
  text_box <- c(x1, y1, x2, y2)
  #int <- intersect_line_rectangle(point_pos, center, c(x1, y1, x2, y2))
  int <- select_line_connection(point_pos, text_box)

  # Check if the data point is inside the label box.
  point_inside <- FALSE
  if (text_box[1] <= point_pos[1] && point_pos[1] <= text_box[3] &&
      text_box[2] <= point_pos[2] && point_pos[2] <= text_box[4]) {
    point_inside <- TRUE
  }

  # Nudge the original data point toward the label with point.padding.
  point_padding_x <- convertWidth(point.padding, "native", TRUE) / 2
  point_padding_y <- convertHeight(point.padding, "native", TRUE) / 2
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
    mx <- convertWidth(min.segment.length, "native", TRUE)
    my <- convertHeight(min.segment.length, "native", TRUE)
    min.segment.length <- sqrt((mx * dx / d) ^ 2 + (my * dy / d) ^ 2)
  }

  grobs <- list(textbox = list(rect = r, text = t))

  if (!point_inside && d > 0 && euclid(int, point_pos) > min.segment.length) {
    s <- segmentsGrob(
      x0 = int[1],
      y0 = int[2],
      x1 = point_pos[1],
      y1 = point_pos[2],
      default.units = "native",
      gp = segment.gp,
      name = sprintf("segmentrepelgrob%s", i),
      arrow = arrow
    )
    grobs[["segment"]] <- s
  }

  grobs
}

# Convert a list of list(segment = segmentsGrob,
#                        textbox = list(roundrectGrob, textGrob))
# to a list(segmentsGrob, segmentsGrob, ...
#           roundrectGrob, textGrob, roundrectGrob, textGrob, ...)
arrange_label_repel_grobs <- function(grobs) {
  c(Filter(function(x) !is.null(x), lapply(grobs, "[[", "segment")),
    unlist(lapply(grobs, "[[", "textbox"),
           recursive = FALSE, use.names = FALSE))
}
