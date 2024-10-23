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
  min.segment.length = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 10000,
  max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both","y","x"),
  seed = NA,
  verbose = getOption("verbose", default = FALSE),
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge_repel(nudge_x, nudge_y)
  }
  # Warn about limitations of the algorithm
  if (verbose && any(abs(data$angle %% 90) > 5)) {
    message(
      "ggrepel: Repulsion works correctly only for rotation angles multiple of 90 degrees"
    )
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
      min.segment.length = to_unit(min.segment.length),
      arrow = arrow,
      na.rm = na.rm,
      force = force,
      force_pull = force_pull,
      max.time = max.time,
      max.iter = max.iter,
      max.overlaps = max.overlaps,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      xlim = xlim,
      ylim = ylim,
      direction = match.arg(direction),
      seed = seed,
      verbose = verbose,
      ...
    )
  )
}

#' GeomLabelRepel
#' @rdname ggrepel
#' @format NULL
#' @usage NULL
#' @seealso \link[ggplot2]{GeomLabel} from the ggplot2 package.
#' @keywords internal
#' @export
GeomLabelRepel <- ggproto(
  "GeomLabelRepel", Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
    colour = "black", fill = "white", size = 3.88, angle = 0,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2,
    hjust = 0.5, vjust = 0.5, point.size = 1,
    segment.linetype = 1, segment.colour = NULL, segment.size = 0.5, segment.alpha = NULL,
    segment.curvature = 0, segment.angle = 90, segment.ncp = 1,
    segment.shape = 0.5, segment.square = TRUE, segment.squareShape = 1,
    segment.inflect = FALSE, segment.debug = FALSE
  ),

  draw_panel = function(
    data, panel_scales, coord,
    parse = FALSE,
    na.rm = FALSE,
    box.padding = 0.25,
    label.padding = 0.25,
    point.padding = 1e-6,
    label.r = 0.15,
    label.size = 0.25,
    min.segment.length = 0.5,
    arrow = NULL,
    force = 1,
    force_pull = 1,
    max.time = 0.5,
    max.iter = 10000,
    max.overlaps = 10,
    nudge_x = 0,
    nudge_y = 0,
    xlim = c(NA, NA),
    ylim = c(NA, NA),
    direction = "both",
    seed = NA,
    verbose = getOption("verbose", default = FALSE)
  ) {

    if (parse) {
      data$label <- parse_safe(as.character(data$label))
    }
    if (!length(data$label) || !length(which(not_empty(data$label)))) {
      return()
    }

    # if needed rename columns using our convention
    for (this_dim in c("x", "y")) {
      this_orig <- sprintf("%s_orig", this_dim)
      this_nudge <- sprintf("nudge_%s", this_dim)
      if (!this_nudge %in% colnames(data)) {
        data[[this_nudge]] <- data[[this_dim]]
        if (this_orig %in% colnames(data)) {
          data[[this_dim]] <- data[[this_orig]]
          data[[this_orig]] <- NULL
        }
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

    # Allow Inf.
    if (length(limits$x) == length(xlim)) {
      limits$x[is.infinite(xlim)] <- xlim[is.infinite(xlim)]
    }
    if (length(limits$y) == length(ylim)) {
      limits$y[is.infinite(ylim)] <- ylim[is.infinite(ylim)]
    }

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
      lab = data$label,
      box.padding = to_unit(box.padding),
      label.padding = to_unit(label.padding),
      point.padding = to_unit(point.padding),
      label.r = to_unit(label.r),
      label.size = label.size,
      min.segment.length = to_unit(min.segment.length),
      arrow = arrow,
      force = force,
      force_pull = force_pull,
      max.time = max.time,
      max.iter = max.iter,
      max.overlaps = max.overlaps,
      direction = direction,
      seed = seed,
      verbose = verbose,
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

  # Do not create text labels for empty strings.
  valid_strings <- which(not_empty(x$lab))
  invalid_strings <- which(!not_empty(x$lab))
  ix <- c(valid_strings, invalid_strings)
  x$data <- x$data[ix,]
  x$lab <- x$lab[ix]

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(seq_along(valid_strings), function(i) {
    row <- x$data[i, , drop = FALSE]
    t <- textGrob(
      x$lab[i],
      unit(row$x, "native") + x$label.padding,
      unit(row$y, "native") + x$label.padding,
      gp = gpar(
        fontsize   = row$size * .pt,
        fontfamily = row$family,
        fontface   = row$fontface,
        lineheight = row$lineheight
      ),
      name = "text"
    )
    r <- roundrectGrob(
      row$x, row$y, default.units = "native",
      width = grobWidth(t) + 2 * x$label.padding,
      height = grobHeight(t) + 2 * x$label.padding,
      r = x$label.r,
      gp = gpar(lwd = x$label.size * .pt),
      name = "box"
    )
    gw <- convertWidth(grobWidth(r), "native", TRUE)
    gh <- convertHeight(grobHeight(r), "native", TRUE)
    c(
      "x1" = row$x - gw *       row$hjust - box_padding_x + row$nudge_x,
      "y1" = row$y - gh *       row$vjust - box_padding_y + row$nudge_y,
      "x2" = row$x + gw * (1 - row$hjust) + box_padding_x + row$nudge_x,
      "y2" = row$y + gh * (1 - row$vjust) + box_padding_y + row$nudge_y
    )
  })

  # Make the repulsion reproducible if desired.
  if (!is.null(x$seed) && is.na(x$seed)) {
    x$seed <- sample.int(.Machine$integer.max, 1L)
  }

  # The points are represented by circles.
  x$data$point.size[is.na(x$data$point.size)] <- 0

  # Beware the magic numbers. I do not understand them.
  # I just accept them as necessary to get the code to work.
  p_width <- convertWidth(unit(1, "npc"), "inch", TRUE)
  p_height <- convertHeight(unit(1, "npc"), "inch", TRUE)
  p_ratio <- (p_width / p_height)
  if (p_ratio > 1) {
    p_ratio <- p_ratio ^ (1 / (1.15 * p_ratio))
  }
  point_size <- p_ratio * convertWidth(
    to_unit(x$data$point.size), "native", valueOnly = TRUE
  ) / 13
  point_padding <- p_ratio * convertWidth(
    to_unit(x$point.padding), "native", valueOnly = TRUE
  ) / 13

  # Repel overlapping bounding boxes away from each other.
  repel <- with_seed_null(x$seed, repel_boxes2(
    data_points     = as.matrix(x$data[,c("x","y")]),
    point_size      = point_size,
    point_padding_x = point_padding,
    point_padding_y = point_padding,
    boxes           = do.call(rbind, boxes),
    xlim            = range(x$limits$x),
    ylim            = range(x$limits$y),
    hjust           = x$data$hjust %||% 0.5,
    vjust           = x$data$vjust %||% 0.5,
    force_push      = x$force * 1e-6,
    force_pull      = x$force_pull * 1e-2,
    max_time        = x$max.time,
    max_iter        = ifelse(is.infinite(x$max.iter), 1e9, x$max.iter),
    max_overlaps    = x$max.overlaps,
    direction       = x$direction,
    verbose         = x$verbose
  ))

  if (x$verbose && any(repel$too_many_overlaps)) {
    message(
      sprintf(
        "ggrepel: %s unlabeled data points (too many overlaps). Consider increasing 'max.overlaps'",
        sum(repel$too_many_overlaps)
      )
    )
  }

  if (all(repel$too_many_overlaps)) {
    grobs <- list()
    class(grobs) <- "gList"
    return(setChildren(x, grobs))
  }

  grobs <- lapply(seq_along(valid_strings), function(i) {
    if (!repel$too_many_overlaps[i]) {
      row <- x$data[i, , drop = FALSE]
      makeLabelRepelGrobs(
        i,
        x$lab[i],
        # Position of text bounding boxes.
        x = unit(repel$x[i], "native"),
        y = unit(repel$y[i], "native"),
        # Position of original data points.
        x.orig = row$x,
        y.orig = row$y,
        # Width and height of text boxes.
        box.width = boxes[[i]]["x2"] - boxes[[i]]["x1"],
        box.height = boxes[[i]]["y2"] - boxes[[i]]["y1"],
        box.padding = x$box.padding,
        label.padding = x$label.padding,
        point.size = point_size[i],
        point.padding = x$point.padding,
        segment.curvature = row$segment.curvature,
        segment.angle     = row$segment.angle,
        segment.ncp       = row$segment.ncp,
        segment.shape = row$segment.shape,
        segment.square = row$segment.square,
        segment.squareShape = row$segment.squareShape,
        segment.inflect = row$segment.inflect,
        segment.debug = row$segment.debug,
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
          col = scales::alpha(row$segment.colour %||% row$colour, row$segment.alpha %||% row$alpha),
          lwd = row$segment.size * .pt,
          lty = row$segment.linetype %||% 1
        ),
        arrow = x$arrow,
        min.segment.length = x$min.segment.length,
        hjust = row$hjust,
        vjust = row$vjust
      )
    }
  })
  # Put segment grobs before text grobs, rect grobs before text grobs.
  grobs <- c(
    Filter(Negate(is.null), lapply(grobs, "[[", "segment")),
    unlist(
      Filter(Negate(is.null), lapply(grobs, "[[", "textbox")),
      recursive = FALSE, use.names = FALSE
    )
  )
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

makeLabelRepelGrobs <- function(
  i,
  label,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  # Position of original data points.
  x.orig = 0.5,
  y.orig = 0.5,
  # Width and height of text boxes.
  box.width = 0,
  box.height = 0,
  default.units = "npc",
  box.padding = 0.25,
  label.padding = 0.25,
  point.size = 1,
  point.padding = 1e-6,
  segment.curvature = 0,
  segment.angle = 90,
  segment.ncp = 1,
  segment.shape = 0.5,
  segment.square = TRUE,
  segment.squareShape = 1,
  segment.inflect = FALSE,
  segment.debug = FALSE,
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
  if (!is.unit(box.width))
    box.width <- unit(box.width, default.units)
  if (!is.unit(box.height))
    box.height <- unit(box.height, default.units)

  t <- textGrob(
    label,
    x - box.width * (0.5 - hjust),
    y - box.height * (0.5 - vjust),
    hjust = hjust,
    vjust = vjust,
    gp = text.gp,
    name = sprintf("textrepelgrob%s", i)
  )

  r <- roundrectGrob(
    x - box.width * (0.5 - hjust) - label.padding * (0.5 - hjust),
    y - box.height * (0.5 - vjust) - label.padding * (0.5 - vjust),
    default.units = "native",
    width = grobWidth(t) + 2 * label.padding,
    height = grobHeight(t) + 2 * label.padding,
    just = c(hjust, vjust),
    r = r,
    gp = rect.gp,
    name = sprintf("rectrepelgrob%s", i)
  )

  x1 <- convertWidth(x - 0.5 * grobWidth(r), "native", TRUE)
  x2 <- convertWidth(x + 0.5 * grobWidth(r), "native", TRUE)
  y1 <- convertHeight(y - 0.5 * grobHeight(r), "native", TRUE)
  y2 <- convertHeight(y + 0.5 * grobHeight(r), "native", TRUE)

  point_pos <- c(x.orig, y.orig)

  # Get the coordinates of the intersection between the line from the
  # original data point to the centroid and the rectangle's edges.
  text_box <- c(x1, y1, x2, y2)
  #int <- intersect_line_rectangle(point_pos, center, c(x1, y1, x2, y2))
  int <- select_line_connection(point_pos, text_box)

  # Check if the data point is inside the label box.
  point_inside_text <- FALSE
  if (text_box[1] <= point_pos[1] && point_pos[1] <= text_box[3] &&
      text_box[2] <= point_pos[2] && point_pos[2] <= text_box[4]) {
    point_inside_text <- TRUE
  }

  # This seems just fine.
  point.padding <- convertWidth(to_unit(point.padding), "native", TRUE) / 2

  point_int <- intersect_line_circle(int, point_pos, (point.size + point.padding))

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

  if (
    !point_inside_text &&
    d > 0 &&
    # Distance from label to point edge is greater than minimum.
    (!is.na(min.segment.length) && euclid(int, point_int) > min.segment.length) &&
    # Distance from label to point edge is less than from label to point center.
    euclid(int, point_int) < euclid(int, point_pos) &&
    # Distance from label to point center is greater than point size.
    euclid(int, point_pos) > point.size &&
    # Distance from label to point center is greater than from point edge to point center.
    euclid(int, point_pos) > euclid(point_int, point_pos)
  ) {
    s <- curveGrob(
      x1 = int[1],
      y1 = int[2],
      x2 = point_int[1],
      y2 = point_int[2],
      default.units = "native",
      curvature = segment.curvature,
      angle = segment.angle,
      ncp = segment.ncp,
      shape = segment.shape,
      square = segment.square,
      squareShape = segment.squareShape,
      inflect = segment.inflect,
      debug = segment.debug,
      gp = segment.gp,
      name = sprintf("segmentrepelgrob%s", i),
      arrow = arrow
    )
    grobs[["segment"]] <- s
  }

  grobs
}
