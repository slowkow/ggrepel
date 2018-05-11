#' Wordcloud.
#'
#' \code{geom_text_wordcloud} adds a wordcloud directly to the plot. It is inspired
#' by the layout of \code{\link[wordcloud]{wordcloud}}.
#'
#' This geom is based on \code{\link[ggplot2]{geom_text}}. See the
#' documentation for those functions for more details. Differences
#' from those functions are noted here.
#'
#' Text labels have height and width, but they are physical units, not data
#' units. The amount of space they occupy on that plot is not constant in data
#' units: when you resize a plot, labels stay the same size, but the size of
#' the axes changes. The text labels are repositioned after resizing a plot.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}} or
#'   \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot. Note that \code{x} and \code{y} do not have to be
#'   specified and default to .5 if they are not specified.
#'
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
#' @param rstep relative wordclould spiral radius increment after one full rotation. Default to .1.
#' @param tstep wordclould spiral angle increment at each step. Default to .1.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 2000.
#' @param seed Random seed passed to \code{\link[base]{set.seed}}. Defaults to
#'   \code{NA}, which means that \code{set.seed} will not be called.
#'
#' @examples
#' p <- ggplot(mtcars,
#'             aes(label = rownames(mtcars), size = mpg,
#'                 colour = factor(cyl)))
#'
#' p + geom_text_wordcloud()
#'
#' p + geom_text_wordcloud() +
#'   scale_size(limits = c(0, NA))
#'
#' p + geom_text_wordcloud(aes(angle = 90 * (runif(nrow(mtcars))>.9)))
#'
#' p + geom_text_wordcloud(aes(x = cyl)) +
#'   scale_x_continuous(limits = c(3, 9))
#' @export
geom_text_wordcloud <- function(
  mapping = NULL, data = NULL, stat = "identity",
  parse = FALSE,
  ...,
  box.padding = 0.025,
  point.padding = NA,
  arrow = NULL,
  rstep = .1,
  tstep = .1,
  max.iter = 2000,
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  seed = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextWordcloud,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      box.padding = to_unit(box.padding),
      point.padding = to_unit(point.padding),
      rstep = rstep,
      tstep = tstep,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      xlim = xlim,
      ylim = ylim,
      seed = seed,
      ...
    )
  )
}

#' GeomTextWordcloud
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextWordcloud <- ggproto("GeomTextWordcloud", Geom,
  required_aes = c("label"),

  default_aes = aes(
    x = .5, y = .5,
    colour = "black", size = 3.88, angle = 0,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(
    data, panel_scales, coord,
    parse = FALSE,
    na.rm = FALSE,
    box.padding = 0.025,
    point.padding = NA,
    rstep = .1,
    tstep = .1,
    max.iter = 2000,
    nudge_x = 0,
    nudge_y = 0,
    xlim = c(NA, NA),
    ylim = c(NA, NA),
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

    ggname("geom_text_wordcloud", gTree(
      limits = limits,
      data = data,
      lab = lab,
      nudges = nudges,
      box.padding = to_unit(box.padding),
      point.padding = to_unit(point.padding),
      rstep = rstep,
      tstep = tstep,
      max.iter = max.iter,
      seed = seed,
      cl = "textwordcloudtree"
    ))
  },

  draw_key = draw_key_text
)

#' grid::makeContent function for the grobTree of textRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.textwordcloudtree <- function(x) {

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
    gw <- convertWidth(grobWidth(tg), "native", TRUE)
    gh <- convertHeight(grobHeight(tg), "native", TRUE)

    c(
      "x1" = row$x - box_padding_x + x$nudges$x[i],
      "y1" = row$y - box_padding_y + x$nudges$y[i],
      "x2" = row$x + gw + box_padding_x + x$nudges$x[i],
      "y2" = row$y + gh + box_padding_y + x$nudges$y[i]
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


  repel <- wordcloud_boxes(
    data_points = points_valid_first,
    point_padding_x = point_padding_x,
    point_padding_y = point_padding_y,
    boxes = do.call(rbind, boxes),
    xlim = range(x$limits$x),
    ylim = range(x$limits$y),
    rstep = x$tstep,
    tstep = x$tstep,
    maxiter = x$max.iter
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
      min.segment.length = to_unit(100)
    )
  })
  class(grobs) <- "gList"

  setChildren(x, grobs)
}
