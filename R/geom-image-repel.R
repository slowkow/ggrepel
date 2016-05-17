#' Plot Repeled Images.
#'
#' \code{geom_image_repel} adds images directly to the plot after moving them
#' from the starting position to avoid overlap.
#'
#' @section \code{geom_image_repel}:

#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}} or
#'   \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot.
#' @param image The name of the column holding filepaths to you PNG images.
#' @param width the width of the image files to plot using "npc" units where 1=100%
#' Defaults to \code{unit(0.1, "npc")}.
#' @param aspectratio. Along with width it is used to determine the width of your imgae. Defaults to
#'   \code{1}.
#'
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There are
#'   three types of arguments you can use here:
#'
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
#' @param segment.size Width of line segment connecting the data point to
#'   the text label, in mm.
#' @param segment.alpha Transparency of the segment, in \code{[0,1]}. Makes segments half transparent by default.
#' @param arrow specification for arrow heads, as created by \code{\link[grid]{arrow}}
#' @param force Force of repulsion between overlapping text labels. Defaults
#'   to 1.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 2000.
#'
#' @examples
#'
#' p <- ggplot(mtcars, aes(x=mpg,y=cyl))
#'
#' # Avoid overlaps by repelling text labels
#' p + geom_image_repel()
#' p + geom_image_repel(aes(size=mpg, color=factor(cyl)))
#'
#' @importFrom png readPNG
#' @export
geom_image_repel <- function(
  mapping = NULL, data = NULL, stat = "identity",
  parse = FALSE,
  ...,
  box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  width = 0.1,
  aspectratio = 1,
  segment.size = 0.5,
  segment.alpha = 0.5,
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
    geom = GeomImageRepel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      box.padding = box.padding,
      point.padding = point.padding,
      segment.size = segment.size,
      segment.alpha = segment.alpha,
      arrow = arrow,
      force = force,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      width = width,
      aspectratio = aspectratio,
      ...
    )
  )
}

#' GeomImageRepel
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomImageRepel <- ggproto("GeomImageRepel", Geom,
                          required_aes = c("x", "y", "image"),

                          default_aes = aes(
                            colour = "black", size = 3.88, angle = 0,
                            alpha = 1, family = "", fontface = 1, lineheight = 1.2
                          ),

                          draw_panel = function(
                            data, panel_scales, coord,
                            parse = FALSE,
                            na.rm = FALSE,
                            box.padding = unit(0.25, "lines"),
                            point.padding = unit(1e-6, "lines"),
                            segment.size = 0.5,
                            segment.alpha = 0.5,
                            arrow = NULL,
                            force = 1,
                            max.iter = 2000,
                            nudge_x = 0,
                            nudge_y = 0,
                            width = 0.1,
                            aspectratio = 1
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

                            ggname("geom_image_repel", gTree(
                              limits = limits,
                              data = data,
                              lab = lab,
                              nudges = nudges,
                              box.padding = box.padding,
                              point.padding = point.padding,
                              segment.size = segment.size,
                              segment.alpha = segment.alpha,
                              arrow = arrow,
                              force = force,
                              width = width,
                              aspectratio = aspectratio,
                              max.iter = max.iter,
                              cl = "imagerepeltree"
                            ))
                          },

                          draw_key = draw_key_text
)

#' grid::makeContent function for the grobTree of textRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.imagerepeltree <- function(x) {

  #calculate width/height based on the width and the aspect ratio
  imgwidth  <- x$width
  imgheight <- imgwidth * x$aspectratio

  # The padding around each bounding box.
  pad.x <- convertWidth(x$box.padding, "native", valueOnly = TRUE)
  pad.y <- convertHeight(x$box.padding, "native", valueOnly = TRUE)

  # The padding around each point.
  pad.point.x <- convertWidth(x$point.padding, "native", valueOnly = TRUE)
  pad.point.y <- convertHeight(x$point.padding, "native", valueOnly = TRUE)

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(1:nrow(x$data), function(i) {
    row <- x$data[i, , drop = FALSE]

    rectg <- grid::rectGrob(
      x      = row$x,
      y      = row$y,
      width  = imgwidth,
      height = imgheight,
      default.units = "native"

    )
    gw <- convertWidth(grobWidth(rectg), "native", TRUE) / 2
    gh <- convertHeight(grobHeight(rectg), "native", TRUE) / 2
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
    # browser()
    imageRepelGrob(
      x$lab[i],
      x = unit(repel$x[i], "native"),
      y = unit(repel$y[i], "native"),
      x.orig = unit(x$data$x[i], "native"),
      y.orig = unit(x$data$y[i], "native"),
      imgwidth  = imgwidth,
      imgheight = imgheight,
      image = x$data$image[i],
      box.padding = x$box.padding,
      point.padding = x$point.padding,
      segment.gp = gpar(
        col = scales::alpha(row$colour, row$alpha * x$segment.alpha),
        lwd = x$segment.size * .pt
      ),
      arrow = x$arrow
    )
  })
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

imageRepelGrob <- function(
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  imgwidth = unit(0.1, "npc"),
  imgheight = unit(0.1, "npc"),
  x.orig = unit(0.5, "npc"),
  y.orig = unit(0.5, "npc"),
  default.units = "npc",
  just = "center",
  box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-6, "lines"),
  name = NULL,
  segment.gp = gpar(),
  vp = NULL,
  arrow = NULL,
  image = NULL
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
    imgwidth = imgwidth,
    imgheight = imgheight,
    just = just,
    box.padding = box.padding,
    point.padding = point.padding,
    name = name,
    segment.gp = segment.gp,
    vp = vp,
    cl = "imagerepelgrob",
    arrow = arrow,
    image = image
  )
}

#' grid::makeContent function for textRepelGrob.
#'
#' @param x A grid grob.
#' @export
#' @noRd
makeContent.imagerepelgrob <- function(x) {

  pnt <- pointsGrob(
    x$x,
    x$y,
    gp = x$text.gp,
    name = "points"
  )

  x1 <- convertWidth(x$x - 0.5 * grobWidth(pnt), "native", TRUE)
  x2 <- convertWidth(x$x + 0.5 * grobWidth(pnt), "native", TRUE)
  y1 <- convertHeight(x$y - 0.5 * grobHeight(pnt), "native", TRUE)
  y2 <- convertHeight(x$y + 0.5 * grobHeight(pnt), "native", TRUE)

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

  #test that image is present and it opens as a PNG
  if (!is.character(x$image)) {stop("Image column must specify a character pointing to an image file")}

  img <- NULL
  try(img  <- readPNG(x$image))
  if (is.null(img)) {stop("Your image file must be a PNG image.")}

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

  rg <- rasterGrob(
     image  = img,
     width  = x$imgwidth,
     height = x$imgheight,
     x = int[1],
     y = int[2],
     interpolate=TRUE,
     default.units = "native",
     name="image"
  )

  setChildren(x, gList(s, rg))
}
