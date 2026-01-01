#' Repulsive text element
#'
#' This text element is a replacement for \code{\link[ggplot2]{element_text}}
#' that repulses labels.
#'
#' @inheritParams ggplot2::element_text
#' @inheritParams geom_text_repel
#' @param segment.colour,segment.linetype,segment.size Graphical parameters for
#'   the line connecting the text to points of origin.
#' @param segment.curvature,segment.angle,segment.ncp,segment.shape,segment.square,segment.squareShape,segment.inflect
#'   Settings for curving the connecting line. See \code{\link[grid]{curveGrob}}
#'   for descriptions of these parameters.
#' @param position One of \code{"top"}, \code{"right"}, \code{"bottom"},
#'   \code{"left"} setting where the text labels should be relative to points
#'   of origin.
#'
#' @return An object of class \code{<element_text_repel>}.
#' @export
#'
#' @examples
#' # A plot with a crowded y-axis
#' p <- ggplot(mtcars, aes(mpg, rownames(mtcars))) +
#'   geom_col() +
#'   coord_cartesian(ylim = c(-32, 64)) +
#'   theme(axis.text.y = element_text_repel())
#'
#' # By default there isn't enough space to draw distinctive lines
#' p
#'
#' # The available space can be increased by setting the margin
#' p + theme(axis.text.y.left = element_text_repel(margin = margin(r = 20)))
#'
#' # For secondary axis positions at the top and right, the `position` argument
#' # should be set accordingly
#' p + scale_y_discrete(position = "right") +
#'   theme(axis.text.y.right = element_text_repel(
#'     margin = margin(l = 20),
#'     position = "right"
#'   ))
#'
#' # Using segment settings and matching tick colour
#' p + theme(
#'   axis.text.y.left = element_text_repel(
#'     margin = margin(r = 20),
#'     segment.curvature = -0.1,
#'     segment.inflect = TRUE,
#'     segment.colour = "red"
#'   ),
#'   axis.ticks.y.left = element_line(colour = "red")
#' )
element_text_repel <- function(
  # Generic text settings
  family = NULL,
  face   = NULL,
  colour = NULL,
  size   = NULL,
  hjust  = NULL,
  vjust  = NULL,
  angle  = NULL,
  lineheight = NULL,
  color = NULL,

  # Spacings
  margin = NULL,
  box.padding = NULL,

  # Repel settings
  force = NULL,
  force_pull = NULL,
  max.time = NULL,
  max.iter = NULL,
  max.overlaps = NULL,

  # Segment settings
  min.segment.length = NULL,
  segment.colour = NULL,
  segment.linetype = NULL,
  segment.size = NULL,
  segment.curvature = NULL,
  segment.angle = NULL,
  segment.ncp = NULL,
  segment.shape = NULL,
  segment.square = NULL,
  segment.squareShape = NULL,
  segment.inflect = NULL,
  arrow = NULL,

  # General settings
  seed = NA,
  position = c("bottom", "top", "left", "right"),
  inherit.blank = FALSE
) {
  # Capture arguments in list
  args <- setdiff(rlang::fn_fmls_names(element_text_repel), c("color", "colour"))
  vals <- mget(args, envir = rlang::current_env())
  vals["colour"] <- list(color %||% colour)

  structure(
    vals,
    class = c("element_text_repel", "element_text", "element")
  )
}

#' @export
#' @method element_grob element_text_repel
element_grob.element_text_repel <- function(
  element, label = "", x = NULL, y = NULL,
  family = NULL, face = NULL, colour = NULL, size = NULL,
  hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  margin = NULL, margin_x = FALSE, margin_y = FALSE,
  position = c("bottom", "top", "left", "right"), ...
) {
  if (is.null(x %||% y)) {
    # Nothing to repel from, might be a legend or title
    # Fall back to standard element_text rendering (replicating ggplot2's titleGrob)
    if (is.null(label)) {
      return(zeroGrob())
    }
    vj <- vjust %||% element$vjust
    hj <- hjust %||% element$hjust
    margin <- margin %||% element$margin
    angle <- angle %||% element$angle %||% 0
    
    gp <- gpar(
      fontsize = size %||% element$size,
      col = colour %||% element$colour,
      fontfamily = family %||% element$family,
      fontface = face %||% element$face,
      lineheight = lineheight %||% element$lineheight
    )
    
    out <- ggplot2:::titleGrob(
      label, x, y, hjust = hj, vjust = vj, angle = angle,
      gp = gp, margin = margin, margin_x = margin_x, margin_y = margin_y, ...
    )
    return(out)
  }
  if (is.null(label) || sum(nzchar(label) & !is.na(label)) < 1) {
    # No labels to render
    return(zeroGrob())
  }

  # Resolve position.
  # Axes often have only x *or* y defined but not both.
  # So if we have `x` but not `y`, we're probably in a top or bottom axis.
  # Likewise, if we have `y` but not `x`, we're a left or right axis.
  # In some rare cases we might have both, which will get the `"none"` position.
  position <- element$position
  if (is.null(x)) {
    position <- intersect(position, c("left", "right"))
  }
  if (is.null(y)) {
    position <- intersect(position, c("top", "bottom"))
  }
  if (length(position) < 1 || (!is.null(x) && !is.null(y))) {
    position <- "none"
  } else {
    position <- position[1]
  }

  vjust  <- vjust %||% element$vjust
  hjust  <- hjust %||% element$hjust

  # Setup text-related graphical paramters
  gp <- gpar(
    fontsize = size, fontfamily = family,
    fontface = face, lineheight = lineheight
  )
  element_gp <- gpar(
    fontsize = element$size, fontfamily = element$family,
    fontface = element$face, lineheight = element$lineheight
  )
  for (i in names(gp)) element_gp[i] <- gp[i]
  gp <- element_gp

  # We set a temporary viewport so that text-related sizes are calculated
  # correctly relative to the font size
  grid::pushViewport(grid::viewport(gp = gp), recording = FALSE)
  on.exit(grid::popViewport(recording = FALSE))

  margin   <- margin %||% element$margin
  x_margin <- if (margin_x) width_cm(margin[c(2, 4)])  else c(0, 0)
  y_margin <- if (margin_y) height_cm(margin[c(1, 3)]) else c(0, 0)

  box.padding <- height_cm(to_unit(element$box.padding %||% 0.25))
  max_width   <- max(width_cm(stringWidth(label)))   + sum(x_margin) + box.padding
  max_height  <- max(height_cm(stringHeight(label))) + sum(y_margin) + box.padding

  # Set position dependent defaults
  direction <- switch(position, left = , right = "y", top = , bottom = "x", "both")
  vp <- switch(
    direction,
    x = grid::viewport(width = unit(1, "npc"), height = unit(max_height, "cm")),
    y = grid::viewport(width = unit(max_width, "cm"), height = unit(1, "npc")),
    both = grid::viewport(width = unit(1, "npc"), height = unit(1, "npc"))
  )

  x <- x %||% switch(position, right = 0, left = 1, hjust)
  y <- y %||% switch(position, bottom = 1, top = 0, vjust)
  x_nudge <- switch(position, left = -x_margin[1], right = x_margin[2], 0)
  y_nudge <- switch(position, top = y_margin[2], bottom = -y_margin[1], 0)
  x_nudge <- x_nudge / max_width
  y_nudge <- y_nudge / max_height

  # Set defaults
  arg_names <- rlang::fn_fmls_names(element_grob.element_text_repel)
  defaults <- GeomTextRepel$use_defaults(NULL)
  defaults <- defaults[setdiff(names(defaults), c(arg_names, "fontface"))]
  both <- intersect(names(defaults), names(element)[lengths(element) > 0])
  defaults[both] <- element[both]

  data <- rlang::inject(data.frame(
    label = label,
    colour     = colour %||% element$colour,
    angle      = angle  %||% element$angle,
    size       = gp$fontsize / .pt,
    family     = gp$fontfamily,
    fontface   = names(gp$font),
    lineheight = gp$lineheight,
    hjust      = hjust,
    vjust      = vjust,
    segment.colour = element$segment.colour %||% colour %||% element$colour,
    point.size = 0,
    !!!defaults,
    nudge_x = x_nudge,
    nudge_y = y_nudge
  ))

  # We cannot declare x/y in the data.frame directly because if they are units,
  # data.frame might because of the lack of an as.data.frame.unit method.
  data$x <- x
  data$y <- y

  gTree(
    limits = data.frame(x = c(0, 1), y = c(0, 1)),
    data = data,
    lab = label,
    direction = direction,
    box.padding = unit(box.padding, "cm"),
    point.padding = to_unit(sqrt(.Machine$double.eps)),
    min.segment.length = to_unit(0),
    arrow = element$arrow,
    force = element$force %||% 1,
    force_pull = element$force_pull %||% 1,
    max.time = element$max.time %||% 0.5,
    max.iter = element$max.iter %||% 1000,
    max.overlaps = element$max.overlaps %||% getOption("ggrepel.max.overlaps", default = 10),
    seed = element$seed %||% NA,
    verbose = FALSE,
    width  = unit(max_width, "cm"),
    height = unit(max_height, "cm"),
    vp = vp,
    cl = c("element_textrepeltree", "textrepeltree")
  )
}

# Helper funcions
width_cm  <- function(x) convertWidth(x, "cm", valueOnly = TRUE)
height_cm <- function(x) convertHeight(x, "cm", valueOnly = TRUE)

#' @export
#' @importFrom grid widthDetails
#' @method widthDetails element_textrepeltree
widthDetails.element_textrepeltree <- function(x) x$width
#' @export
#' @importFrom grid heightDetails
#' @method heightDetails element_textrepeltree
heightDetails.element_textrepeltree <- function(x) x$height
