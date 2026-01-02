#' @importFrom S7 new_class new_property class_any S7_class prop
NULL

# Cache environment for the S7 class
.element_text_repel_cache <- new.env(parent = emptyenv())

get_element_text_repel_class <- function() {
  if (is.null(.element_text_repel_cache$class)) {
    element_text_class <- S7::S7_class(ggplot2::element_text())
    # Simple approach: just add ONE property to store all repel settings
    # This avoids merge conflicts with element_text
    .element_text_repel_cache$class <- S7::new_class(
      name = "element_text_repel",
      parent = element_text_class,
      properties = list(
        repel = S7::new_property(S7::class_any, default = list())
      )
    )
  }
  .element_text_repel_cache$class
}

#' Repulsive text element
#'
#' This text element is a replacement for \code{\link[ggplot2]{element_text}}
#' that repulses labels.
#'
#' @inheritParams ggplot2::element_text
#' @inheritParams geom_text_repel
#' @param size Font size in points.
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
  colour <- color %||% colour

  # Store all repel-specific settings in a list
  repel_settings <- list(
    box.padding = box.padding,
    force = force,
    force_pull = force_pull,
    max.time = max.time,
    max.iter = max.iter,
    max.overlaps = max.overlaps,
    min.segment.length = min.segment.length,
    segment.colour = segment.colour,
    segment.linetype = segment.linetype,
    segment.size = segment.size,
    segment.curvature = segment.curvature,
    segment.angle = segment.angle,
    segment.ncp = segment.ncp,
    segment.shape = segment.shape,
    segment.square = segment.square,
    segment.squareShape = segment.squareShape,
    segment.inflect = segment.inflect,
    arrow = arrow,
    seed = seed,
    position = position[1]
  )

  # Get the S7 class constructor
  class_constructor <- get_element_text_repel_class()

  # Create the S7 object with standard element_text properties + repel list
  class_constructor(
    family = family,
    face = face,
    colour = colour,
    size = size,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    inherit.blank = inherit.blank,
    repel = repel_settings
  )
}

# Helper to get repel settings from element
get_repel_setting <- function(element, name, default = NULL) {
  repel <- S7::prop(element, "repel")
  repel[[name]] %||% default
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
    # S7 inheritance makes NextMethod() work!
    return(NextMethod())
  }
  if (is.null(label) || sum(nzchar(label) & !is.na(label)) < 1) {
    return(zeroGrob())
  }

  # Determine position based on which coordinate is provided
  position <- get_repel_setting(element, "position", "bottom")
  if (is.null(x)) {
    # Vertical axis: need left/right position
    valid_pos <- intersect(position, c("left", "right"))
    position <- if (length(valid_pos) > 0) valid_pos[1] else "left"
  } else if (is.null(y)) {
    # Horizontal axis: need top/bottom position
    valid_pos <- intersect(position, c("top", "bottom"))
    position <- if (length(valid_pos) > 0) valid_pos[1] else "bottom"
  } else {
    # Both coordinates provided - no specific positioning needed
    position <- "none"
  }

  vjust <- vjust %||% S7::prop(element, "vjust")
  hjust <- hjust %||% S7::prop(element, "hjust")

  # Setup text-related graphical parameters
  gp <- gpar(
    fontsize = size, fontfamily = family,
    fontface = face, lineheight = lineheight
  )
  element_gp <- gpar(
    fontsize = S7::prop(element, "size"),
    fontfamily = S7::prop(element, "family"),
    fontface = S7::prop(element, "face"),
    lineheight = S7::prop(element, "lineheight")
  )
  for (i in names(gp)) element_gp[i] <- gp[i]
  gp <- element_gp

  grid::pushViewport(grid::viewport(gp = gp), recording = FALSE)
  on.exit(grid::popViewport(recording = FALSE))

  margin <- margin %||% S7::prop(element, "margin")
  x_margin <- if (margin_x) width_cm(margin[c(2, 4)])  else c(0, 0)
  y_margin <- if (margin_y) height_cm(margin[c(1, 3)]) else c(0, 0)

  box.padding <- height_cm(to_unit(get_repel_setting(element, "box.padding", 0.25)))
  max_width   <- max(width_cm(stringWidth(label)))   + sum(x_margin) + box.padding
  max_height  <- max(height_cm(stringHeight(label))) + sum(y_margin) + box.padding

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

  # Set defaults from GeomTextRepel
  arg_names <- rlang::fn_fmls_names(element_grob.element_text_repel)
  defaults <- GeomTextRepel$use_defaults(NULL)
  defaults <- defaults[setdiff(names(defaults), c(arg_names, "fontface"))]

  # Override with repel settings from element
  repel <- S7::prop(element, "repel")
  repel_props <- c(
    "box.padding", "force", "force_pull", "max.time", "max.iter", "max.overlaps",
    "min.segment.length", "segment.colour", "segment.linetype", "segment.size",
    "segment.curvature", "segment.angle", "segment.ncp", "segment.shape",
    "segment.square", "segment.squareShape", "segment.inflect", "seed"
  )
  for (prop_name in repel_props) {
    if (!is.null(repel[[prop_name]])) {
      defaults[[prop_name]] <- repel[[prop_name]]
    }
  }
  # Remove arrow from defaults - it can't be stored in a data.frame
  arrow_setting <- repel[["arrow"]] %||% defaults[["arrow"]]
  defaults[["arrow"]] <- NULL

  element_colour <- S7::prop(element, "colour")
  element_angle <- S7::prop(element, "angle")
  segment_colour <- get_repel_setting(element, "segment.colour")

  data <- rlang::inject(data.frame(
    label = label,
    colour     = colour %||% element_colour,
    angle      = angle  %||% element_angle,
    size       = gp$fontsize / .pt,
    family     = gp$fontfamily,
    fontface   = names(gp$font),
    lineheight = gp$lineheight,
    hjust      = hjust,
    vjust      = vjust,
    segment.colour = segment_colour %||% colour %||% element_colour,
    point.size = 0,
    !!!defaults,
    nudge_x = x_nudge,
    nudge_y = y_nudge
  ))

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
    arrow = arrow_setting,
    force = get_repel_setting(element, "force", 1),
    force_pull = get_repel_setting(element, "force_pull", 1),
    max.time = get_repel_setting(element, "max.time", 0.5),
    max.iter = get_repel_setting(element, "max.iter", 1000),
    max.overlaps = get_repel_setting(element, "max.overlaps", getOption("ggrepel.max.overlaps", default = 10)),
    seed = get_repel_setting(element, "seed", NA),
    verbose = FALSE,
    width  = unit(max_width, "cm"),
    height = unit(max_height, "cm"),
    vp = vp,
    cl = c("element_textrepeltree", "textrepeltree")
  )
}

# Helper functions
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
