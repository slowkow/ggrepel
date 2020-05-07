get_boxes = function(
  x, y, labels, adj, pos, offset, vfont, cex, font
) {
  # par('ps') appears to be handled by strwidth
  delx = strwidth(labels, cex = cex, font = font, vfont = vfont)
  dely = strheight(labels, cex = cex, font = font, vfont = vfont)

  # not completely satisfactory, but should suffice:
  #  https://stackoverflow.com/questions/61651361
  cbind(
    x1 = x - adj[1L]*delx, y1 = y - adj[2L]*dely,
    x2 = x + (1-adj[1L])*delx, y2 = y + (1-adj[2L])*dely
  )
}

repel_text = function(
  x, y, labels, adj = NULL, pos = NULL, offset = .5,
  vfont = NULL, cex = 1, col = NULL, font = NULL,
  point.padding = 0, force = 1, force_pull = 1,
  max.time = .5, max.iter = 10000L, max.overlaps = 10L,
  direction = c('both', 'y', 'x'), ...
) {
  if (!missing(pos)) stop("Not yet implemented")
  if (!missing(offset)) stop("Not yet implemented")

  direction = match.arg(direction)

  # from src/library/graphics/plot.c:C_text, setting pos is a shortcut
  #   for setting adj: 1->[.5, .5]; 2->[1, 0]; 3->[.5, 0]; 4->[0, 0];
  #   application of offset is also as there
  #   [I'm ignoring 'dd->dev->yCharOffset' which AFAICT is not exposed to
  #    R outside C, and which is described as "mysterious" in R-ints manual]
  if (is.null(pos)) {
    # will grow adj if adj is length-1
    if (is.null(adj)) adj = c(par('adj'), NA_real_)
    if (length(adj) == 1L) adj[2L] = .5
    if (anyNA(adj)) adj[is.na(adj)] = .5
  } else {
    pos = as.intger(pos)
    offset = offset * par('cxy')
    if (length(pos) > 1L) stop("'pos' must have length one")
    if (pos == 1L) {
      y = y - offset
      adj = c(.5, .5)
    } else if (pos == 2L) {
      x = x - offset
      adj = c(1, 0)
    } else if (pos == 3L) {
      y = y + offset
      adj = c(.5, 0)
    } else if (pos == 4L) {
      x = x + offset
      adj = c(0, 0)
    } else stop("Invalid value for 'pos' [",pos,"]; valid values are 1,2,3,4")
  }

  repel = repel_boxes2(
    data_points     = cbind(x, y),
    point_size      = strheight('m', cex = cex, font = font, vfont = vfont),
    point_padding_x = point.padding,
    point_padding_y = point.padding,
    boxes           = get_boxes(x, y, labels, adj, pos, offset, vfont, cex, font),
    xlim            = lims[1:2],
    ylim            = lims[3:4],
    hjust           = adj[1L],
    vjust           = adj[2L],
    force_push      = force * 1e-6,
    force_pull      = force_pull * 1e-2,
    max_time        = max.time,
    max_iter        = max.iter,
    max_overlaps    = max.overlaps,
    direction       = direction
  )

  text(repel$x, repel$y, labels, adj = adj, pos = pos, offset = offset,
       vfont = vfont, cex = cex, col = col, font = font)

  segments(x, y, repel$x, repel$y)
}
