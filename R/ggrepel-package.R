#' ggrepel
#'
#' This package contains extra geoms for \pkg{ggplot2}.
#'
#' Please see the help pages listed below:
#'
#' \itemize{
#'   \item \code{\link{geom_text_repel}}
#'   \item \code{\link{geom_label_repel}}
#' }
#'
#' Also see the vignette for more usage examples:
#'
#' \code{browseVignettes("ggrepel")}
#'
#' Please report issues and suggest improvements at Github:
#'
#' \url{https://github.com/slowkow/ggrepel}
#'
#' @name ggrepel
#' @docType package
#' @import ggplot2
#' @import Rcpp
#' @importFrom grid
#'   convertHeight
#'   convertWidth
#'   gList
#'   gTree
#'   gpar
#'   grobHeight
#'   grobName
#'   grobTree
#'   grobWidth
#'   grobX
#'   grobY
#'   stringHeight
#'   stringWidth
#'   is.grob
#'   is.unit
#'   unit
#'   makeContent
#'   resolveHJust
#'   resolveVJust
#'   roundrectGrob
#'   segmentsGrob
#'   curveGrob
#'   setChildren
#'   textGrob
#' @importFrom rlang
#'   warn
#' @useDynLib ggrepel
#' @keywords internal
"_PACKAGE"
