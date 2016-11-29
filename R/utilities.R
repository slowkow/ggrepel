"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Return a boolean vector of non-empty items.
#'
#' @param xs Vector with a mix of "expression" items, "character" items,
#'  and items from other classes.
#' @return Boolean vector indicating which items are not empty.
#' @noRd
not_empty <- function(xs) {
  sapply(seq_along(xs), function(i) {
    if (is.expression(xs[i])) {
      return(length(nchar(xs[i])) > 0)
    } else {
      return(xs[i] != "")
    }
  })
}
