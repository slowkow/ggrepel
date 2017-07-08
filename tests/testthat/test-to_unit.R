context("to_unit")

# Given a ggplot where the second layer is the geom_*_repel layer,
# return the given param
extract_param <- function(pl, param) {
  pl$layers[[2]]$geom_params[[param]]
}

test_that("defaults work with geom_label_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, label = b)) + geom_point() + geom_label_repel()

  expect_identical(extract_param(p, "box.padding"), unit(0.25, "lines"))
  expect_identical(extract_param(p, "label.padding"), unit(0.25, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(1e-06, "lines"))
  expect_identical(extract_param(p, "label.r"), unit(0.15, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(0.5, "lines"))
})

test_that("defaults work with geom_text_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() + geom_text_repel()

  expect_identical(extract_param(p, "box.padding"), unit(0.25, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(1e-06, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(0.5, "lines"))
})
