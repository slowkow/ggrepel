# It should be possible to pass some arguments as either unit() objects or as
# numbers. Numbers should be converted to unit(n, "lines"), while unit() objects
# should remain unchanged.
# A combination of units for some arguments and numbers for others should be
# possible.
#
# These arguments are:
# box.padding         (both)
# label.padding       (geom_label_repel)
# point.padding       (both)
# label.r             (geom_label_repel)
# min.segment.length  (both)
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

test_that("non-default values as units work with geom_label_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, label = b)) + geom_point() +
    geom_label_repel(box.padding = unit(1, "lines"),
      label.padding = unit(2, "lines"),
      point.padding = unit(3, "lines"), label.r = unit(4, "lines"),
      min.segment.length = unit(5, "lines"))

  expect_identical(extract_param(p, "box.padding"), unit(1, "lines"))
  expect_identical(extract_param(p, "label.padding"), unit(2, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(3, "lines"))
  expect_identical(extract_param(p, "label.r"), unit(4, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(5, "lines"))
})

test_that("non-default values as units work with geom_text_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() +
    geom_text_repel(box.padding = unit(1, "lines"),
      point.padding = unit(2, "lines"),
      min.segment.length = unit(3, "lines"))

  expect_identical(extract_param(p, "box.padding"), unit(1, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(2, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(3, "lines"))
})

test_that("default values as numbers work with geom_label_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, label = b)) + geom_point() +
    geom_label_repel(box.padding = 0.25, label.padding = 0.25,
      point.padding = 1e-06, label.r = 0.15, min.segment.length = 0.5)

  expect_identical(extract_param(p, "box.padding"), unit(0.25, "lines"))
  expect_identical(extract_param(p, "label.padding"), unit(0.25, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(1e-06, "lines"))
  expect_identical(extract_param(p, "label.r"), unit(0.15, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(0.5, "lines"))
  expect_equal(p,
    ggplot(d, aes(x, y, label = b)) + geom_point() + geom_label_repel())
})

test_that("default values as numbers work with geom_text_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() +
    geom_text_repel(box.padding = 0.25, point.padding = 1e-06,
      min.segment.length = 0.5)

  expect_identical(extract_param(p, "box.padding"), unit(0.25, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(1e-06, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(0.5, "lines"))
  expect_equal(p,
    ggplot(d, aes(x, y, text = b)) + geom_point() + geom_text_repel())
})

test_that("non-default values as numbers work with geom_label_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, label = b)) + geom_point() +
    geom_label_repel(box.padding = 1, label.padding = 2,
      point.padding = 3, label.r = 4, min.segment.length = 5)

  expect_identical(extract_param(p, "box.padding"), unit(1, "lines"))
  expect_identical(extract_param(p, "label.padding"), unit(2, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(3, "lines"))
  expect_identical(extract_param(p, "label.r"), unit(4, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(5, "lines"))
  expect_equal(p, ggplot(d, aes(x, y, label = b)) + geom_point() +
      geom_label_repel(box.padding = unit(1, "lines"),
        label.padding = unit(2, "lines"),
        point.padding = unit(3, "lines"), label.r = unit(4, "lines"),
        min.segment.length = unit(5, "lines")))
})

test_that("non-default values as numbers work with geom_text_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() +
    geom_text_repel(box.padding = 1, point.padding = 2,
      min.segment.length = 3)

  expect_identical(extract_param(p, "box.padding"), unit(1, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(2, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(3, "lines"))
  expect_equal(p, ggplot(d, aes(x, y, text = b)) + geom_point() +
      geom_text_repel(box.padding = unit(1, "lines"),
        point.padding = unit(2, "lines"),
        min.segment.length = unit(3, "lines")))
})

test_that("non-default values as mix of units and numbers work with geom_label_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, label = b)) + geom_point() +
    geom_label_repel(box.padding = unit(1, "lines"), label.padding = 2,
      point.padding = 3, label.r = unit(4, "lines"), min.segment.length = 5)

  expect_identical(extract_param(p, "box.padding"), unit(1, "lines"))
  expect_identical(extract_param(p, "label.padding"), unit(2, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(3, "lines"))
  expect_identical(extract_param(p, "label.r"), unit(4, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(5, "lines"))
  expect_equal(p, ggplot(d, aes(x, y, label = b)) + geom_point() +
      geom_label_repel(box.padding = unit(1, "lines"),
        label.padding = unit(2, "lines"),
        point.padding = unit(3, "lines"), label.r = unit(4, "lines"),
        min.segment.length = unit(5, "lines")))
})

test_that("non-default values as mix of units and numbers work with geom_text_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() +
    geom_text_repel(box.padding = 1, point.padding = unit(2, "lines"),
      min.segment.length = 3)

  expect_identical(extract_param(p, "box.padding"), unit(1, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(2, "lines"))
  expect_identical(extract_param(p, "min.segment.length"), unit(3, "lines"))
  expect_equal(p, ggplot(d, aes(x, y, text = b)) + geom_point() +
      geom_text_repel(box.padding = unit(1, "lines"),
        point.padding = unit(2, "lines"),
        min.segment.length = unit(3, "lines")))
})

test_that("non-line units work with geom_label_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, label = b)) + geom_point() +
    geom_label_repel(box.padding = unit(1, "cm"), label.padding = 2,
      point.padding = 3, label.r = unit(4, "cm"), min.segment.length = 5)

  expect_identical(extract_param(p, "box.padding"), unit(1, "cm"))
  expect_identical(extract_param(p, "label.padding"), unit(2, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(3, "lines"))
  expect_identical(extract_param(p, "label.r"), unit(4, "cm"))
  expect_identical(extract_param(p, "min.segment.length"), unit(5, "lines"))
})

test_that("non-line units work with geom_text_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() +
    geom_text_repel(box.padding = 1, point.padding = unit(2, "cm"),
      min.segment.length = 3)

  expect_identical(extract_param(p, "box.padding"), unit(1, "lines"))
  expect_identical(extract_param(p, "point.padding"), unit(2, "cm"))
  expect_identical(extract_param(p, "min.segment.length"), unit(3, "lines"))
})

test_that("returns NA, not unit(NA, 'lines') when given NA in geom_label_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() +
    geom_label_repel(box.padding = NA)

  # returns TRUE even is unit(NA, "lines")
  expect_true(is.na(extract_param(p, "box.padding")))
  # returns TRUE for NA, but not unit(NA, "lines")
  expect_true(class(extract_param(p, "box.padding")) != "unit")
})

test_that("returns NA, not unit(NA, 'lines') when given NA in geom_text_repel", {
  d <- data.frame(x = rnorm(10), y = rnorm(10), b = letters[1:10])
  p <- ggplot(d, aes(x, y, text = b)) + geom_point() +
    geom_text_repel(box.padding = NA)

  # returns TRUE even is unit(NA, "lines")
  expect_true(is.na(extract_param(p, "box.padding")))
  # returns TRUE for NA, but not unit(NA, "lines")
  expect_true(class(extract_param(p, "box.padding")) != "unit")
})

