# The side aesthetic attaches line segments to one side of the label:
#
#   ggplot(...) + geom_text_repel(..., side = "left")
#
context("side")

# A box spanning x from 0 to 2 and y from 0 to 1.
box <- c(0, 0, 2, 1)

test_that("side = 0 attaches to the closest side of the box", {
  # Point below the box attaches to the bottom.
  expect_equal(select_line_connection(c(1, -3), box), c(1, 0))
  # Point left of the box attaches to the left.
  expect_equal(select_line_connection(c(-3, 0.5), box), c(0, 0.5))
})

test_that("side forces the segment to attach to that side", {
  p <- c(-3, -3)
  expect_equal(select_line_connection(p, box, side = 1)[2], 1)
  expect_equal(select_line_connection(p, box, side = 2)[1], 2)
  expect_equal(select_line_connection(p, box, side = 3)[2], 0)
  expect_equal(select_line_connection(p, box, side = 4)[1], 0)
})

test_that("side is forced even when the point is level with the box", {
  # Point directly below the box.
  expect_equal(select_line_connection(c(1, -3), box, side = 2)[1], 2)
  expect_equal(select_line_connection(c(1, -3), box, side = 4)[1], 0)
  # Point directly left of the box.
  expect_equal(select_line_connection(c(-3, 0.5), box, side = 1)[2], 1)
  expect_equal(select_line_connection(c(-3, 0.5), box, side = 3)[2], 0)
})

test_that("attachment point is finite and on the box for every side", {
  # Includes points on the edge of the box and inside it.
  points <- list(
    c(1, -3), c(-3, 0.5), c(-3, -3), c(5, 5), c(2, 0.5), c(1, 1), c(1, 0.5)
  )
  for (p in points) {
    for (side in 0:4) {
      out <- select_line_connection(p, box, side = side)
      expect_true(all(is.finite(out)))
      expect_true(out[1] >= 0 && out[1] <= 2 && out[2] >= 0 && out[2] <= 1)
    }
  }
})

test_that("side names are converted to numbers", {
  expect_equal(
    compute_side(c("top", "right", "bottom", "left")), c(1, 2, 3, 4)
  )
  expect_equal(compute_side(factor(c("left", "top"))), c(4, 1))
  expect_warning(out <- compute_side(c("left", "Left")), "Ignoring unknown")
  expect_equal(out, c(4, 0))
})

test_that("side can be set or mapped in geom_text_repel and geom_label_repel", {
  dat <- data.frame(
    x = 1:4, y = 1:4, label = letters[1:4],
    side = c("top", "right", "bottom", "left")
  )
  png_file <- withr::local_tempfile(pattern = "testthat_test-side")
  png(png_file)
  for (geom in list(geom_text_repel, geom_label_repel)) {
    p <- ggplot(dat, aes(x, y, label = label))
    expect_silent(print(p + geom(side = "left", seed = 1)))
    expect_silent(print(p + geom(side = 2, seed = 1)))
    expect_silent(print(p + geom(aes(side = side), seed = 1)))
    expect_warning(
      print(p + geom(side = "nonsense", seed = 1)), "Ignoring unknown"
    )
  }
  dev.off()
})
