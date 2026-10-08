# Since ggplot2 4.0, geom_text() and geom_label() take default aesthetics such
# as family, size, and colour from the theme. The repel geoms should match them.
#
#   ggplot(...) + geom_text_repel(...) + theme_gray(base_family = "serif")
#
context("theme defaults")

custom_theme <- theme_gray(
  base_size = 16, base_family = "serif", ink = "darkorange", paper = "cornsilk"
)

p <- ggplot(mtcars[1, ], aes(wt, mpg, label = "a")) + custom_theme

test_that("geom_text_repel() uses the same theme defaults as geom_text()", {
  cols <- c("family", "size", "colour")
  expect_equal(
    layer_data(p + geom_text_repel())[cols],
    layer_data(p + geom_text())[cols]
  )
})

test_that("geom_label_repel() uses the same theme defaults as geom_label()", {
  cols <- c("family", "size", "colour", "fill", "linewidth", "linetype")
  expect_equal(
    layer_data(p + geom_label_repel())[cols],
    layer_data(p + geom_label())[cols]
  )
})
