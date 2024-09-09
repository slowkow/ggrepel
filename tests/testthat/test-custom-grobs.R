test_that("geom_text_repel() works with custom grobs", {

  my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
  my.cars$car.names <- rownames(my.cars)

  p <- ggplot(my.cars, aes(wt, mpg, label = car.names)) +
    geom_point(colour = "red") +
    expand_limits(x = c(1, 7), y = c(12, 24))

  custom_rectangles <- function(label, x, y, hjust, vjust, ..., fill) {
    grid::rectGrob(
      x = x, y = y, width = unit(1, "cm"), height = unit(2, "cm"),
      hjust = hjust, vjust = vjust, gp = gpar(fill = fill, col = "black")
    )
  }

  p + geom_text_repel(
    min.segment.length = 0, box.padding = 1,
    grob = custom_rectangles, grob_args = list(fill = "gold")
  )

  vdiffr::expect_doppelganger(
    "geom_text_repel_rectangles",
    p + geom_text_repel(
      min.segment.length = 0, box.padding = 1,
      grob = custom_rectangles, grob_args = list(fill = "gold"),
      seed = 1234
    )
  )

  custom_circles <- function(label, x, y, ..., custom_gp) {
    grid::circleGrob(x = x, y = y, r = unit(1, "cm"), gp = custom_gp)
  }

  vdiffr::expect_doppelganger(
    "geom_text_repel_circles",
    p + geom_text_repel(
      min.segment.length = 0, box.padding = 1,
      grob = custom_circles,
      grob_args = list(
        custom_gp = gpar(fill = "red", col = "gold", linetype = 2, lwd = 2)
      ),
      seed = 1234
    )
  )
})
