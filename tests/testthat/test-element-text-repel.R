test_that("element_text_repel positions are interpreted correctly", {

  # Unit calculations require an active device
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  withr::defer({
    dev.off()
    unlink(tmp)
  })

  columns <- c("x", "y", "nudge_x", "nudge_y")

  examplar <- calc_element("text", theme_get())
  element <- element_text_repel(margin = margin(1, 1, 1, 1, "cm"))
  el <- merge_element(element, examplar)

  grob <- element_grob(el, label = "foo")
  expect_s3_class(grob, "titleGrob")

  grob <- element_grob(el, label = "foo", x = 0.25)
  expect_s3_class(grob, "element_textrepeltree")
  data <- unlist(grob$data[columns], use.names = FALSE)
  expect_equal(data, c(0.25, 1, 0, 0))

  grob <- element_grob(el, label = "foo", y = 0.25)
  data <- unlist(grob$data[columns], use.names = FALSE)
  expect_equal(data, c(1, 0.25, 0, 0))

  # Create new element with position = "right" (can't modify S7 repel property)
  el_right <- element_text_repel(margin = margin(1, 1, 1, 1, "cm"), position = "right")
  el_right <- merge_element(el_right, examplar)
  grob <- element_grob(el_right, label = "foo", y = 0.25)
  data <- unlist(grob$data[columns], use.names = FALSE)
  expect_equal(data, c(0, 0.25, 0, 0))

  # Create new element with position = "top"
  el_top <- element_text_repel(margin = margin(1, 1, 1, 1, "cm"), position = "top")
  el_top <- merge_element(el_top, examplar)
  grob <- element_grob(el_top, label = "foo", x = 0.25)
  data <- unlist(grob$data[columns], use.names = FALSE)
  expect_equal(data, c(0.25, 0, 0, 0))

})

test_that("element_text_repel renders as expected", {

  x <- c(0, 4.9, 5, 5.1, 10)
  labels <- c("Lorem ipsum", "dolor amet", "consectetur", "adipiscing", "elit")

  p <- ggplot(mapping = aes(x, x, colour = x, shape = labels)) +
    geom_point() +
    scale_x_continuous(breaks = x, labels = labels) +
    scale_y_continuous(breaks = x, labels = labels) +
    scale_colour_viridis_c(breaks = x, labels = labels) +
    guides(x.sec = "axis", y.sec = "axis") +
    theme(
      axis.text.x.bottom = element_text_repel(
        margin = margin(t = 10),
        colour = "dodgerblue",
        seed = 42
      ),
      axis.text.y.left = element_text_repel(
        margin = margin(r = 10),
        segment.colour = "dodgerblue",
        seed = 42
      ),
      axis.text.x.top = element_text_repel(
        margin = margin(b = 10), position = "top",
        arrow = arrow(length = unit(2, "mm")),
        seed = 42
      ),
      axis.text.y.right = element_text_repel(
        margin = margin(l = 10), position = "right",
        segment.curvature = 0.1, segment.inflect = TRUE,
        seed = 42
      ),
      legend.text = element_text_repel(
        margin = margin(l = 10), position = "right",
        segment.linetype = "dotted", colour = "dodgerblue",
        seed = 42
      )
    )

  vdiffr::expect_doppelganger("element_text_repel", p)
})
