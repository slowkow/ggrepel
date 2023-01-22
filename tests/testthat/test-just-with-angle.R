context("geom_text_repel_just")

my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
my.cars$car.names <- rownames(my.cars)

p <- ggplot(my.cars, aes(wt, mpg, label = car.names)) +
  geom_point(colour = "red") +
  expand_limits(x = c(1, 7), y = c(12, 24))

test_that("center with rotation", {
  vdiffr::expect_doppelganger("geom_text_repel_center_0",
                              p +
                                geom_text_repel(vjust = 0.5,
                                                hjust = 0.5,
                                                angle = 0,
                                                max.iter = 0,
                                                seed = 1234)
  )

  # currently not quite the expected plot
  # triggers rlang::warn()
  # vdiffr::expect_doppelganger("geom_text_repel_center_45",
  #                             p +
  #                               geom_text_repel(vjust = 0.5,
  #                                               hjust = 0.5,
  #                                               angle = 45,
  #                                               max.iter = 0,
  #                                               seed = 1234)
  # )

  vdiffr::expect_doppelganger("geom_text_repel_center_90",
                              p +
                                geom_text_repel(vjust = 0.5,
                                                hjust = 0.5,
                                                angle = 90,
                                                max.iter = 0,
                                                seed = 1234)
  )

  vdiffr::expect_doppelganger("geom_text_repel_center_180",
                              p +
                                geom_text_repel(vjust = 0.5,
                                                hjust = 0.5,
                                                angle = 180,
                                                max.iter = 0,
                                                seed = 1234)
  )
})

test_that("inward with rotation", {
  vdiffr::expect_doppelganger("geom_text_repel_inward_0",
                              p +
                                geom_text_repel(vjust = "inward",
                                                hjust = "inward",
                                                angle = 0,
                                                max.iter = 0,
                                                seed = 1234)
  )

  # currently not the expected plot
  # triggers rlang::warn()
  # vdiffr::expect_doppelganger("geom_text_repel_inward_45",
  #                             p +
  #                               geom_text_repel(vjust = "inward",
  #                                               hjust = "inward",
  #                                               angle = 45,
  #                                               max.iter = 0,
  #                                               seed = 1234)
  # )

  vdiffr::expect_doppelganger("geom_text_repel_inward_90",
                              p +
                                geom_text_repel(vjust = "inward",
                                                hjust = "inward",
                                                angle = 90,
                                                max.iter = 0,
                                                seed = 1234)
  )

  vdiffr::expect_doppelganger("geom_text_repel_inward_180",
                              p +
                                geom_text_repel(vjust = "inward",
                                                hjust = "inward",
                                                angle = 180,
                                                max.iter = 0,
                                                seed = 1234)
  )
})

test_that("outward with rotation", {
  vdiffr::expect_doppelganger("geom_text_repel_outward_0",
                              p +
                                geom_text_repel(vjust = "outward",
                                                hjust = "outward",
                                                angle = 0,
                                                max.iter = 0,
                                                seed = 1234)
  )

  # currently not the expected plot
  # vdiffr::expect_doppelganger("geom_text_repel_outward_45",
  #                             p +
  #                               geom_text_repel(vjust = "outward",
  #                                               hjust = "outward",
  #                                               angle = 45,
  #                                               max.iter = 0,
  #                                               seed = 1234)
  # )

  vdiffr::expect_doppelganger("geom_text_repel_outward_90",
                              p +
                                geom_text_repel(vjust = "outward",
                                                hjust = "outward",
                                                angle = 90,
                                                max.iter = 0,
                                                seed = 1234)
  )

  vdiffr::expect_doppelganger("geom_text_repel_outward_180",
                              p +
                                geom_text_repel(vjust = "outward",
                                                hjust = "outward",
                                                angle = 180,
                                                max.iter = 0,
                                                seed = 1234)
  )
})

test_that("zero one with rotation", {
  vdiffr::expect_doppelganger("geom_text_repel_zero_one_0",
                              p +
                                geom_text_repel(vjust = 0,
                                                hjust = 1,
                                                angle = 0,
                                                max.iter = 0,
                                                seed = 1234)
  )

  # currently not the expected plot
  # vdiffr::expect_doppelganger("geom_text_repel_zero_one_45",
  #                             p +
  #                               geom_text_repel(vjust = 0,
  #                                               hjust = 1,
  #                                               angle = 45,
  #                                               max.iter = 0,
  #                                               seed = 1234)
  # )

  vdiffr::expect_doppelganger("geom_text_repel_zero_one_90",
                              p +
                                geom_text_repel(vjust = 0,
                                                hjust = 1,
                                                angle = 90,
                                                max.iter = 0,
                                                seed = 1234)
  )

  vdiffr::expect_doppelganger("geom_text_repel_zero_one_180",
                              p +
                                geom_text_repel(vjust = 0,
                                                hjust = 1,
                                                angle = 180,
                                                max.iter = 0,
                                                seed = 1234)
  )
})

p1 <- ggplot(my.cars, aes(wt, mpg, label = gsub(" ", "\n", car.names))) +
  geom_point(colour = "red") +
  expand_limits(x = c(2, 6), y = c(13, 23))


test_that("other just with rotation", {
  vdiffr::expect_doppelganger("geom_text_repel_multiline_0",
                              p1 +
                                geom_text_repel(vjust = 0,
                                                hjust = 1,
                                                angle = 0,
                                                max.iter = 0,
                                                size = 2.5,
                                                seed = 1234)
  )

  # currently not the expected plot
  # vdiffr::expect_doppelganger("geom_text_repel_multiline_45",
  #                             p1 +
  #                               geom_text_repel(vjust = 0,
  #                                               hjust = 1,
  #                                               angle = 45,
  #                                               max.iter = 0,
  #                                               size = 2.5,
  #                                               seed = 1234)
  # )

  vdiffr::expect_doppelganger("geom_text_repel_multiline_90",
                              p1 +
                                geom_text_repel(vjust = 0,
                                                hjust = 1,
                                                angle = 90,
                                                max.iter = 0,
                                                size = 2.5,
                                                seed = 1234)
  )

  vdiffr::expect_doppelganger("geom_text_repel_multiline_180",
                              p1 +
                                geom_text_repel(vjust = 0,
                                                hjust = 1,
                                                angle = 180,
                                                max.iter = 0,
                                                size = 2.5,
                                                seed = 1234)
  )
})
