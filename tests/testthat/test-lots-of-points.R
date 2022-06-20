context("lots-of-points")

test_that("geom_text_repel works with 10,000 points and 32 labels", {

  cars <- mtcars
  cars$car <- rownames(mtcars)

  set.seed(42)
  dat3 <- rbind(
    data.frame(
      wt  = rnorm(n = 10000, mean = 3),
      mpg = rnorm(n = 10000, mean = 19),
      car = ""
    ),
    cars[,c("wt", "mpg", "car")]
  )

  p <- ggplot(dat3, aes(wt, mpg, label = car)) +
    geom_point(data = dat3[dat3$car == "",], color = "grey50") +
    geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
    geom_point(data = dat3[dat3$car != "",], color = "red")

  png_name <- withr::local_tempfile(pattern = "testthat_test-lots-of-points-1")
  png(png_name)
  print(p)
  dev.off()

  # If we got here, then the plot was created successfully.
  expect_true(TRUE)
})
