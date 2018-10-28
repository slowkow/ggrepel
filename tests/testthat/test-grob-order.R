# Segment grobs should be underneath text and label grobs.
#
context("grob-order")

library(grid)

test_that("for geom_text_repel, all segment grobs come before text grobs", {
  dat1 <- mtcars
  dat1$label <- rownames(mtcars)

  # Make a plot with no seed and get the label positions.
  png("testthat_test-grob-order1.png")
  p1 <- ggplot(dat1) + geom_text_repel(aes(wt, mpg, label = label))
  print(p1)
  grid.force()
  grobnames <- names(grid.get(
    gPath = "textrepeltree", grep = TRUE, global = TRUE
  )$children)
  dev.off()
  unlink("testthat_test-grob-order1.png")

  ix_segment <- max(which(startsWith(grobnames, "segmentrepelgrob")))
  ix_text    <- min(which(startsWith(grobnames, "textrepelgrob")))

  # Confirm that all segment grobs appear before text grobs.
  expect_true(ix_segment < ix_text)
})

test_that("for geom_label_repel, all rect grobs come before text grobs", {
  dat1 <- mtcars
  dat1$label <- rownames(mtcars)

  # Make a plot with no seed and get the label positions.
  png("testthat_test-grob-order1.png")
  p1 <- ggplot(dat1) + geom_label_repel(aes(wt, mpg, label = label))
  print(p1)
  grid.force()
  grobnames <- names(grid.get(
    gPath = "labelrepeltree", grep = TRUE, global = TRUE
  )$children)
  dev.off()
  unlink("testthat_test-grob-order1.png")

  ix_segment <- max(which(startsWith(grobnames, "segmentrepelgrob")))
  ix_rect    <- min(which(startsWith(grobnames, "rectrepelgrob")))
  ix_text    <- min(which(startsWith(grobnames, "textrepelgrob")))

  # Confirm that all segment grobs appear before rect and text grobs.
  expect_true(ix_segment < ix_text)
  expect_true(ix_segment < ix_rect)
  # Confirm that all rect grobs appear before text grobs.
  expect_true(ix_rect < ix_text)
})
