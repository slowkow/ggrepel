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
  p1 <- ggplot(dat1) + geom_label_repel(aes(wt, mpg, label = label), max.overlaps = Inf)
  print(p1)
  grid.force()
  grobnames <- names(grid.get(
    gPath = "labelrepeltree", grep = TRUE, global = TRUE
  )$children)
  dev.off()
  unlink("testthat_test-grob-order1.png")

  isrect <- startsWith(grobnames, "rectrepelgrob")
  istext <- startsWith(grobnames, "textrepelgrob")
  ix_rect <- which(isrect)
  ix_text <- which(istext)

  # Confirm that number of segment grobs is equal to number of text grobs.
  expect_true(length(ix_rect) == length(ix_text))

  rectnames <- grobnames[isrect]
  textnames <- grobnames[istext]
  ix_rect_ordered <- ix_rect[order(rectnames)]
  ix_text_ordered <- ix_text[order(textnames)]

  # Confirm that a rect grob always appears before its corresponding text grob
  expect_true(all(ix_rect_ordered < ix_text_ordered))

  ix_segment_max <- max(which(startsWith(grobnames, "segmentrepelgrob")))
  ix_rect_min    <- min(ix_rect)
  ix_text_min    <- min(ix_text)

  # Confirm that all segment grobs appear before rect and text grobs.
  expect_true(ix_segment_max < ix_text_min)
  expect_true(ix_segment_max < ix_rect_min)
})
