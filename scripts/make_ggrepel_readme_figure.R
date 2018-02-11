optimize_png <- function(filename) {
  optimized <- sub(".png", "-fs8.png", filename)
  command <- sprintf(
    "pngquant --speed=1 --quality=0-2 -f %s && mv -f %s %s",
    filename, optimized, filename
  )
  system(command)
}

library(ggrepel)
set.seed(42)
p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_text_repel(
    size = 1.75,
    segment.size = 0.2,
    box.padding = 0.13, max.iter = 1e4
  ) +
  geom_point(color = 'red', size = 0.5) +
  theme_classic(base_size = 7) +
  theme(
    axis.line = element_line(size = 0.2),
    axis.ticks = element_line(size = 0.2)
  )
ggsave(filename = "fig.png", plot = p, width = 4, height = 2.5)
optimize_png("fig.png")
file.info("fig.png")$size
