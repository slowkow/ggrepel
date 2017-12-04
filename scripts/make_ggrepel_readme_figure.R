library(ggrepel)
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_text_repel(
    size = 1.75,
    segment.size = 0.2,
    box.padding = 0.13, max.iter = 1e4
  ) +
  geom_point(color = 'red', size = 0.5) +
  theme_classic(base_size = 7)
ggsave("fig.png", width = 4, height = 2.5)
system("pngquant -f fig.png && mv -f fig-fs8.png fig.png")
