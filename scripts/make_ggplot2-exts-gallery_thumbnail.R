library(ggrepel)

df <- subset(mtcars, wt > 2.75 & wt < 3.45)
df$label <- rownames(df)
df$cyl <- factor(df$cyl)

ggplot(df, aes(wt, mpg)) +
  geom_point(color = 'red') +
  geom_text_repel(aes(label = label)) +
  theme_classic(base_size = 12)

set.seed(42)
ggsave("ggrepel.png", width = 4, height = 3, units = "in", dpi = 300)
system("pngquant -f ggrepel.png && mv -f ggrepel-fs8.png ggrepel.png")
