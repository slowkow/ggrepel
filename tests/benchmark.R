library(microbenchmark)

microbenchmark(
  geom_text_repel = {
    p <- ggplot(mtcars) +
      geom_point(aes(wt, mpg), color = 'red') +
      geom_text_repel(aes(wt, mpg, label = rownames(mtcars))) +
      theme_classic(base_size = 16)
    print(p)
  },
  geom_text = {
    p <- ggplot(mtcars) +
      geom_point(aes(wt, mpg), color = 'red') +
      geom_text(aes(wt, mpg, label = rownames(mtcars))) +
      theme_classic(base_size = 16)
    print(p)
  },
  times = 10L
)
