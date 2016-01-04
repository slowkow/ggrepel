

# ggrepel

## geom_text

If we plot the data from `mtcars` with `geom_text`, some of the text labels
overlap each other:


```r
library(ggplot2)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text(aes(wt, mpg, label = rownames(mtcars))) +
  theme_classic(base_size = 16)
```

![plot of chunk geom_text](https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text-1.png) 

## geom_text_repel

We can repel the text labels away from each other by loading `ggrepel` and
using `geom_text_repel` instead:


```r
library(ggrepel)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text_repel(aes(wt, mpg, label = rownames(mtcars))) +
  theme_classic(base_size = 16)
```

![plot of chunk geom_text_repel](https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel-1.png) 

### Options

We can adjust all the options available for `geom_text` such as `size` and
`fontface`. We can also change the line segment color, the padding surrounding
each text label, the force of repulsion between overlapping text labels, and
the maximum number of iterations to attempt to resolve the overlap.


```r
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text_repel(
    aes(wt, mpg, label = rownames(mtcars)),
    size = 5,
    fontface = 'bold',
    segment.color = 'red',
    label.padding = unit(0.5, "lines"),
    force = 2,
    max.iter = 1e4
  ) +
  theme_classic(base_size = 16)
```

![plot of chunk geom_text_repel_options](https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel_options-1.png) 
