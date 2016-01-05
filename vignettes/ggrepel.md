

# ggrepel

## geom_text

If we plot the data from `mtcars` with [geom_text], some of the text labels
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

We can adjust all the options available for [geom_text] such as `size` and
`fontface`.

We also have additional options for `geom_text_repel` and `geom_label_repel`:

- `segment.color` is the line segment color
- `label.padding` is the padding surrounding the text
- `force` is the force of repulsion between overlapping text labels
- `max.iter` is the maximum number of iterations to attempt to resolve overlaps
- `expand` the text will be arranged in the expanded plot area if TRUE, or else
  the text will be arranged within the range of the data points


```r
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text_repel(
    aes(
      wt, mpg,
      color = factor(cyl),
      label = rownames(mtcars)
    ),
    size = 5,
    fontface = 'bold',
    segment.color = 'red',
    label.padding = unit(0.3, 'lines'),
    force = 2,
    max.iter = 1e4,
    expand = TRUE
  ) +
  scale_color_discrete(name = 'cyl') +
  theme_classic(base_size = 16)
```

![plot of chunk geom_text_repel_options](https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel_options-1.png) 

## geom_label_repel

`ggrepel` includes `geom_label_repel`, based on [geom_label], to repel
rectangular labels:


```r
library(ggrepel)
ggplot(mtcars) +
  geom_point(aes(wt, mpg)) +
  geom_label_repel(
    aes(wt, mpg, fill = factor(cyl), label = rownames(mtcars)),
    fontface = 'bold', color = 'white'
  ) +
  theme_classic(base_size = 16)
```

![plot of chunk geom_label_repel](https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_label_repel-1.png) 

[geom_text]: http://docs.ggplot2.org/current/geom_text.html
[geom_label]: http://docs.ggplot2.org/current/geom_text.html
