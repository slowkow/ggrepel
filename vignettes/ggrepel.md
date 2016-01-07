---
title: "ggrepel"
author: "Kamil Slowikowski"
date: "2016-01-07"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# ggrepel

## Motivation

Some text labels overlap each other in plots created with [geom_text]:


```r
library(ggplot2)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text(aes(wt, mpg, label = rownames(mtcars))) +
  theme_classic(base_size = 16)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text-1.png" title="" alt="" width="700" />

## Algorithm

`ggrepel` implements functions to repel overlapping text labels away from
each other and away from the data points that they label. The algorithm
works as follows:

- For each box:
    - Move the box into the allowed plotting area.
    - If the bounding box overlaps other boxes:
        - Repel the overlapping boxes from each other.
    - If the bounding box overlaps data points:
        - Repel the box away from the data points.
- Repeat until all overlaps are resolved, up to a preset limit
  of iterations.

## Usage Examples

### geom_text_repel

We can repel the text labels away from each other by loading `ggrepel` and
using `geom_text_repel` instead:


```r
library(ggrepel)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text_repel(aes(wt, mpg, label = rownames(mtcars))) +
  theme_classic(base_size = 16)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel-1.png" title="" alt="" width="700" />

#### Options

All options available for [geom_text] such as `size` and
`fontface` are also available for `geom_text_repel`.

However, the following parameters are not supported:

- `hjust`
- `vjust`
- `nudge_x`
- `nudge_y`
- `position`
- `check_overlap`

`ggrepel` provides additional parameters for `geom_text_repel` and `geom_label_repel`:

- `segment.color` is the line segment color
- `box.padding` is the padding surrounding the text bounding box
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
    box.padding = unit(0.3, 'lines'),
    force = 2,
    max.iter = 1e4,
    expand = TRUE
  ) +
  scale_color_discrete(name = 'cyl') +
  theme_classic(base_size = 16)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel_options-1.png" title="plot of chunk geom_text_repel_options" alt="plot of chunk geom_text_repel_options" width="700" />

### geom_label_repel

`geom_label_repel` is based on [geom_label].


```r
library(ggrepel)
set.seed(100)
ggplot(mtcars) +
  geom_point(aes(wt, mpg)) +
  geom_label_repel(
    aes(wt, mpg, fill = factor(cyl), label = rownames(mtcars)),
    fontface = 'bold', color = 'white',
    # box.padding = unit(0.5, "lines"),
    # label.padding = unit(0.5, "lines")
    box.padding = unit(0.25, "lines")
  ) +
  theme_classic(base_size = 16)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_label_repel-1.png" title="plot of chunk geom_label_repel" alt="plot of chunk geom_label_repel" width="700" />

[geom_text]: http://docs.ggplot2.org/current/geom_text.html
[geom_label]: http://docs.ggplot2.org/current/geom_text.html
