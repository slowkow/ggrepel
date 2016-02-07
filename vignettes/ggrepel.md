---
title: "ggrepel Usage Examples"
author: "Kamil Slowikowski"
date: "2016-02-07"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{ggrepel Usage Examples}
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

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text-1.png" title="plot of chunk geom_text" alt="plot of chunk geom_text" width="700" />

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
set.seed(42)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text_repel(aes(wt, mpg, label = rownames(mtcars))) +
  theme_classic(base_size = 16)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel-1.png" title="plot of chunk geom_text_repel" alt="plot of chunk geom_text_repel" width="700" />

#### Options

All options available for [geom_text] such as `size` and
`fontface` are also available for `geom_text_repel`.

However, the following parameters are not supported:

- `hjust`
- `vjust`
- `position`
- `check_overlap`

`ggrepel` provides additional parameters for `geom_text_repel` and `geom_label_repel`:

- `segment.color` is the line segment color
- `box.padding` is the padding surrounding the text bounding box
- `point.padding` is the padding around the labeled point
- `arrow` is the specification for arrow heads created by `grid::arrow`
- `force` is the force of repulsion between overlapping text labels
- `max.iter` is the maximum number of iterations to attempt to resolve overlaps
- `nudge_x` is how much to shift the starting position of the text label along
  the x axis
- `nudge_x` is how much to shift the starting position of the text label along
  the y axis


```r
set.seed(42)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'grey', size = 4, shape = 15) +
  geom_text_repel(
    aes(
      wt, mpg,
      color = factor(cyl),
      label = rownames(mtcars)
    ),
    size = 5,
    fontface = 'bold',
    box.padding = unit(0.5, 'lines'),
    point.padding = unit(1.6, 'lines'),
    segment.color = '#555555',
    segment.size = 0.5,
    arrow = arrow(length = unit(0.01, 'npc')),
    force = 1,
    max.iter = 2e3,
    nudge_x = ifelse(mtcars$cyl == 6, 1, 0),
    nudge_y = ifelse(mtcars$cyl == 6, 8, 0)
  ) +
  scale_color_discrete(name = 'cyl') +
  theme_classic(base_size = 16)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel_options-1.png" title="plot of chunk geom_text_repel_options" alt="plot of chunk geom_text_repel_options" width="700" />

### geom_label_repel

`geom_label_repel` is based on [geom_label].


```r
set.seed(42)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), size = 5, color = 'grey') +
  geom_label_repel(
    aes(wt, mpg, fill = factor(cyl), label = rownames(mtcars)),
    fontface = 'bold', color = 'white',
    box.padding = unit(0.25, "lines"),
    point.padding = unit(0.5, "lines")
  ) +
  theme_classic(base_size = 16)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_label_repel-1.png" title="plot of chunk geom_label_repel" alt="plot of chunk geom_label_repel" width="700" />

### Line plot


```r
set.seed(42)
ggplot(Orange, aes(age, circumference, color = Tree)) +
  geom_line() +
  coord_cartesian(xlim = c(min(Orange$age), max(Orange$age) + 90)) +
  geom_text_repel(
    data = subset(Orange, age == max(age)),
    aes(label = paste("Tree", Tree)),
    size = 6,
    nudge_x = 45,
    segment.color = NA
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "Age (days)", y = "Circumference (mm)")
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/line_plot-1.png" title="plot of chunk line_plot" alt="plot of chunk line_plot" width="700" />

### Volcano plot


```r
set.seed(42)

# Read Stephen Turner's data
genes <- read.table("genes.txt.bz2", header = TRUE)
genes$Significant <- ifelse(genes$padj < 0.05, "FDR < 0.05", "Not Sig")

ggplot(genes, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point(aes(color = Significant)) +
  scale_color_manual(values = c("red", "grey")) +
  theme_bw(base_size = 16) +
  geom_text_repel(
    data = subset(genes, padj < 0.05),
    aes(label = Gene),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/volcano-1.png" title="plot of chunk volcano" alt="plot of chunk volcano" width="700" />

### Animation


```r
# This chunk of code will take a minute or two to run.
library(ggrepel)
library(animation)

plot_frame <- function(n) {
  set.seed(42)
  p <- ggplot(mtcars) +
    geom_point(aes(wt, mpg), color = 'red') +
    geom_text_repel(
      aes(wt, mpg, label = rownames(mtcars)),
      size = 5, force = 3, max.iter = n
    ) +
    theme_classic(base_size = 16)
  print(p)
}

saveGIF(
  lapply(c(seq(0, 2000, 25)), function(i) {
    plot_frame(i)
  }),
  interval = 0.05,
  ani.width = 800,
  ani.heigth = 600,
  movie.name = 'animated.gif'
)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/animated.gif" title="animation"
  alt="animation of geom_repel_text" width="700"/>

## R Session Info


```r
sessionInfo()
```

```
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.5 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggrepel_0.4.5 ggplot2_2.0.0 knitr_1.12   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.3      codetools_0.2-14 digest_0.6.9     grid_3.2.3      
##  [5] plyr_1.8.3       gtable_0.1.2     formatR_1.2.1    magrittr_1.5    
##  [9] evaluate_0.8     scales_0.3.0     stringi_1.0-1    labeling_0.3    
## [13] tools_3.2.3      stringr_1.0.0    munsell_0.4.2    colorspace_1.2-6
```

[geom_text]: http://docs.ggplot2.org/current/geom_text.html
[geom_label]: http://docs.ggplot2.org/current/geom_text.html
