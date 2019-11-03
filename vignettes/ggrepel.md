---
title: "ggrepel examples"
author: "Kamil Slowikowski"
date: "2019-05-06"
output:
  prettydoc::html_pretty:
    theme: hpstr
    highlight: github
    toc: true
    mathjax: null
    self_contained: true
vignette: >
  %\VignetteIndexEntry{ggrepel examples}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---



## Overview

ggrepel provides geoms for [ggplot2] to repel overlapping text labels:

- `geom_text_repel()`
- `geom_label_repel()`

[ggplot2]: http://ggplot2.tidyverse.org/

Text labels repel away from each other, away from data points, and away
from edges of the plotting area.

Let's compare `geom_text()` and `geom_text_repel()`:


```r
library(ggrepel)
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

p <- ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red")

p1 <- p + geom_text() + labs(title = "geom_text()")

p2 <- p + geom_text_repel() + labs(title = "geom_text_repel()")

gridExtra::grid.arrange(p1, p2, ncol = 2)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/comparison-1.png" title="plot of chunk comparison" alt="plot of chunk comparison" width="700" />

## Installation

[ggrepel version 0.8.0.9000][cran] is available on CRAN:


```r
install.packages("ggrepel")
```

The [latest development version][github] may have new features, and you can get
it from GitHub:


```r
# Use the devtools package
# install.packages("devtools")
devtools::install_github("slowkow/ggrepel")
```

[cran]: https://CRAN.R-project.org/package=ggrepel
[github]: https://github.com/slowkow/ggrepel

## Options

Options available for [geom_text()][geom_text] and [geom_label()][geom_text]
are also available for `geom_text_repel()` and `geom_label_repel()`,
including `size`, `angle`, `family`, `fontface`, etc.

[geom_text]: http://ggplot2.tidyverse.org/reference/geom_text.html

ggrepel provides additional options for `geom_text_repel` and `geom_label_repel`:

|Option          | Default      | Description
|--------------- | ------------ | ------------------------------------------------
|`force`         | `1`          | force of repulsion between overlapping text labels
|`direction`     | `"both"`     | move text labels "both" (default), "x", or "y" directions
|`max.iter`      | `2000`       | maximum number of iterations to try to resolve overlaps
|`nudge_x`       | `0`          | adjust the starting x position of the text label
|`nudge_y`       | `0`          | adjust the starting y position of the text label
|`box.padding`   | `0.25 lines` | padding around the text label
|`point.padding` | `0 lines`    | padding around the labeled data point
|`segment.color` | `"black"`    | line segment color
|`segment.size`  | `0.5 mm`     | line segment thickness
|`segment.alpha` | `1.0`        | line segment transparency
|`segment.curvature` | `0`      | numeric, negative for left-hand and positive for right-hand curves, 0 for straight lines
|`segment.angle` | `90`         | 0-180, less than 90 skews control points toward the start point
|`segment.ncp`   | `1`          | number of control points to make a smoother curve
|`arrow`         | `NULL`       | render line segment as an arrow with `grid::arrow()`

## Examples

### Hide some of the labels

Set labels to the empty string `""` to hide them. All data points repel the
non-empty labels.


```r
set.seed(42)

dat2 <- subset(mtcars, wt > 3 & wt < 4)
# Hide all of the text labels.
dat2$car <- ""
# Let's just label these items.
ix_label <- c(2, 3, 14)
dat2$car[ix_label] <- rownames(dat2)[ix_label]

ggplot(dat2, aes(wt, mpg, label = car)) +
  geom_text_repel() +
  geom_point(color = ifelse(dat2$car == "", "grey50", "red"))
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/empty_string-1.png" title="plot of chunk empty_string" alt="plot of chunk empty_string" width="700" />

We can quickly repel a few text labels from 10,000 data points:


```r
set.seed(42)

dat3 <- rbind(
  data.frame(
    wt  = rnorm(n = 10000, mean = 3),
    mpg = rnorm(n = 10000, mean = 19),
    car = ""
  ),
  dat2[,c("wt", "mpg", "car")]
)

ggplot(dat3, aes(wt, mpg, label = car)) +
  geom_point(data = dat3[dat3$car == "",], color = "grey50") +
  geom_text_repel(box.padding = 0.5) +
  geom_point(data = dat3[dat3$car != "",], color = "red")
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/empty_string_big-1.png" title="plot of chunk empty_string_big" alt="plot of chunk empty_string_big" width="700" />

### Do not repel labels from data points

Set `point.size = NA` to prevent label repulsion away from data points.

Now labels move away from each other and away from the edges of the plot.


```r
set.seed(42)
ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red") +
  geom_text_repel(point.size = NA)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/point_size_na-1.png" title="plot of chunk point_size_na" alt="plot of chunk point_size_na" width="700" />

### Make curved line segments

The line segments can be curved as in `geom_curve()` from ggplot2.

- `segment.curvature = 1` increases right-hand curvature, negative values would
  increase left-hand curvature, 0 makes straight lines
- `segment.ncp = 3` gives 3 control points for the curve
- `segment.angle = 20` skews the curve towards the start, values greater than
  90 would skew toward the end


```r
set.seed(42)

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red") +
  geom_text_repel(
    nudge_x = .15,
    box.padding = 0.5,
    nudge_y = 1,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  )
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/line_curve-1.png" title="plot of chunk line_curve" alt="plot of chunk line_curve" width="700" />

Setting the curvature to a value near zero gives a sharp angle:


```r
set.seed(42)
cars <- c("Volvo 142E", "Merc 230")

ggplot(dat, aes(wt, mpg, label = ifelse(car %in% cars, car, ""))) +
  geom_point(color = "red") +
  geom_text_repel(
    point.padding = 0.2, 
    nudge_x = .15,
    nudge_y = .5,
    segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.015, "npc"))
  )
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/line_curve_annotate-1.png" title="plot of chunk line_curve_annotate" alt="plot of chunk line_curve_annotate" width="700" />

### Repel labels from data points with different sizes

TODO: This example should be improved:

- Use real data.
- Show a side-by-side comparison with and without `point.size`.


```r
library(ggrepel)

d <- data.frame(
  x = c(1, 2, 2, 3, 2.5),
  y = c(1, 2, 3, 1, 2.0),
  size = c(5, 2, 4, 1, 2),
  label = sprintf("label%s", 1:5)
)

size_range <- c(1, 50)
ggplot(d, aes(x, y)) +
  geom_point(aes(size = size), alpha = 0.6) +
  continuous_scale(
    aesthetics = c("size", "point.size"), scale_name = "size",
    palette = scales::area_pal(size_range), guide = FALSE
  ) +
  geom_text_repel(
    aes(label = label, point.size = size), size = 5,
    point.padding = 0, min.segment.length = 0,
    max.time = 1, max.iter = 1e5,
    box.padding = 2
  ) +
  # coord_equal() +
  scale_y_continuous(limits = c(0.5, 3.5))
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/point_size-1.png" title="plot of chunk point_size" alt="plot of chunk point_size" width="700" />

### Limit labels to a specific area

Use options `xlim` and `ylim` to constrain the labels to a specific area.
Limits are specified in data coordinates. Use `NA` when there is no lower or
upper bound in a particular direction.

Here we also use `grid::arrow()` to render the segments as arrows.


```r
set.seed(42)

# All labels should be to the right of 3.
x_limits <- c(3, NA)

ggplot(dat, aes(wt, mpg, label = car, color = factor(cyl))) +
  geom_vline(xintercept = x_limits, linetype = 3) +
  geom_point() +
  geom_label_repel(
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
    xlim  = x_limits,
    point.padding = NA,
    box.padding = 0.1
  ) +
  scale_color_discrete(name = "cyl")
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/xlim-1.png" title="plot of chunk xlim" alt="plot of chunk xlim" width="700" />

### Align labels on the top or bottom edge

Use `hjust` or `vjust` to justify the text neatly:

- `hjust = 0` for left-align
- `hjust = 0.5` for center
- `hjust = 1` for right-align

Sometimes the labels do not align perfectly. Try using `direction = "x"` to
limit label movement to the x-axis (left and right) or `direction = "y"` to
limit movement to the y-axis (up and down). The default is `direction =
"both"`.

Also try using [xlim()][xlim] and [ylim()][ylim] to increase the size of the
plotting area so all of the labels fit comfortably.


```r
set.seed(42)

ggplot(mtcars, aes(x = wt, y = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  geom_text_repel(
    force        = 0.5,
    nudge_y      = 0.05,
    direction    = "x",
    angle        = 90,
    vjust        = 0,
    segment.size = 0.2
  ) +
  xlim(1, 6) +
  ylim(1, 0.8) +
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/direction_x-1.png" title="plot of chunk direction_x" alt="plot of chunk direction_x" width="700" />

Align text vertically with `nudge_y` and allow the labels to move horizontally
with `direction = "x"`:


```r
set.seed(42)

dat <- mtcars
dat$car <- rownames(dat)

ggplot(dat, aes(qsec, mpg, label = car)) +
  geom_text_repel(
    data          = subset(dat, mpg > 30),
    nudge_y       = 36 - subset(dat, mpg > 30)$mpg,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "x"
  ) +
  geom_point(color = ifelse(dat$mpg > 30, "red", "black")) +
  scale_x_continuous(expand = c(0.05, 0.05)) +
  scale_y_continuous(limits = c(NA, 36))
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/neat-offset-x-1.png" title="plot of chunk neat-offset-x" alt="plot of chunk neat-offset-x" width="700" />

### Align labels on the left or right edge

Set `direction` to "y" and try `hjust` 0.5, 0, and 1:


```r
set.seed(42)

p <- ggplot(mtcars, aes(y = wt, x = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  ylim(1, 5.5) +
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank()
  )

p1 <- p +
  xlim(1, 1.375) +
  geom_text_repel(
    force        = 0.5,
    nudge_x      = 0.15,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0")

p2 <- p + 
  xlim(1, 1.375) +
  geom_text_repel(
    force        = 0.5,
    nudge_x      = 0.2,
    direction    = "y",
    hjust        = 0.5,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0.5 (default)")

p3 <- p +
  xlim(0.25, 1) +
  scale_y_continuous(position = "right") +
  geom_text_repel(
    force        = 0.5,
    nudge_x      = -0.25,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 1")

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/direction_y-1.png" title="plot of chunk direction_y" alt="plot of chunk direction_y" width="700" />

Align text horizontally with `nudge_x` and `hjust`, and allow the labels to
move vertically with `direction = "y"`:


```r
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_text_repel(
    data          = subset(dat, wt > 3),
    nudge_x       = 3.5 - subset(dat, wt > 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0
  ) +
  geom_text_repel(
    data          = subset(dat, wt < 3),
    nudge_x       = 2.7 - subset(dat, wt < 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 1
  ) +
  scale_x_continuous(
    breaks = c(2.5, 2.75, 3, 3.25, 3.5),
    limits = c(2.4, 3.8)
  ) +
  geom_point(color = "red")
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/neat-offset-y-1.png" title="plot of chunk neat-offset-y" alt="plot of chunk neat-offset-y" width="700" />

### Label jittered points

**Note:** This example will not work with ggplot2 version 2.2.1 or older.

To get the latest development version of ggplot2, try:


```r
# install.packages("devtools")
devtools::install_github("tidyverse/ggplot2")
```

If your ggplot2 is newer than 2.2.1, try this example:


```r
mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$cyl != 6] <- ""

# New! (not available in ggplot2 version 2.2.1 or earlier)
pos <- position_jitter(width = 0.3, seed = 2)

ggplot(mtcars, aes(factor(cyl), mpg, color = label != "", label = label)) +
  geom_point(position = pos) +
  geom_text_repel(position = pos) +
  theme(legend.position = "none") +
  labs(title = "position_jitter()")
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/jitter-1.png" title="plot of chunk jitter" alt="plot of chunk jitter" width="700" />

You can also use other position functions, like `position_quasirandom()` from
the [ggbeeswarm] package by [Erik Clarke]:

[ggbeeswarm]: https://github.com/eclarke/ggbeeswarm
[Erik Clarke]: https://github.com/eclarke


```r
mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$cyl != 6] <- ""

library(ggbeeswarm)
pos <- position_quasirandom()

ggplot(mtcars, aes(factor(cyl), mpg, color = label != "", label = label)) +
  geom_point(position = pos) +
  geom_text_repel(position = pos) +
  theme(legend.position = "none") +
  labs(title = "position_quasirandom()")
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/quasirandom-1.png" title="plot of chunk quasirandom" alt="plot of chunk quasirandom" width="700" />

### Word cloud

The `force` option controls the strength of repulsion.

The `force_pull` option controls the strength of the spring that pulls the text
label toward its data point.

To make a word cloud, we can assign all of the text labels the same data point
at the origin (0, 0) and set `force_pull = 0` to disable the springs.


```r
set.seed(42)
ggplot(mtcars) +
  geom_text_repel(
    aes(
      label  = rownames(mtcars),
      size   = mpg > 15,
      colour = factor(cyl),
      x      = 0,
      y      = 0
    ),
    force_pull    = 0, # do not pull text toward the point at (0,0)
    max.time      = 0.5,
    max.iter      = 1e5,
    segment.color = NA,
    point.padding = NA
  ) +
  theme_void() +
  theme(strip.text = element_text(size = 16)) +
  facet_wrap(~ factor(cyl)) +
  scale_color_discrete(name = "Cylinders") +
  scale_size_manual(values = c(2, 3)) +
  theme(
    strip.text   = element_blank(),
    panel.border = element_rect(size = 0.2, fill = NA)
  )
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/wordcloud-1.png" title="plot of chunk wordcloud" alt="plot of chunk wordcloud" width="700" />

### Polar coordinates


```r
set.seed(42)

mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$mpg < 25] <- ""

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), label = label)) +
  coord_polar(theta = "x") +
  geom_point(size = 2) +
  scale_color_discrete(name = "cyl") +
  geom_text_repel(show.legend = FALSE) + # Don't display "a" in the legend.
  theme_bw(base_size = 18)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/polar-1.png" title="plot of chunk polar" alt="plot of chunk polar" width="700" />

### Mathematical expressions


```r
d <- data.frame(
  x    = c(1, 2, 2, 1.75, 1.25),
  y    = c(1, 3, 1, 2.65, 1.25),
  math = c(
    NA,
    "integral(f(x) * dx, a, b)",
    NA,
    "lim(f(x), x %->% 0)",
    NA
  )
)

ggplot(d, aes(x, y, label = math)) +
  geom_point() +
  geom_label_repel(
    parse       = TRUE, # Parse mathematical expressions.
    size        = 6,
    box.padding = 2
  )
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/math-1.png" title="plot of chunk math" alt="plot of chunk math" width="700" />

### Animation


```r
# This chunk of code will take a minute or two to run.
library(ggrepel)
library(animation)

plot_frame <- function(n) {
  set.seed(42)
  p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
    geom_text_repel(
      size = 5, force = 1, max.iter = n
    ) +
    geom_point(color = "red") +
    # theme_minimal(base_size = 16) +
    labs(title = n)
  print(p)
}

xs <- ceiling(1.18^(1:52))
# xs <- ceiling(1.4^(1:26))
xs <- c(xs, rep(xs[length(xs)], 15))
# plot(xs)

saveGIF(
  lapply(xs, function(i) {
    plot_frame(i)
  }),
  interval   = 0.15,
  ani.width  = 800,
  ani.heigth = 600,
  movie.name = "animated.gif"
)
```

<a href="https://i.imgur.com/vv7uTwI.gifv">Click here</a> to see the animation.

## Source code

View the [source code for this vignette][source] on GitHub.

[source]: https://github.com/slowkow/ggrepel/blob/master/vignettes/ggrepel.Rmd

## R Session Info


```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.6
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggbeeswarm_0.6.0   ggrepel_0.8.0.9000 ggplot2_3.1.0     
## [4] gridExtra_2.3      knitr_1.21        
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.1       magrittr_1.5     tidyselect_0.2.5 munsell_0.5.0   
##  [5] colorspace_1.4-1 R6_2.4.0         rlang_0.3.1.9000 vipor_0.4.5     
##  [9] highr_0.7        stringr_1.4.0    plyr_1.8.4       dplyr_0.8.0.9002
## [13] tools_3.5.1      grid_3.5.1       beeswarm_0.2.3   gtable_0.3.0    
## [17] xfun_0.5         withr_2.1.2      digest_0.6.18    lazyeval_0.2.2  
## [21] assertthat_0.2.1 tibble_2.1.1     crayon_1.3.4     purrr_0.3.0     
## [25] glue_1.3.1       evaluate_0.13    labeling_0.3     stringi_1.4.3   
## [29] compiler_3.5.1   pillar_1.3.1     scales_1.0.0     pkgconfig_2.0.2
```

[xlim]: http://ggplot2.tidyverse.org/reference/lims.html
[ylim]: http://ggplot2.tidyverse.org/reference/lims.html

