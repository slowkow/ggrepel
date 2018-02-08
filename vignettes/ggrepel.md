---
title: "ggrepel examples"
author: "Kamil Slowikowski"
date: "2018-02-08"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{ggrepel examples}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Compare `geom_text` and `geom_text_repel`


```r
library(ggrepel)
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

p <- ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 18)

p1 <- p + geom_text() + labs(title = "geom_text")

p2 <- p + geom_text_repel() + labs(title = "geom_text_repel")

gridExtra::grid.arrange(p1, p2, ncol = 2)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/comparison-1.png" title="plot of chunk comparison" alt="plot of chunk comparison" width="700" />

## Algorithm

`ggrepel` implements functions to repel overlapping text labels away from
each other and away from the data points that they label.

The algorithm run time is `O(n^2)` where n is the number of text labels:

- For each text box:
    - Move the box into the allowed plotting area.
    - If the box overlaps other boxes:
        - Repel the overlapping boxes from each other.
    - If the box overlaps data points:
        - Repel the box away from the data points.
- Repeat until boxes do not overlap, or until we reach the maximum
  number of iterations.

## Options

Options available for [geom_text] are also available for `geom_text_repel`,
including `size`, `angle`, `family`, `fontface`, etc.

However, the following options are not supported:

- `position`
- `check_overlap`

Options `hjust` and `vjust` are supported, but text alignment may be
disrupted in some cases. For best alignment, use `direction="x"` or
`direction="y"` to limit how the text labels can move. Also consider using
[xlim] and [ylim] to increase the size of the plotting area.

[xlim]: http://ggplot2.tidyverse.org/reference/lims.html
[ylim]: http://ggplot2.tidyverse.org/reference/lims.html

ggrepel provides additional options for `geom_text_repel` and `geom_label_repel`:


|Option          | Default      | Description
|--------------- | ---------    | ------------------------------------------------
|`segment.color` | `"black"`    | line segment color
|`segment.size`  | `0.5 mm`     | line segment thickness
|`segment.alpha` | `1.0`        | line segment transparency
|`box.padding`   | `0.25 lines` | padding around the text box
|`point.padding` | `0 lines`    | padding around the labeled point
|`arrow`         | `NULL`       | options for arrow heads created by `grid::arrow`
|`force`         | `1`          | force of repulsion between overlapping text labels
|`max.iter`      | `2000`       | maximum number of iterations to try to resolve overlaps
|`nudge_x`       | `0`          | shift the starting x position of the text label
|`nudge_y`       | `0`          | shift the starting y position of the text label
|`direction`     | `"both"`     | move text labels "both" (default), "x", or "y" directions

## Hide some of the labels

Set labels to the empty string `""` to hide them.

This way, the unlabeled data points will still repel the remaining labels.


```r
set.seed(42)

dat2 <- subset(mtcars, wt > 3 & wt < 4)
dat2$car <- ""
ix_label <- c(2,3,16)
dat2$car[ix_label] <- rownames(dat2)[ix_label]

ggplot(dat2, aes(wt, mpg, label = car)) +
  geom_point(color = ifelse(dat2$car == "", "grey50", "red")) +
  geom_text_repel() +
  theme_classic(base_size = 18)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/empty_string-1.png" title="plot of chunk empty_string" alt="plot of chunk empty_string" width="700" />

## Do not repel labels from data points

Set `point.padding = NA` to prevent text repulsion away from data points.


```r
set.seed(42)
ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red") +
  geom_text_repel(point.padding = NA) +
  theme_classic(base_size = 18)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/point_padding_na-1.png" title="plot of chunk point_padding_na" alt="plot of chunk point_padding_na" width="700" />

## Limit labels to a specific area

Use `xlim` and `ylim` to constrain the labels to a specific area. Limits are
specified in data coordinates. Use `NA` when there is no lower or upper bound
in a particular direction.


```r
set.seed(42)

x_limits <- c(3, NA)

ggplot(dat, aes(wt, mpg, label = car, color = factor(cyl))) +
  geom_vline(xintercept = x_limits, linetype = 3) +
  geom_point() +
  geom_label_repel(
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
    force = 10,
    xlim  = x_limits
  ) +
  scale_color_discrete(name = "cyl") +
  theme_classic(base_size = 18)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/xlim-1.png" title="plot of chunk xlim" alt="plot of chunk xlim" width="700" />

## Align text labels

Use `direction` to limit label movement to the x-axis (left and right) or
y-axis (up and down). The allowed values are "both" (default), "x", or "y".

Then, use `hjust` or `vjust` to align the text neatly.


```r
set.seed(42)

ggplot(mtcars, aes(x = wt, y = 1, label = rownames(mtcars))) +
  geom_point(color = 'red') +
  geom_text_repel(
    nudge_y      = 0.05,
    direction    = "x",
    angle        = 90,
    vjust        = 0,
    segment.size = 0.2
  ) +
  theme_classic(base_size = 18) +
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

Set `direction` to "y" and try `hjust` 0.5, 0, and 1:


```r
set.seed(42)

p <- ggplot(mtcars, aes(y = wt, x = 1, label = rownames(mtcars))) +
  geom_point(color = 'red') +
  theme_classic(base_size = 18) +
  ylim(1, 5.5) +
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    plot.title   = element_text(hjust = 0.5)
  )

p1 <- p +
  xlim(1, 1.375) +
  geom_text_repel(
    nudge_x      = 0.15,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0")

p2 <- p + 
  xlim(1, 1.375) +
  geom_text_repel(
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
    nudge_x      = -0.35,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 1")

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/direction_y-1.png" title="plot of chunk direction_y" alt="plot of chunk direction_y" width="700" />

## Polar coordinates


```r
set.seed(42)

mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$mpg < 25] <- ""

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), label = label)) +
  coord_polar(theta = "x") +
  geom_point(size = 2) +
  scale_color_discrete(name = "cyl") +
  geom_text_repel(show.legend = FALSE) + # Don't display 'a' in the legend.
  theme_bw(base_size = 18)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/polar-1.png" title="plot of chunk polar" alt="plot of chunk polar" width="700" />

## Mathematical expressions


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
    size        = 8,
    box.padding = 2
  ) +
  theme_classic(base_size = 20)
```

<img src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/math-1.png" title="plot of chunk math" alt="plot of chunk math" width="700" />

## Animation


```r
# This chunk of code will take a minute or two to run.
library(ggrepel)
library(animation)

plot_frame <- function(n) {
  set.seed(42)
  p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
    geom_point(color = 'red') +
    geom_text_repel(
      size = 5, force = 3, max.iter = n
    ) +
    theme_minimal(base_size = 16)
  print(p)
}

saveGIF(
  lapply(ceiling(1.75^(1:12)), function(i) {
    plot_frame(i)
  }),
  interval   = 0.20,
  ani.width  = 800,
  ani.heigth = 600,
  movie.name = 'animated.gif'
)
```

<img src="https://i.imgur.com/vv7uTwI.gif" title="animation"
  alt="animation of geom_repel_text" width="700"/>

## R Session Info


```r
devtools::session_info()
```

```
##  setting  value                       
##  version  R version 3.4.3 (2017-11-30)
##  system   x86_64, darwin15.6.0        
##  ui       X11                         
##  language (EN)                        
##  collate  en_US.UTF-8                 
##  tz       America/New_York            
##  date     2018-02-08                  
## 
##  package    * version    date       source                          
##  base       * 3.4.3      2017-12-07 local                           
##  codetools    0.2-15     2016-10-05 CRAN (R 3.4.3)                  
##  colorspace   1.3-2      2016-12-14 CRAN (R 3.4.0)                  
##  compiler     3.4.3      2017-12-07 local                           
##  datasets   * 3.4.3      2017-12-07 local                           
##  devtools     1.13.4     2017-11-09 CRAN (R 3.4.2)                  
##  digest       0.6.15     2018-01-28 cran (@0.6.15)                  
##  evaluate     0.10.1     2017-06-24 CRAN (R 3.4.1)                  
##  ggplot2    * 2.2.1      2016-12-30 CRAN (R 3.4.0)                  
##  ggrepel    * 0.7.2      2018-02-08 local                           
##  graphics   * 3.4.3      2017-12-07 local                           
##  grDevices  * 3.4.3      2017-12-07 local                           
##  grid         3.4.3      2017-12-07 local                           
##  gridExtra    2.3        2017-09-09 CRAN (R 3.4.1)                  
##  gtable       0.2.0      2016-02-26 CRAN (R 3.4.0)                  
##  highr        0.6        2016-05-09 CRAN (R 3.4.0)                  
##  knitr      * 1.17       2017-08-10 CRAN (R 3.4.1)                  
##  labeling     0.3        2014-08-23 CRAN (R 3.4.0)                  
##  lazyeval     0.2.1      2017-10-29 CRAN (R 3.4.2)                  
##  magrittr     1.5        2014-11-22 CRAN (R 3.4.0)                  
##  memoise      1.1.0      2017-04-21 CRAN (R 3.4.0)                  
##  methods    * 3.4.3      2017-12-07 local                           
##  munsell      0.4.3      2016-02-13 CRAN (R 3.4.0)                  
##  pillar       1.1.0      2018-01-14 cran (@1.1.0)                   
##  plyr         1.8.4      2016-06-08 CRAN (R 3.4.0)                  
##  Rcpp         0.12.15    2018-01-20 cran (@0.12.15)                 
##  rlang        0.1.6.9003 2018-02-01 Github (tidyverse/rlang@c6747f9)
##  scales       0.5.0.9000 2017-12-01 Github (hadley/scales@d767915)  
##  stats      * 3.4.3      2017-12-07 local                           
##  stringi      1.1.6      2017-11-17 CRAN (R 3.4.2)                  
##  stringr      1.2.0      2017-02-18 CRAN (R 3.4.0)                  
##  tibble       1.4.2      2018-01-22 cran (@1.4.2)                   
##  tools        3.4.3      2017-12-07 local                           
##  utils      * 3.4.3      2017-12-07 local                           
##  withr        2.1.1.9000 2017-12-20 Github (jimhester/withr@df18523)
```

[geom_text]: http://ggplot2.tidyverse.org/reference/geom_text.html
[geom_label]: http://ggplot2.tidyverse.org/reference/geom_label.html
