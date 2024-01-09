<!-- badges: start -->
[![Build Status][bb]][travis] [![CRAN_Status_Badge][cb]][cran] [![CRAN_Downloads_Badge][db]][r-pkg]

[bb]: https://travis-ci.org/slowkow/ggrepel.svg?branch=master
[travis]: https://travis-ci.org/slowkow/ggrepel

[cb]: https://www.r-pkg.org/badges/version/ggrepel?color=blue
[cran]: https://CRAN.R-project.org/package=ggrepel

[db]: https://cranlogs.r-pkg.org/badges/grand-total/ggrepel?color=blue
[r-pkg]: https://www.r-pkg.org/pkg/ggrepel
<!-- badges: end -->

<div class="text-center">
<img style="max-width:80%" src="https://user-images.githubusercontent.com/209714/200123867-db9bd406-54d7-4ce0-aa09-1ec30740a87a.jpg"></img>
</div>

Overview
--------

ggrepel provides geoms for [ggplot2] to repel overlapping text labels:

[ggplot2]: https://ggplot2.tidyverse.org/

- `geom_text_repel()`
- `geom_label_repel()`

Text labels repel away from each other, away from data points, and away
from edges of the plotting area.

```r
library(ggrepel)
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_text_repel() +
  geom_point(color = 'red') +
  theme_classic(base_size = 16)
```

<p align="center">
<img src="https://imgur.com/ii9ova8.gif" />
</p>

Installation
------------

```r
# The easiest way to get ggrepel is to install it from CRAN:
install.packages("ggrepel")

# Or get the latest development version from GitHub:
# install.packages("devtools")
devtools::install_github("slowkow/ggrepel")
```

Getting help
------------

Check out the [examples] to learn how to use ggrepel in your project.

See our collection of [related work] for similar R packages, Python packages,
and more.

If you have an issue with ggrepel or ggplot2, someone might already have
experienced it. Try searching the web, or create a new post:

1. The [RStudio community] is an active forum with RStudio and R users.

2. [Stack Overflow] has lots of questions and answers about ggplot2 and ggrepel.

[examples]: articles/examples.html
[related work]: articles/related-work.html
[Rstudio community]: https://community.rstudio.com/
[Stack Overflow]: https://stackoverflow.com/questions/tagged/ggrepel?sort=frequent&pageSize=50

Examples
--------

Click one of the images below to view the source code for that figure:

<a href="https://ggrepel.slowkow.com/articles/examples.html#hide-some-of-the-labels"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/empty_string-1.png" alt="Hide some of the labels"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#always-show-all-labels-even-when-they-have-too-many-overlaps"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/show_all_labels-1.png" alt="Always show all labels, even when they have too many overlaps"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#do-not-repel-labels-from-data-points"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/point_size_na-1.png" alt="Do not repel labels from data points"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#do-not-repel-labels-from-plot-panel-edges"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/plot_edges-1.png" alt="Do not repel labels from plot (panel) edges"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#expand-the-scale-to-make-room-for-labels"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/expand_scale-1.png" alt="Expand the scale to make room for labels"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#always-or-never-draw-line-segments"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/all_segments-1.png" alt="Always (or never) draw line segments"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#make-curved-line-segments-or-arrows"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/line_curve-1.png" alt="Make curved line segments or arrows"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#repel-labels-from-data-points-with-different-sizes"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/point_size_cars-1.png" alt="Repel labels from data points with different
sizes"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#limit-labels-to-a-specific-area"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/xlim-1.png" alt="Limit labels to a specific area"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#remove-a-from-the-legend"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/remove_a_2-1.png" alt="Remove “a” from the legend"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#align-labels-on-the-top-or-bottom-edge"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/direction_x-1.png" alt="Align labels on the top or bottom edge"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#align-labels-on-the-left-or-right-edge"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/direction_y-1.png" alt="Align labels on the left or right edge"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#using-ggrepel-with-stat_summary"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/stat_summary-1.png" alt="Using ggrepel with stat_summary()"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#justify-multiple-lines-of-text-with-hjust"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/geom_text_repel-hjust-1.png" alt="Justify multiple lines of text with hjust"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#label-jittered-points"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/jitter-1.png" alt="Label jittered points"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#nudge-labels-in-different-directions-with-ggpp"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/position_nudge_center-1.png" alt="Nudge labels in different directions with ggpp"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#label-sf-objects"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/label-sf-objects-1.png" alt="Label sf objects"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#shadows-or-glow-under-text-labels"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/shadowtext-1.png" alt="Shadows (or glow) under text labels"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#verbose-timing-information"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/timing-1.png" alt="Verbose timing information"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#word-cloud"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/wordcloud-1.png" alt="Word cloud"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#polar-coordinates"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/polar-1.png" alt="Polar coordinates"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#unicode-characters-japanese"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/japanese-1.png" alt="Unicode characters (Japanese)"></img></a>
<a href="https://ggrepel.slowkow.com/articles/examples.html#mathematical-expressions"><img width="200" src="https://raw.githubusercontent.com/slowkow/ggrepel/master/docs/articles/examples_files/figure-html/math-1.png" alt="Mathematical expressions"></img></a>

Thanks
------

Thanks to everyone who has contributed pull requests, opened issues, asked
questions, and shared examples!

And thanks to [Allison Horst] for the beautiful artwork!

[Allison Horst]: https://github.com/allisonhorst


