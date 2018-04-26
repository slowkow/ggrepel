ggrepel <img src="logo.svg" width="120px" align="right" />
============================================

[![GitHub Version][gb]][releases] [![Build Status][bb]][travis] [![CRAN_Status_Badge][cb]][cran] [![CRAN_Downloads_Badge][db]][r-pkg]

[cb]: http://www.r-pkg.org/badges/version/ggrepel?color=blue
[cran]: https://CRAN.R-project.org/package=ggrepel

[db]: http://cranlogs.r-pkg.org/badges/grand-total/ggrepel?color=blue
[r-pkg]: https://www.r-pkg.org/pkg/ggrepel

[gb]: https://badge.fury.io/gh/slowkow%2Fggrepel.svg?text=Github
[releases]: https://github.com/slowkow/ggrepel/releases

[bb]: https://travis-ci.org/slowkow/ggrepel.svg?branch=master
[travis]: https://travis-ci.org/slowkow/ggrepel

Overview
--------

ggrepel provides geoms for [ggplot2] to repel overlapping text labels.

```r
library(ggrepel)
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_text_repel() +
  geom_point(color = 'red') +
  theme_classic(base_size = 16)
```

<img width="80%" src="fig.png" />

Installation
------------

```r
# The easiest way to get ggrepel is to install it from CRAN:
install.packages("ggrepel")

# Or get the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("slowkow/ggrepel")
```

Usage
-----

See the [vignette] for the code behind these examples:

<a href="vignettes/ggrepel.md#hide-some-of-the-labels"><img style="margin:1rem;" width="30%" src="vignettes/figures/ggrepel/empty_string-1.png" /></a> <a href="vignettes/ggrepel.md#do-not-repel-labels-from-data-points"><img style="margin:1rem;" width="30%" src="vignettes/figures/ggrepel/point_padding_na-1.png" /></a> <a href="vignettes/ggrepel.md#align-text-labels"><img style="margin:1rem;" width="30%" src="vignettes/figures/ggrepel/direction_y-1.png" /></a> <a href="vignettes/ggrepel.md#limit-labels-to-a-specific-area"><img style="margin:1rem;" width="30%" src="vignettes/figures/ggrepel/xlim-1.png" /></a> <a href="vignettes/ggrepel.md#polar-coordinates"><img style="margin:1rem;" width="30%" src="vignettes/figures/ggrepel/polar-1.png" /></a> <a href="vignettes/ggrepel.md#mathematical-expressions"><img style="margin:1rem;" width="30%" src="vignettes/figures/ggrepel/math-1.png" /></a>

Contributing
------------

Please [submit an issue][issues] to report bugs or ask questions.

Please contribute bug fixes or new features with a [pull request][pull] to this
repository.

[issues]: https://github.com/slowkow/ggrepel/issues
[pull]: https://help.github.com/articles/using-pull-requests/

Related work
------------

### Python

[adjustText]

> A small library for automatically adjusting text position in matplotlib plots to minimize overlaps.

Ilya Flyamer's Python library that extends [matplotlib].

[adjustText]: https://github.com/Phlya/adjustText
[matplotlib]: https://matplotlib.org/

### R

[directlabels]

> An extensible framework for automatically placing direct labels onto
> multicolor 'lattice' or 'ggplot2' plots. Label positions are described
> using Positioning Methods which can be re-used across several different
> plots. There are heuristics for examining "trellis" and "ggplot" objects
> and inferring an appropriate Positioning Method.

[wordcloud]

> Pretty word clouds.

The `wordcloud` package implements a spiraling algorithm to prevent text
labels from overlapping each other.

[FField]

> Force field simulation of interaction of set of points. Very useful for
> placing text labels on graphs, such as scatterplots.

I found that functions in the `FField` package were not ideal for repelling
overlapping rectangles, so I wrote my own.

See [this gist][1] for examples of how to use the `wordcloud` and `FField`
packages with `ggplot2`.

[1]: https://gist.github.com/slowkow/003b4d9f3f59cee8551c

[ggplot2]: http://ggplot2.tidyverse.org
[vignette]: https://github.com/slowkow/ggrepel/blob/master/vignettes/ggrepel.md
[directlabels]: https://cran.r-project.org/package=directlabels
[wordcloud]: https://cran.r-project.org/package=wordcloud
[FField]: https://cran.r-project.org/package=FField
