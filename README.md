ggrepel <img src="man/figures/logo.svg" width="181px" align="right" />
============================================

[![Build Status][bb]][travis] [![CRAN_Status_Badge][cb]][cran] [![CRAN_Downloads_Badge][db]][r-pkg]

[bb]: https://travis-ci.org/slowkow/ggrepel.svg?branch=master
[travis]: https://travis-ci.org/slowkow/ggrepel

[cb]: https://www.r-pkg.org/badges/version/ggrepel?color=blue
[cran]: https://CRAN.R-project.org/package=ggrepel

[db]: https://cranlogs.r-pkg.org/badges/ggrepel
[r-pkg]: https://www.r-pkg.org/pkg/ggrepel

Overview
--------

ggrepel provides geoms for [ggplot2] to repel overlapping text labels:

[ggplot2]: https://ggplot2.tidyverse.org

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

# Or get the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("slowkow/ggrepel")
```

Usage
-----

See the [examples] page to learn more about how to use ggrepel in your project.

[examples]: https://CRAN.R-project.org/package=ggrepel/vignettes/ggrepel.html

Contributing
------------

Please [submit an issue][issues] to report bugs or ask questions.

Please contribute bug fixes or new features with a [pull request][pull] to this
repository.

[issues]: https://github.com/slowkow/ggrepel/issues
[pull]: https://help.github.com/articles/using-pull-requests/
