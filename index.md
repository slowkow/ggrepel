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

Thanks
------

Thanks to everyone who has contributed pull requests, opened issues, asked
questions, and shared examples!

And thanks to [Allison Horst] for the beautiful artwork!

[Allison Horst]: https://github.com/allisonhorst


