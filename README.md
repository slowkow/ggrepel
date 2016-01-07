# ggrepel

`ggrepel` is an R package that implements functions to repel overlapping text
labels away from each other in a plot created by [ggplot2].

```r
library(ggrepel)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), color = 'red') +
  geom_text_repel(aes(wt, mpg, label = rownames(mtcars))) +
  theme_classic(base_size = 16)
```

![geom_text_repel](https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/ggrepel/geom_text_repel-1.png) 

## Usage

See the [vignette] for more usage examples.

## Installation

```r
install.packages("devtools")
devtools::install_github("slowkow/ggrepel")
```

## Contributing

Please [submit an issue][issues] to report bugs or ask questions.

Please contribute bug fixes or new features with a [pull request][pull] to this
repository.

[issues]: https://github.com/slowkow/ggrepel/issues
[pull]: https://help.github.com/articles/using-pull-requests/

## Related work

[directlabels]

> This package is an attempt to make direct labeling a reality in everyday
> statistical practice by making available a body of useful functions that
> make direct labeling of common plots easy to do with high-level plotting
> systems such as lattice and ggplot2. The main function that the package
> provides is direct.label(p), which takes a lattice or ggplot2 plot p and
> adds direct labels.

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


[ggplot2]: http://ggplot2.org/
[vignette]: https://github.com/slowkow/ggrepel/blob/master/vignettes/ggrepel.md
[directlabels]: https://cran.r-project.org/package=directlabels
[wordcloud]: https://cran.r-project.org/package=wordcloud
[FField]: https://cran.r-project.org/package=FField

