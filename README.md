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

[wordcloud]

> Pretty word clouds.

The `wordcloud` package implements a spiraling algorithm to prevent text
labels from overlapping each other.

[FField]

> Force field simulation of interaction of set of points. Very useful for
> placing text labels on graphs, such as scatterplots.

I found that functions in the `FField` package were not ideal for repelling
overlapping rectangles, so I wrote my own.

[ggplot2]: http://ggplot2.org/
[vignette]: https://github.com/slowkow/ggrepel/blob/master/vignettes/ggrepel.md
[wordcloud]: https://cran.r-project.org/web/packages/wordcloud/index.html
[FField]: https://cran.r-project.org/web/packages/FField/index.html

