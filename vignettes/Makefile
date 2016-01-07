ggrepel.md : ggrepel.Rmd
	R --slave -e 'knitr::knit("ggrepel.Rmd")'
	perl -i -pe 's{figures/}{https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/}' ggrepel.md
