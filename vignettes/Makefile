ggrepel.md : ggrepel.Rmd
	rm -rf cache/
	R --slave -e 'knitr::knit("ggrepel.Rmd")'
	perl -i -pe 's{figures/}{https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/}' ggrepel.md
