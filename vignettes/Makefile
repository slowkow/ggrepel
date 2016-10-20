ggrepel.md : ggrepel.Rmd
	rm -rf cache/
	R --slave -e 'knitr::knit("ggrepel.Rmd")'
	perl -i -pe 's{figures/}{https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/}' ggrepel.md
	pngquant --force figures/ggrepel/*.png
	for f in figures/ggrepel/*-fs8.png; do mv -f $$f $${f%-fs8.png}.png; done
