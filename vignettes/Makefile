ggrepel.md : ggrepel.Rmd
	rm -rf cache/
	R --slave -e 'knitr::knit("ggrepel.Rmd")'
	perl -i -pe 's{src="figures/}{src="https://github.com/slowkow/ggrepel/blob/master/vignettes/figures/}' ggrepel.md
	# Check if qe have pngquant installed.
	[ -x "`which pngquant 2>/dev/null`" ] && \
	pngquant --force --speed 1 --quality 0-10 figures/ggrepel/*.png && \
	for f in figures/ggrepel/*-fs8.png; do mv -f $$f $${f%-fs8.png}.png; done
