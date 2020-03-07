#!/usr/bin/env Rscript
# build_site.R
# 
# Build the pkgdown site and then make a few additional tweaks.

library(pkgdown)

build_site(lazy = TRUE)

# build_site(lazy = FALSE)

# This vignette should only be on CRAN, not on the pkgdown website.
unlink("pkgdown/articles/ggrepel_files", recursive = TRUE)
unlink("pkgdown/articles/ggrepel.html", recursive = TRUE)

# Use the artwork by Allison Horst as the OpenGraph image.
og_image <- "https://ggrepel.slowkow.com/reference/figures/ggrepel-allison_horst.jpg"
# img <- '<meta property="og:image" content="http://example.com/pkg/logo.png">'


