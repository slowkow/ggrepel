#!/usr/bin/env Rscript
# build_site.R
# 
# Build the pkgdown site and then make a few additional tweaks.

library(pkgdown)

build_site(lazy = TRUE)

# build_site(lazy = FALSE)

