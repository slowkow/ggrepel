examples:
  R -e 'rmarkdown::render("vignettes/examples.Rmd")'

site:
  R -e 'pkgdown::build_site()'

install:
  R -e 'devtools::install(build = TRUE, upgrade = "never")'
