examples:
  R -e 'rmarkdown::render("vignettes/examples.Rmd"); beepr::beep()'

man:
  R -e 'devtools::document()'

site:
  R -e 'pkgdown::build_site(); beepr::beep()'

install:
  R -e 'devtools::install(build = TRUE, upgrade = "never")'
