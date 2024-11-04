# Build the examples
examples:
  R -e 'rmarkdown::render("vignettes/examples.Rmd"); beepr::beep()'

# Build the documentation files
man:
  R -e 'devtools::document()'

# Install the package
install:
  R -e 'devtools::install(build = TRUE, upgrade = "never")'

# Build the documentation website
site:
  R -e 'pkgdown::build_site(); beepr::beep()'

