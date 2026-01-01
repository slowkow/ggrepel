# Build the examples
examples:
  R --vanilla -e 'devtools::load_all(); rmarkdown::render("vignettes/examples.Rmd")' && notify "rendered vignettes/examples.Rmd"

# Build the documentation files
man:
  R --vanilla -e 'devtools::load_all(); devtools::document()'

# Install the package
install:
  R -e 'devtools::install(build = TRUE, upgrade = "never")'

# Build the documentation website
site:
  R --vanilla -e 'devtools::load_all(); pkgdown::build_site()' && notify "pkgdown::build_site() finished"

test:
  R --vanilla -e 'devtools::load_all(); devtools::test()'

