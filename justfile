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

check:
  [ -e _build ] || mkdir _build
  rm -rf _build/*
  rsync -a --exclude _build --exclude .git --exclude scripts --exclude doc --exclude docs \
           --exclude movies --exclude .github --exclude pkgdown --exclude revdep --exclude '*.pdf' \
           --exclude tests --exclude vignettes/*_cache --exclude vignettes/*_files --exclude '*.html' \
           --exclude '*.css' --exclude .DS_Store --exclude '*.so' --exclude '*.o' --exclude '*.Rproj' \
           --exclude vignettes/figures --exclude .gitignore --exclude .Rbuildignore --exclude .Rhistory \
           --exclude .Rproj.user --exclude build_site.R --exclude index.md --exclude justfile \
           --exclude '*.swp' \
           . _build
  R --vanilla -q -e "devtools::check('_build', check_dir='_build/check')"

