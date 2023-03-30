# build-bib.R
packages <- c("ggplot", "dplyr", "stringi", "knitr")

install.packages(packages, repos = "https://cloud.r-project.org")

source("Exporte_PDF/setup_tex_files.R")
source("Bibliographie.R")

knitr::knit2html("test.Rmd")
