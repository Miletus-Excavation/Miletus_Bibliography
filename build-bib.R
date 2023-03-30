# build-bib.R

install.packages("ggplot2", repos = "https://cloud.r-project.org")
install.packages("dplyr", repos = "https://cloud.r-project.org")
install.packages("stringi", repos = "https://cloud.r-project.org")
install.packages("reshape2", repos = "https://cloud.r-project.org")

source("Exporte_PDF/setup_tex_files.R")
source("Bibliographie.R")

source("test.Rmd")