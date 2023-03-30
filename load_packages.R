#renv::init(profile = "bib")

packages <- c("dplyr", "stringi", "knitr", "rmarkdown", "ggplot2")

for (p in packages) {
  if (!suppressWarnings(require(p, character.only = TRUE))) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
  library(p, character.only = TRUE)
}
rm(packages, p)