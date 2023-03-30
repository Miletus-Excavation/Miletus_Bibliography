

source("Exporte_PDF/setup_tex_files.R")
source("Bibliographie.R")

library(knitr)
knitr::knit2html("test.Rmd")
library(tinytex)
tinytex::pdflatex("Exporte_PDF/milet-bibliographie-summary.tex")


