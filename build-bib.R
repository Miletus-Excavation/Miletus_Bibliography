
source("Exporte_PDF/setup_tex_files.R")
source("Bibliographie.R")

knitr::knit2html("test.Rmd")
tinytex::pdflatex("Exporte_PDF/milet-bibliographie-summary.tex")
