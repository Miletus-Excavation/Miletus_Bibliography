

source("Exporte_PDF/setup_tex_files.R")
source("Bibliographie.R")

#library(knitr)
#knitr::knit2html("test.Rmd")
library(tinytex)

setwd("Exporte_PDF/")
tinytex::latexmk("milet-bibliographie-summary.tex", 
                 bib_engine = "biber", min_times = 3)
setwd("..")
