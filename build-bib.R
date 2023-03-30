

source("R/setup_tex_files.R", local = FALSE)
source("R/figures.R", local = FALSE)

#library(knitr)
#knitr::knit2html("test.Rmd")
#library(tinytex)

#setwd("Exporte_PDF/")
#tinytex::latexmk("milet-bibliographie-summary.tex", 
#                 bib_engine = "biber", min_times = 3)
#setwd("..")


if (!is.null(bib[texkey_false,"tex_key"])) {
  wrong_tex <- bib[texkey_false,"tex_key"]
} else {
  wrong_tex <- "all good"
}
writeLines(wrong_tex, con = "out/wrong_tex.txt")
writeLines(defbibcheck, 
           con = "out/defbibcheck_by_year.tex", 
           useBytes = TRUE)
writeLines(bibsections, 
           con = "out/bibsections_by_year.tex", 
           useBytes = TRUE)
writeLines(bibstructure, 
           con = "out/bibstructure_by_author.tex", 
           useBytes = TRUE)
