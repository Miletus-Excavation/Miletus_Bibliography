

source("R/figures.R", 
       local = FALSE, echo = TRUE, verbose = TRUE)
source("R/setup_tex_files.R", 
       local = FALSE, echo = TRUE, verbose = TRUE)



if (length(bib[texkey_false,"tex_key"]) > 0) {
  wrong_tex <- bib[texkey_false,"tex_key"]
} else {
  wrong_tex <- "all good"
}

# check for duplicate keys in db
key_table <- table(bib$tex_key)
key_table <- key_table[key_table > 1]
if (length(key_table) > 0) {
  key_table
} else {
  key_table <- "all good"
}


#Define the file name that will be deleted
out_files <- c("out/wrong_tex.txt",
               "out/duplicate_keys.txt",
               "out/defbibcheck_by_year.tex", 
               "out/bibsections_by_year.tex", 
               "out/bibstructure_by_author.tex")

#Check its existence
if (any(file.exists(out_files))) {
  #Delete file if it exists
  file.remove(out_files[file.exists(out_files)])
}

writeLines(wrong_tex, con = "out/wrong_tex.txt")
writeLines(key_table, con = "out/duplicate_keys.txt")
writeLines(defbibcheck, 
           con = "out/defbibcheck_by_year.tex", 
           useBytes = TRUE)
writeLines(bibsections, 
           con = "out/bibsections_by_year.tex", 
           useBytes = TRUE)
writeLines(bibstructure, 
           con = "out/bibstructure_by_author.tex", 
           useBytes = TRUE)
