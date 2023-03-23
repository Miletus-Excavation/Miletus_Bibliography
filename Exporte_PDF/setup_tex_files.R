library(ggplot2)
library(tidyverse)
library(stringi)
#library(utf8)


remove_na_cols <<- function(data) {
  na_cols <- apply(data, function(x) all(is.na(x)), MARGIN = 2)
  data <- data[, !na_cols]
  return(data)
}

# Bibliographie laden
bib <- read.csv("Exporte/Milet_Bibliography_CSV.csv", encoding = "UTF-8", na.strings = "") %>%
  remove_na_cols()
bib$Publication.Year <- as.numeric(bib$Publication.Year)
bib$Date.Added <- as.Date(bib$Date.Added)
bib$Date.Modified <- as.Date(bib$Date.Modified)


# Exportieren von defbibcheck und sections nach Jahren
min(bib$Publication.Year, na.rm = TRUE)
max(bib$Publication.Year, na.rm = TRUE)

years_tbl <- table(bib$Publication.Year)
years <- as.numeric(names(years_tbl))

defbibcheck <- "NA"
for (y in length(years):1) {
  defbibcheck <- c(defbibcheck, paste("\\defbibcheck{yr", years[y], "}{%
  \\iffieldint{year}
  {\\ifnumequal{\\thefield{year}}{", years[y], "}
    {}
    {\\skipentry}}
  {\\skipentry}}\n\n", sep = ""))
}
defbibcheck <- defbibcheck[-1]

write(defbibcheck, file = "Exporte_PDF/defbibcheck_by_year.tex")



bibsections <- "NA"
for (y in length(years):1) {
  a <- paste("\\section*{", years[y], "}", sep = "")
  a <- paste(a, "\n", "\\addcontentsline{toc}{section}{", years[y], "}%", sep = "")
  a <- paste(a, "\n", "\\printbibliography[check=yr", years[y], ",heading=none, env=compactbib]", sep = "")
  bibsections <- c(bibsections, a)
}
bibsections <- bibsections[-1]


write(bibsections, file = "Exporte_PDF/bibsections_by_year.tex")



# Exportieren von defbibcheck und sections nach Autoren #TODO

authors <- bib$Author
authors <- authors %>%
  strsplit("; ") %>%
  unlist() %>%
  unique() %>%
  sort()


letters <- toupper(substr(authors, 1, 1))
letters <- stri_trans_general(letters, "cyrillic-latin/bgn")
letters <- stri_trans_general(letters, "Greek-Latin/BGN")

names(authors) <- letters

letters <- sort(unique(names(authors)))

authors

key <- strsplit(bib$Extra, "Citation Key: ")

key <- unlist(lapply(key, function(x) x[[2]]))

#key[which(!grepl("_", key))]

key <- strsplit(key, " ")
#key
key <- unlist(lapply(key, function(x) x[[1]]))

bib$tex_key <- key

letters

#library(stringi)
#stri_trans_list()

bibstructure <- "NA"
for(letter in letters) {
  subset_authors <- authors[names(authors) == letter]
  bibstructure <- c(bibstructure, paste("\\section{", letter, "}\n", sep = ""))
  for (i in 1:length(subset_authors)) {
    bib_select <- bib[grepl(subset_authors[i], bib$Author), ]
    bib_select <- bib_select %>% arrange(Publication.Year, Title)
    singleauthkeys <- bib_select$tex_key
    numberofpubs <- length(singleauthkeys)
    singleauthkeys <- paste("\\fullcite{", singleauthkeys, "}", sep = "", collapse = "\n\n")
    #author_fix <- stri_trans_general(authors[i], "cyrillic-latin/bgn")
    author_fix <- subset_authors[i]
    sectionhead <- paste("\\subsection[", 
                         author_fix, 
                         " (", 
                         numberofpubs, 
                         ")]{", 
                         author_fix, 
                         "}\n", 
                         sep = "")
    bibstructure <- c(bibstructure, paste(sectionhead, singleauthkeys, "\n", sep = ""))
  }
}
bibstructure <- bibstructure[-1]



writeLines(bibstructure, 
           "Exporte_PDF/bibstructure_by_author.tex", 
           useBytes = TRUE)



## Check for wrong entries
texkey_regex <- "^[a-z-]+_[[:alnum:]]+_(\\d{4}[a-z]{0,1}|o\\.J\\.)$"

texkey_false <- which(!grepl(texkey_regex, bib$tex_key))

View(bib[texkey_false,c("Author", "Publication.Year", "Title", "tex_key")])

writeLines(bib[texkey_false,"tex_key"], con = "wrong_tex.txt")

which(table(bib$tex_key) > 1)
