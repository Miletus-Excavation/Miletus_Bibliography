library(dplyr)
library(stringi)
#library(utf8)


remove_na_cols <- function(data) {
  na_cols <- apply(data, function(x) all(is.na(x)), MARGIN = 2)
  data <- data[, !na_cols]
  return(data)
}

# get the bibliography from the csv file
bib <- read.csv("data/Milet_Bibliography_CSV.csv", encoding = "UTF-8", na.strings = "") %>%
  remove_na_cols() %>%
  type.convert(as.is = TRUE)

# check if Publication.Year makes sense
min(bib$Publication.Year, na.rm = TRUE)
max(bib$Publication.Year, na.rm = TRUE)

# a table of years
years_tbl <- table(bib$Publication.Year)
years <- as.numeric(names(years_tbl))

# a bit of a work-around... define a bibcheck for each year that we can use
# later to tell biblatex which entries to use
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

#write(defbibcheck, file = "out/defbibcheck_by_year.tex")


# for each year...
bibsections <- "NA"
for (y in length(years):1) {
  # we produce a section for latex
  a <- paste("\\section*{", years[y], "}", sep = "")
  # add it to the table of contents with a custom name
  a <- paste(a, "\n", "\\addcontentsline{toc}{section}{", years[y], "}%", sep = "")
  # have latex print the bibliography according to the corresponding bibcheck
  a <- paste(a, "\n", "\\printbibliography[check=yr", years[y], ",heading=none, env=compactbib]", sep = "")
  # bind
  bibsections <- c(bibsections, a)
}
# remove NA
bibsections <- bibsections[-1]

# save the file
#write(bibsections, file = "out/bibsections_by_year.tex")



# Exportieren von defbibcheck und sections nach Autoren
# get all authors
authors <- bib$Author

authors <- lapply(authors, function(x) strsplit(x, "; "))

# save authors as list/vector for later use
bib$Author.List <- authors

authors <- data.frame("name" = unique(unlist(authors)))

# selecting tr_TR as locale for sorting manually everywhere to keep 
# comparable, works best with our author list as it equals to de/en with 
# added turkish characters; also makes it easy to change if needed
# default (C) behaves unpleasantly around turkish characters
sort_locale <- "tr_TR"

authors <- authors %>%
  # replace cyrillic and greek with their latin representaion for labels
  mutate(name.latin = stri_trans_general(name, "Any-Latin")) %>%
  # get the associated first letter for sorting and sections
  mutate(firstletter = toupper(substr(name.latin, 1, 1))) %>%
  arrange(name.latin, .locale = sort_locale) %>% 
  na.omit()

# unique vector of letter, already sorted from df
letters <- unique(authors$firstletter)
letters
# check it out
# View(authors)

# the citation key is saved in "extra" with the prefix Citation Key: 
# so we split the string along that
key <- strsplit(bib$Extra, "Citation Key: ")
# and get the second element, which should be the latex-key
key <- unlist(lapply(key, function(x) x[[2]]))
# it is kind of important that there is nothing else saved in the
# extra field... be sure to check that sometimes and fix the database
# accordingly

#key[which(!grepl("_", key))]

#
key <- strsplit(key, " ")
#key
key <- unlist(lapply(key, function(x) x[[1]]))

# save the result as the tex_key
bib$tex_key <- key

## Check for wrong entries and possible errors
which(table(bib$tex_key) > 1)

# regex of how the keys should look according to my settings
texkey_regex <- "^[a-z-]+_[[:alnum:]]+_(\\d{4}[a-z]{0,1}|o\\.J\\.)$"
texkey_false <- which(!grepl(texkey_regex, bib$tex_key))

# see if there are any wrong keys
# View(bib[texkey_false,c("Author", "Publication.Year", "Title", "tex_key")])
# View(bib[,c("Author", "Publication.Year", "Title", "tex_key")])

# save them to check out / match in regex editor?
# writeLines(bib[texkey_false,"tex_key"], con = "wrong_tex.txt")



bibstructure <- "NA"
# for each letter in out alphabet
for(letter in letters) {
  # we get all the author names that start with this letter
  subset_authors <- authors[authors$firstletter == letter, ]
  # and produce a section for the letter
  bibstructure <- c(bibstructure, paste("\\section{", letter, "}\n", sep = ""))
  # for each of the authors that should be in this section
  for (i in 1:nrow(subset_authors)) {
    
    # Author.List contains a list of all authors for that publication, 
    # with the regex we get the formatted author name exactly which string
    # start and end marked, to avoid merging authors with same names but 
    # differing middle names
    regex <- paste0("^", subset_authors$name[i], "$")
    index <- lapply(bib$Author.List, function(x) {
      lapply(x, function (y) grepl(regex, y))
    })
    index <- lapply(index, function(x) any(unlist(x))) %>%
      unlist()
    
    # we select the bibliography accordingly and sort it by 
    # year of publication, then title
    bib_select <- bib[index, ]
    
    # use same sorting locale here to be consistent
    bib_select <- bib_select %>% 
      arrange(Publication.Year, Title, .locale = sort_locale)
    # and get the keys with are now in that order
    singleauthkeys <- bib_select$tex_key
    # we also record the number of publications of this author
    numberofpubs <- length(singleauthkeys)
    # and build the latex-lines for citing all of those keys
    singleauthkeys <- paste("\\fullcite{", singleauthkeys, "}", sep = "", collapse = "\n\n")
    
    # and paste our subsection markup including the number of publications
    sectionhead <- paste("\\subsection[", 
                         subset_authors$name.latin[i], 
                         " (", 
                         numberofpubs, 
                         ")]{", 
                         subset_authors$name.latin[i], 
                         "}\n", 
                         sep = "")
    # and all of that gets bound together in one big vector
    bibstructure <- c(bibstructure, paste(sectionhead, singleauthkeys, "\n", sep = ""))
  }
}
# remove the NA
bibstructure <- bibstructure[-1]

# of course, this is inefficient as it grows the vector - but whatever, 
# we don't do it every day, it doesn't take horribly long, if the bibliography
# ever counts more than 10k entries I would obviously want to rewrite, but
# I don't care for now. 

# save that so latex can have it
#writeLines(bibstructure, 
#           "out/bibstructure_by_author.tex", 
#           useBytes = TRUE)

