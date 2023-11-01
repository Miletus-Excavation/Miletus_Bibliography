log_tex <- file("out/logs/setup_tex_files.log", open = "at")
sink(log_tex, type = "message")

message("Producing *.tex-files for exports.")

if (exists("bib_csv")) {
  bib <- bib_csv
} else {
  bib <- read.csv("data/Milet_Bibliography_CSV.csv", encoding = "UTF-8", na.strings = "")
}

remove_na_cols <- function(data) {
  na_cols <- apply(data, function(x) all(is.na(x)), MARGIN = 2)
  data <- data[, !na_cols]
  return(data)
}


bib <- bib %>%
  remove_na_cols() %>%
  type.convert(as.is = TRUE)

# check if any entry is missing a Citation Key!
# if so, create an improvised - issakjdnsadkjsadd i cannot to that because it does not work like that. 
cit_keys <- unlist(lapply(bib$Extra, function(x) grepl("Citation Key: ", x)))
if (any(!cit_keys)) {
  missing_cit_key_index <- which(cit_keys == FALSE)
  message(paste0(length(missing_cit_key_index), " items do not have a pinned LaTeX-Citation Key! Please fix this."))
  message("These are the entries: ")
  entries_missing_keys <- list()
  for (i in missing_cit_key_index) {
    entry <- paste0(bib$Author[i], " ", bib$Publication.Year[i], ": ", bib$Title[i])
    print(entry)
    entries_missing_keys <- append(entries_missing_keys, entry)
    bib$Extra[i] <- "Citation Key: MISSING"
  }
} else {
  message("All entries have pinned keys.")
}

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

# save defbibcheck
filename <- "out/defbibcheck_by_year.tex"
writeLines(defbibcheck, 
           con = filename, 
           useBytes = TRUE)
message(paste0("Saved: ", filename))

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
filename <- "out/bibsections_by_year.tex"
writeLines(bibsections, 
           con = filename, 
           useBytes = TRUE)
message(paste0("Saved: ", filename))

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
key <- unlist(lapply(key, function(x) 
  if(length(x) >= 2) {
    x[[2]]
  } else {
    x
  }
))

head(key)
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
filename <- "out/bibstructure_by_author.tex"
writeLines(bibstructure, 
           con = filename, 
           useBytes = TRUE)
message(paste0("Saved: ", filename))

##### Setup by keyword
tagslist <- bib %>%
  select(Key, Manual.Tags) %>%
  mutate(Manual.Tags = strsplit(Manual.Tags, "; "))

tagslist <- as.list(tagslist)
names(tagslist$Manual.Tags) <- tagslist$Key
tagslist <- tagslist$Manual.Tags

no_tags <- "NA"
bibstructure <- "NA"
# for each letter in out alphabet
for(i in 1:nrow(tags_sys)) {
  # get all the keys we need
  subset_keys <- lapply(tagslist, function(x) { tags_sys$tag[i] %in% x })
  subset_keys <- unlist(subset_keys, use.names = TRUE)
  subset_keys <- names(subset_keys[subset_keys])
  subset <- bib[bib$Key %in% subset_keys, ]
  
  nextline <- paste0("\\", tags_sys$sys[i], 
                     "[", gsub("^.*: ", "", tags_sys$DE[i]), "]",
                     "{", tags_sys$DE[i], "}\n")
  bibstructure <- c(bibstructure, nextline)
  
  add_to_this_level <- lapply(tagslist[subset_keys], function(x) {
    if (tags_sys$sys[i] == "section") {
      regex <- paste0("^", tags_sys$Gruppe[i], "-\\d\\d ")
      has_subsection_tag <- any(grepl(regex, unlist(x)))
      check <- has_subsection_tag
    } else if (tags_sys$sys[i] == "subsection") {
      regex <- paste0("^", tags_sys$Gruppe[i], "-", 
                      tags_sys$Untergruppe_1[i], "-\\d\\d ")
      has_subsubsection_tag <- any(grepl(regex, unlist(x)))
      check <- has_subsubsection_tag
    } else {
      check <- FALSE
    }
    if (!check) {
      TRUE
    } else {
      FALSE
    }
  })
  add_to_this_level <- unlist(add_to_this_level, use.names = TRUE)
  if (any(add_to_this_level)) {
    if (tags_sys$tag[i] == "01 Grabungs und Arbeitsberichte") {
      to_cite <- subset %>%
        filter(Key %in% names(add_to_this_level[add_to_this_level])) %>%
        arrange(Publication.Year, Author, .locale = sort_locale) %>%
        pull(tex_key)
    } else {
      to_cite <- subset %>%
        filter(Key %in% names(add_to_this_level[add_to_this_level])) %>%
        arrange(Author, Publication.Year, .locale = sort_locale) %>%
        pull(tex_key)
    }
    to_cite <- paste("\\fullcite{", to_cite, "}", sep = "", collapse = "\n\n")
    bibstructure <- c(bibstructure, to_cite)
  } else {
    msg <- paste0("Somehow some keys do not have tags? At tag: ", tags_sys$tag[i])
    no_tags <- c(no_tags, names(add_to_this_level))
    warning(msg)
  }
  bibstructure <- c(bibstructure, "\n\n")
  # View(bib %>% filter(tex_key %in% to_cite) %>% select(tex_key, Manual.Tags)) 
}
# remove the NA
bibstructure <- bibstructure[-1]

#bib_csv[bib_csv$Key %in% no_tags, ]

filename <- "out/bibstructure_by_keyword.tex"
writeLines(bibstructure, 
           con = filename, 
           useBytes = TRUE)
message(paste0("Saved: ", filename))



message("Done with *.tex-files.")
