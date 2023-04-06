library(dplyr)

message("Checking for possible problems...")

# depends on result of: 
# "R/setup_tex_files.R"
if (!exists("key")) {
  source("R/setup_tex_files.R", local = FALSE, echo = TRUE)
}

# testing:
# bib$tex_key[c(1,2,37)] <- "zonk"
# regex of how the keys should look according to my settings
texkey_regex <- "^[a-z-]+_[[:alnum:]]+_(\\d{4}[a-z]{0,1}|o\\.J\\.)$"
texkey_false <- which(!grepl(texkey_regex, bib$tex_key))

# see if there are any wrong keys
# View(bib[texkey_false,c("Author", "Publication.Year", "Title", "tex_key")])
# View(bib[,c("Author", "Publication.Year", "Title", "tex_key")])

# save them to check out / match in regex editor?
filename <- "out/wrong_keys.csv"
wrong_keys <- bib[texkey_false, ]
if (nrow(wrong_keys) > 0) {
  wrong_keys <- wrong_keys %>%
    select(Key, Author, Publication.Year, Title, tex_key)
  warning(paste0("There are ", nrow(wrong_keys), 
                 " items with suspect BibTeX-keys!\n",
                 "Please check them and correct this.\n",
                 "List of affected items saved to: ", filename))
} else {
  wrong_keys <- as.data.frame("all good")
}
write.csv(wrong_keys, file = filename)


# check for duplicate keys in bib
filename <- "out/duplicate_keys.csv"
dupl_keys <- table(bib$tex_key)
dupl_keys <- dupl_keys[dupl_keys > 1]
if (length(dupl_keys) > 0) {
  dupl_keys <- bib %>%
    filter(tex_key %in% names(dupl_keys)) %>%
    select(Key, Author, Publication.Year, Title, tex_key)
  warning(paste0("There are ", nrow(dupl_keys), 
                 " items with duplicate keys!\n",
                 "Please check them and correct this.\n",
                 "List of affected items saved to: ", filename))
} else {
  dupl_keys <- as.data.frame("all good")
}
write.csv(dupl_keys, file = filename)


# fresh start
if (exists("bib_csv")) {
  bib <- bib_csv
} else {
  bib <- read.csv("data/Milet_Bibliography_CSV.csv", encoding = "UTF-8", na.strings = "")
}

tag_groups <- c("02 Allgemeine Darstellungen / Topographie", 
                "03 Funde aus Milet", 
                "03 Funde aus Milet", 
                "03-05 Funde: Keramik",
                "06 Importe aus Milet andernorts",
                "07 Architektur", 
                "08 Kulte und Kulteinrichtungen",
                "10 politische Geschichte")

tag_test <- bib %>%
  select(Key, Manual.Tags) %>%
  mutate(Manual.Tags = strsplit(Manual.Tags, "; "))

tag_test <- as.list(tag_test)
names(tag_test$Manual.Tags) <- tag_test$Key
tag_test <- tag_test$Manual.Tags

checkfortags <- lapply(tag_test, function(x) {
  check_groups <- x %in% tag_groups
  check_sys <- !grepl("^\\d\\d", x)
  if (any(check_groups)) {
    subg_check <- grepl("^\\d\\d-\\d\\d", x)
    if ("03-05 Funde: Keramik" %in% x) {
      subg_check <- grepl("^\\d\\d-\\d\\d-\\d\\d", x)
    }
    return(any(subg_check))
  } else if (all(check_sys)) {
    return(FALSE)
  }
})

checkfortags <- unlist(checkfortags, use.names = TRUE)

checkfortags <- checkfortags[!checkfortags]

checkfortags <- bib %>% 
  filter(Key %in% names(checkfortags)) %>%
  select(Key, Author, Publication.Year, Title, Manual.Tags)

filename <- "out/items_without_precise_tags.csv"
if (nrow(checkfortags) > 0) {
  warning(paste0("There are ", nrow(checkfortags), 
                 " items without precise tags (only groups)!\n",
                 "Please check them and correct this.\n",
                 "List of affected items saved to: ", filename))
}
write.csv(checkfortags, file = filename)

message("Done checking.")