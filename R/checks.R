log_checks <- file("out/logs/checks.log", open = "at")
sink(log_checks, type = "message")

pr_comment_file <- "out/pr_comment.txt"
comment_to_pr <- function(msg, file = pr_comment_file) {
  write(msg, file = file, append = TRUE)
}
# Delete old log file 
if (file.exists(pr_comment_file)) {
  #Delete file if it exists
  file.remove(pr_comment_file)
}
pr_msg <- paste0("Hi there!\n\n",
                 "I logged some additional info while ",
                 "processing the database. If you see any problem here, ",
                 "please check the GitHub-Actions-Logs as well as ",
                 "all txt- and log-files in '/out/' for more information. ",
                 "Logged on ", format(Sys.time(), '%d.%m.%Y'), 
                 " while while running checks:\n")
comment_to_pr(pr_msg)




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
  key_warning <- paste0("There are ", nrow(wrong_keys), 
                        " items with suspect BibTeX-keys! ",
                        "Please check them and correct this. ",
                        "List of affected items saved to: ", filename)
  warning(key_warning)
  comment_to_pr(paste0("\n", key_warning, "\n"))
} else {
  wrong_keys <- as.data.frame("all good")
  comment_to_pr("No strange-looking LaTeX-keys. :) \n")
}
write.csv(wrong_keys, file = filename)

cit_keys <- unlist(lapply(bib$Extra, function(x) grepl("Citation Key: ", x)))
if (exists("missing_cit_key_index")) {
  comment_to_pr("**Critical**:")
  comment_to_pr(paste0(length(missing_cit_key_index), " items do not have a pinned LaTeX-Citation Key! ",
                       "Please fix this in Zotero by adding it to the 'Extra'-field. ",
                       "The affected entries are: "))
  for (i in missing_cit_key_index) {
    comment_to_pr(paste0(bib$Author[i], " ", bib$Publication.Year[i], ": ", bib$Title[i]))
  }
  comment_to_pr("\n")
} else {
  comment_to_pr("All entries have pinned Citation Keys. :) \n")
}




# check for duplicate keys in bib
filename <- "out/duplicate_keys.csv"
dupl_keys <- table(bib$tex_key)
dupl_keys <- dupl_keys[dupl_keys > 1]
if (length(dupl_keys) > 0) {
  dupl_keys <- bib %>%
    filter(tex_key %in% names(dupl_keys)) %>%
    select(Key, Author, Publication.Year, Title, tex_key)
  dupl_warning <- paste0("There are ", nrow(dupl_keys), 
                         " items with duplicate keys! ",
                         "Please check them and correct this. ",
                         "List of affected items saved to: ", filename)
  warning(dupl_warning)
  comment_to_pr(paste0("\n", dupl_warning, "\n"))
} else {
  dupl_keys <- as.data.frame("all good")
  comment_to_pr("No duplicate LaTeX-keys. :) \n")
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
  tag_warning <- paste0("There are ", nrow(checkfortags), 
                        " items without precise tags (only groups)! ",
                        "Please check them and correct this. ",
                        "List of affected items saved to: ", filename)
  warning(tag_warning)
  comment_to_pr(paste0("\n", tag_warning, "\n"))
} else {
  comment_to_pr("No obvious problems with tags. :) \n")
}
write.csv(checkfortags, file = filename)


comment_to_pr("Thanks for checking!")

message("Done checking.")
