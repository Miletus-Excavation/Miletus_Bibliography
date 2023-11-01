log_tags <- file("out/logs/get_tags.log", open = "at")
sink(log_tags, type = "message")

tags_sys <- read.csv("data/tags/tags_sys.csv", 
                    sep = ";", encoding = "UTF-8", na.strings = "", 
                    colClasses = c("character"))

rm <- NA
for (r in 1:nrow(tags_sys)) {
  check <- is.na(tags_sys[r, ])
  if (all(check)) {
    rm <- c(rm, r)
  }
}
rm <- rm[-1]
tags_sys <- tags_sys[-rm, ]

tags_sys$tag <- paste(tags_sys$Gruppe, "-", 
                      tags_sys$Untergruppe_1, "-", 
                      tags_sys$Untergruppe_2, " ",
                      tags_sys$DE,
                      sep = "")
tags_sys$tag <- gsub("-NA", "", tags_sys$tag)
tags_sys$tag <- gsub("--", "", tags_sys$tag)
tags_sys$tag <- gsub("- ", " ", tags_sys$tag)

tags_sys$sys <- ifelse(is.na(tags_sys$Untergruppe_1) & is.na(tags_sys$Untergruppe_2), 
                       "section", NA)
tags_sys$sys <- ifelse(!is.na(tags_sys$Untergruppe_1) & is.na(tags_sys$Untergruppe_2), 
                       "subsection", tags_sys$sys)
tags_sys$sys <- ifelse(!is.na(tags_sys$Untergruppe_1) & !is.na(tags_sys$Untergruppe_2), 
                       "subsubsection", tags_sys$sys)

rm(check, r, rm)
