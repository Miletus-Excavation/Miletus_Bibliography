## Setup

# API key is stored in .Renviron, edit with file.edit("~/.Renviron")
# add row there: ZOTERO_API_KEY = "yourapikey"
# get this from secret...
# (we dont actually need it as long as we do not change things)
headers <- list(`Zotero-API-Key` = Sys.getenv("ZOTERO_API_KEY"),
                Accept = "application/json")

# 4475959 = Group ID of Miletus Bibliography
zot_api <- crul::HttpClient$new(url = "https://api.zotero.org/groups/4475959/items",
                                headers = headers)

# Get the total number of items in the database: 
n_items <- zot_api$get()$response_headers$`total-results` %>%
  as.numeric()

get_zotero_items <- function(zot_api, start_i, format_ch) {
  # tracking, because this takes long
  message(paste("Getting items", start_i, "to", start_i+99))
  
  query <- list(limit = 100, 
                start = start_i,
                format = format_ch)
  # request 100 items in csv-format from the api starting 
  # with the corresponding number in our sequence
  new_items <- zot_api$get(query = query)
  if (new_items$status_code != 200) {
    if (new_items$status_code == 403) {
      stop("Authentication failed. Stopping.")
    } else if (new_items$status_code == 503) {
      stop("Server under maintenance. Stopping.")
    }
    retry_message <- function(x, secs) {
      msg <- paste0("Status code: ", x$status_code, 
                    ". Retrying after ", round(secs), " seconds.")
      message(msg)
    }
    new_items <- zot_api$retry("get", 
                               query = query, 
                               onwait = retry_message)
  }
  # parse them to proper text
  new_items <- new_items$parse("UTF-8") 
  
  return(new_items)
}




## Download CSV files here

message(paste0("Downloading the Bibliography as CSV from Zotero.\n",
               "There are ", n_items, " items."))
# generate the sequence from the number of items
seq <- seq(from = 0, to = n_items, by = 100)
# loop over the sequence, to get 100 items at a time (api-limit)
bib_csv <- lapply(seq, function(x) {
  new_items <- get_zotero_items(zot_api, start_i = x, format_ch = "csv")
  # read the text as a table, all columns need to be character or
  # bind_rows() will complain sooner or later
  new_items <- read.table(text = new_items, 
                          sep = ",", na.strings = "",
                          colClasses = "character",
                          header = TRUE)
  return(new_items)
})
bib_csv <- do.call(bind_rows, bib_csv) 

wrong_keycol <- which(colnames(bib_csv) == "X.U.FEFF.Key")
correct_keycol <- which(colnames(bib_csv) == "Key")
if (length(wrong_keycol) == 1) {
  colnames(bib_csv)[wrong_keycol] <- "Key"
  message("'X.U.FEFF.Key' column exists. Renaming to 'Key'.")
} else if (length(wrong_keycol) == 1) {
  message("'Key' column exists.")
} else {
  message("Please check the columns after downloading the library, unforeseen situation.")
}

# save the result as our export 
filename <- "data/Milet_Bibliography_CSV.csv"
message(paste0("Finished downloading.\n",
               "Saving to: ", filename))
save_csv <- bib_csv
colnames(save_csv) <- gsub(".", " ", colnames(save_csv), fixed = TRUE)
write.csv(save_csv, 
          file = filename, 
          fileEncoding = "UTF-8", 
          na = "", 
          row.names = FALSE)
rm(save_csv)

####
# prep function so substitute some characters that will never work
gsub_nonworking_chars <- function(texstring, type = "biblatex") {
  if (type == "bibtex") {
    texstring <- stri_trans_general(texstring, "Any-Latin")
    #texstring <- stri_trans_general(texstring, "Latin-ASCII")
    #texstring <- iconv(texstring, from = "ASCII", to = "UTF-8")
  }
  texstring <- gsub("ḗ", "{=e}", texstring)
  texstring <- gsub("\\%", "{\\%}", texstring, fixed = TRUE)
  texstring <- gsub("\u200B", "", texstring, fixed = TRUE)
  texstring <- gsub("\u2013", "--", texstring, fixed = TRUE)
  return(texstring)
}

## Download BibLaTeX files here

#####
message(paste0("Downloading the Bibliography as BibLaTeX from Zotero.\n",
               "There are ", n_items, " items."))
# generate the sequence from the number of items
seq <- seq(from = 0, to = n_items, by = 100)
# loop over the sequence, to get 100 items at a time (api-limit)
bib_biblatex <- lapply(seq, function(x) {
  new_items <- get_zotero_items(zot_api, start_i = x, format_ch = "biblatex")
  return(new_items)
})
bib_biblatex <- do.call(paste, bib_biblatex)
# bib_biblatex_tmp <- bib_biblatex
# bib_biblatex <- bib_biblatex_tmp
#bib_biblatex <- iconv(bib_biblatex, from = "UTF-8", to = "UTF-8")

bib_biblatex <- gsub_nonworking_chars(bib_biblatex, type = "biblatex")

#RefManageR::
# save the result as our export 
filename <- "data/Milet_Bibliography_BibLaTeX.bib"
message(paste0("Finished downloading.\n",
               "Saving to: ", filename))
cat(bib_biblatex, file = filename)


rm(bib_biblatex)

## Download BibTeX files here

#####
message(paste0("Downloading the Bibliography as BibTeX from Zotero.\n",
               "There are ", n_items, " items."))
# generate the sequence from the number of items
seq <- seq(from = 0, to = n_items, by = 100)
# loop over the sequence, to get 100 items at a time (api-limit)
bib_bibtex <- lapply(seq, function(x) {
  new_items <- get_zotero_items(zot_api, start_i = x, format_ch = "bibtex")
  return(new_items)
})
bib_bibtex <- do.call(paste, bib_bibtex)
# bib_bibtex_tmp <- bib_bibtex
# bib_bibtex <- bib_bibtex_tmp

bib_bibtex <- gsub_nonworking_chars(bib_bibtex, type = "bibtex")

# save the result as our export 
filename <- "data/Milet_Bibliography_BibTeX.bib"
message(paste0("Finished downloading.\n",
               "Saving to: ", filename))
cat(bib_bibtex, file = filename)
rm(bib_bibtex)






## Download RIS files here

message(paste0("Downloading the Bibliography as RIS from Zotero.\n",
               "There are ", n_items, " items."))
# generate the sequence from the number of items
seq <- seq(from = 0, to = n_items, by = 100)
# loop over the sequence, to get 100 items at a time (api-limit)
bib_ris <- lapply(seq, function(x) {
  new_items <- get_zotero_items(zot_api, start_i = x, format_ch = "ris")
  return(new_items)
})
bib_ris <- paste(bib_ris, sep = "\n")
bib_ris <- gsub("\r\n", "\r", bib_ris)
# save the result as our export 
filename <- "data/Milet_Bibliography_RIS.ris"
message(paste0("Finished downloading.\n",
               "Saving to: ", filename))
cat(bib_ris, file = filename)
