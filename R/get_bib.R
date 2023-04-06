library(crul)
library(jsonlite)
library(dplyr)

# API key is stored in .Renviron, edit with file.edit("~/.Renviron")
# add row there: ZOTERO_API_KEY = "yourapikey"
# get this from secret...
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
                start = 1,#start_i,
                format = "csv")#format_ch)
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

# remove the objects for the loop if they exist
rm(new_items, csv_items)
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
bib_csv <- do.call(bind_rows, csv_items)
colnames(bib_csv) <- gsub(".", " ", colnames(bib_csv), fixed = TRUE)

# save the result as our export 
write.csv(bib_csv, 
          file = "data/Milet_Bibliography_CSV.csv", 
          fileEncoding = "UTF-8", 
          na = "", 
          row.names = FALSE)
