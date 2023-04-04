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

# remove the objects for the loop if they exist
rm(new_items, csv_items)
seq <- seq(from = 0, to = n_items, by = 100)

# loop over the sequence, to get 100 items at a time (api-limit)
for (i in seq) {
  # tracking, because this takes long
  print(i)
  
  # request 100 items in csv-format from the api starting 
  # with the corresponding number in our sequence
  new_items <- zot_api$get(query = list(limit = 100, 
                                        start = i,
                                        format = "csv"))
  # parse them to proper text
  new_items <- new_items$parse("UTF-8") 
  
  # read the text as a table, all columns need to be character or
  # bind_rows() will complain sooner or later
  new_items <- read.table(text = new_items, 
                          sep = ",", na.strings = "",
                          colClasses = "character",
                          header = TRUE)
  
  # if the object we will bind everything into already exists, we
  # use bind_rows() to add out next 100 items; if not, we create the 
  # object from (in that case) our first 100 items
  if (exists("csv_items")) {
    csv_items <- bind_rows(csv_items, new_items)
  } else {
    csv_items <- new_items
  }
  
  # wait a bit to mitigate overload / too many requests
  Sys.sleep(2)
}

colnames(csv_items) <- gsub(".", " ", colnames(csv_items), fixed = TRUE)
# save the result as our export 
write.csv(csv_items, 
          file = "data/Milet_Bibliography_CSV.csv", 
          fileEncoding = "UTF-8", 
          na = "", 
          row.names = FALSE)
