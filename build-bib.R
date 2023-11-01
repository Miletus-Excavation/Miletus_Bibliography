library(dplyr)
library(crul)
library(jsonlite)
library(ggplot2)
library(stringi)

source("data/tags/get_tags.R", 
       local = FALSE)
source("R/get_bib.R", 
       local = FALSE)
source("R/figures.R", 
       local = FALSE)
source("R/setup_tex_files.R", 
       local = FALSE)


# checks should run after setup_tex_files!
source("R/checks.R", 
       local = FALSE)
