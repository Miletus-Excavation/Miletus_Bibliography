library(ggplot2)
library(tidyverse)
library(lubridate)


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


# Activity
bib %>%
  select(X.U.FEFF.Key, Date.Added, Date.Modified) %>%
  melt() %>%
  mutate(month = format(value, "%Y-%m")) %>%
  filter(variable == "Date.Added") %>%
  ggplot(aes(x = month, fill = variable)) +
  geom_bar() +
  #scale_x_date(date_breaks = "1 month") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "grey60", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey80", linetype = "dashed")) +
  guides(fill = guide_legend(nrow = 1))

colnames(bib)


check <- bib$Num.Pages
check[!is.na(check)]

ind <- which(!is.na(bib$Num.Pages))

bib[ind,"Title"]




