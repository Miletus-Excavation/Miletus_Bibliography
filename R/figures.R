message("Making new figures for exports.")

if (exists("bib_csv")) {
  bib <- bib_csv
} else {
  bib <- read.csv("data/Milet_Bibliography_CSV.csv", encoding = "UTF-8", na.strings = "")
}


bib$Publication.Year <- as.numeric(bib$Publication.Year)


bib$Date.Added <- as.Date(bib$Date.Added)
bib$Date.Modified <- as.Date(bib$Date.Modified)



p <- bib %>%
  ggplot(aes(x = Publication.Year, fill = Item.Type)) +
  geom_histogram(binwidth = 10, alpha = 0.8) +
  scale_x_continuous(breaks = seq(1750, 2030, 10)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "grey60", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey80", linetype = "dashed")) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Year of Publication", y = "Number of Publications", 
       title = "Entries in the Miletus Bibliography Database")

# p
filename <- "out/figures/mil-pubs-by-year-type.png"
ggsave(filename, p, 
       width = 1200, height = 500, units = "px", 
       dpi = 100, device = "png")
message(paste0("Saved: ", filename))

p <- bib %>%
  ggplot(aes(x = Publication.Year)) +
  geom_histogram(binwidth = 5, alpha = 0.8, fill = "#3b515b") +
  scale_x_continuous(breaks = seq(1750, 2030, 25)) +
  scale_y_continuous(breaks = seq(0, 300, 25)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "grey60", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey80", linetype = "dashed")) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Year of Publication", y = "Number of Publications", 
       title = "Entries in the Miletus Bibliography Database")

# p
filename <- "out/figures/mil-pubs-by-year.png"
ggsave(filename, p, 
       width = 1200, height = 500, units = "px", 
       dpi = 100, device = "png")
message(paste0("Saved: ", filename))



### With Tags!


bib_tags <- bib$Manual.Tags
names(bib_tags) <- bib$Publication.Year

bib_tags <- lapply(strsplit(bib_tags, "; "), unlist)

bib_tags <- stack(bib_tags)

unique_tags <- sort(unique(bib_tags$values))

sys_tags <- unique_tags[grepl("^\\d\\d", unique_tags)]


bib_tags <- bib_tags %>%
  filter(values %in% sys_tags) %>%
  mutate(year = as.numeric(as.character(ind))) %>%
  mutate(group = gsub("[ -].*", "", values)) %>%
  select(-ind)

# bib_tags

groups <- sys_tags[grepl("^\\d\\d ", sys_tags)]
# groups
names(groups) <- gsub("[ -].*", "", groups)

p <- bib_tags %>%
  filter(values %in% groups) %>%
  ggplot(aes(y = values, group = year, fill = year)) +
  geom_bar(alpha = 0.8) +
  scale_y_discrete(limits = rev) +
  scale_fill_viridis_c(guide = guide_colourbar(barwidth = 20)) +
  #scale_fill_viridis_d(breaks = names(groups), labels = groups) +
  theme(legend.position = "bottom",
        legend.box.margin = margin(l = -200),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        #legend.title = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "grey60", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey80", linetype = "dashed")) +
  labs(y = "Group of Keywords", x = "Number of Publications", 
       title = "Entries in the Miletus Bibliography Database", 
       fill = "Year of Publication")

# p
filename <- "out/figures/mil-pubs-by-keys.png"
ggsave(filename, p, 
       width = 1200, height = 750, units = "px", 
       dpi = 100, device = "png")
message(paste0("Saved: ", filename))


message("Done with figures.")
