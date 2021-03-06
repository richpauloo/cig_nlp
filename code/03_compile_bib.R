library(dplyr)
library(RefManageR) # for formatting bibtex
library(xlsx)
library(colormap)
library(ggplot2)

fp <- "F:/Box Sync/2019 CItation/bib/" # work
#fp <- "/Users/richpauloo/Desktop/2019 Citation/"

b0 <- ReadBib(paste0(fp, "2015pre bibtex_export v2.bib")) %>% as.data.frame() # pre-2015
b1 <- ReadBib(paste0(fp, "2015 bibtex_export v4.2.bib")) %>% as.data.frame()
b2 <- ReadBib(paste0(fp, "2016 bibtex_export v2.2.bib")) %>% as.data.frame()
b3 <- ReadBib(paste0(fp, "2017 bibtex_export v4.2.bib")) %>% as.data.frame()
b4 <- ReadBib(paste0(fp, "2018 bibtex_export v4.2.bib")) %>% as.data.frame()

b <- bind_rows(b0, b1, b2, b3, b4)

# write to data frame for manual cleaning
#write.xlsx(b, "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_data/bib_df.xlsx", row.names = FALSE)

# Lorraine gave instructions on how to aggregate in bib_df_LJH.xlsx
nature <- c("Nature Communications", "Nature Geoscience")
geophy <- c("Journal of Geophysical Research: Solid Earth","Journal of Geophysical Research: Planets",
            "Journal of Geophysical Research B", "Journal of Geophysical Research: Oceans",
            "Journal of geophysical research.")
b <- b %>% 
  mutate(journal = case_when(journal %in% nature ~ "Nature",
                             journal %in% geophy ~ "Journal of Geophysical Research",
                             TRUE ~ journal))

# count journals to determine popular and not so popular ones
b_n <- count(b, journal)

b <- left_join(b, b_n, by = "journal")

# arrange and reorganize columns for easy viewing
b <- b %>% arrange(desc(n)) %>% select(bibtype:title, year:address, journal, n)

# get rid of extra years
b <- distinct(b, title, .keep_all = TRUE) %>% 
  mutate(year = as.numeric(year))

# pre and post 2010
pre_2010  <- filter(b, year < 2010)
post_2009 <- filter(b, year >= 2010)

# write for lorraine
write.xlsx(pre_2010, "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_data/bib_df_pre_2010.xlsx", row.names = FALSE)
write.xlsx(post_2009, "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_data/bib_df_post_2009.xlsx", row.names = FALSE)

# write to data frame for manual cleaning
write.xlsx(b, "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_data/bib_df.xlsx", row.names = FALSE)

# cutoff = 10 journals
b <- b %>% mutate(journal = ifelse(n < 10, "ZZZ", journal))

# total number of unique journals
tu <- b %>% mutate(year = as.numeric(year)) %>% 
  count(journal, year) %>% 
  tidyr::spread(year, n)

#readr::write_csv(tu, "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_plots/total_uniqie_journals.csv")


# papers published per year
b %>% mutate(year = as.numeric(year)) %>% 
  filter(year >= 1990) %>% 
  count(year) %>% 
  ggplot(aes(year, n)) +
  geom_line() +
  labs(x = "Year", y = "Count") +
  theme_minimal()

# papers per year per journal
nam <- sort(unique(b$journal))
nam2 <- nam
nam2[length(nam)] <- "Other"
val <- colormap(colormap = colormaps$jet, nshades = length(nam)-1)

# with other journals
p1 <- b %>% mutate(year = as.numeric(year)) %>% 
  count(journal, year) %>% 
  ggplot(aes(year, n, fill = journal)) +
  geom_col() + 
  #scale_fill_viridis_d() +
  labs(x = "Year", y = "Count", fill = "Journal") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(breaks = nam, labels = nam2, values = c(val, "grey50")) +
  scale_x_continuous(breaks = as.numeric(sort(unique(b$year))),
                     labels = sort(unique(b$year))) +
  guides(fill = guide_legend(nrow = 6))
p1

# without other journals
p2 <- b %>% mutate(year = as.numeric(year)) %>% 
  filter(year >= 1990, journal != "ZZZ") %>% 
  count(journal, year) %>% 
  ggplot(aes(year, n, fill = journal)) +
  geom_col() + 
  #scale_fill_viridis_d() +
  labs(x = "Year", y = "Count", fill = "Journal", caption = "'Other' journals (n = 257) not shown") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(breaks = nam[-length(nam)], labels = nam2[-length(nam)], values = c(val))
p2

# aggreagting all, without other journals
library(forcats)
p3 <- count(b, journal) %>% 
  mutate(journal = ifelse(journal == "ZZZ", "Other", journal)) %>% 
  ggplot(aes(x = fct_reorder(journal, n), n, fill = fct_reorder(journal, n))) +
  geom_col() + 
  coord_flip() +
  scale_fill_viridis_d() +
  labs(x = "", y = "Count") +
  guides(fill = FALSE) +
  theme_minimal()
p3

p4 <- count(b, journal) %>% 
  mutate(journal = ifelse(journal == "ZZZ", "Other", journal)) %>% 
  filter(journal != "Other") %>% 
  ggplot(aes(x = fct_reorder(journal, n), n, fill = fct_reorder(journal, n))) +
  geom_col() + 
  coord_flip() +
  scale_fill_viridis_d() +
  labs(x = "", y = "Count", caption = "'Other' journals (n = 257) not shown") +
  guides(fill = FALSE) +
  theme_minimal() 
p4

ggsave(p1, filename = "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_plots/annual_journal_count.pdf", device = cairo_pdf, width = 8, height= 6)
ggsave(p2, filename = "annual_journal_count_no_other.pdf", device = cairo_pdf, width = 11, height= 7)
ggsave(p3, filename = "all_journal_count.pdf", device = cairo_pdf, width = 11, height= 7)
ggsave(p4, filename = "all_journal_count_no_other.pdf", device = cairo_pdf, width = 11, height= 7)


# write the data in p1 to xlsx
b %>% mutate(year = as.numeric(year)) %>% 
  count(journal, year) %>% 
  tidyr::spread(year, n) %>% 
  write.xlsx(., file = "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_data/annual_journal_count.xlsx")



# repeat p1, but with optkeywords
# papers per year per journal
sort(unique(b$optkeywords))

b <- b %>% 
  mutate(optkeywords = case_when(optkeywords %in% c("CitcomS","CitComS","CitcomCU") ~ "Citicom",
                                 optkeywords %in% c("inversion", "Plate reconstructions", "multiple", "Solar and Stellar Interiors","Solar and Stellar Interiors", "Sea-level change") ~ "Other",
                                 optkeywords == "SEISMIC\\_CPML" ~ "SEISMIC CPML",
                                 optkeywords %in% c("SPECFEM2D","SPECFEM3D","SPECFEM3D Cartesian","SPECFEM3D GEOTECH","SPECFEM3D GLOBE") ~ "SPECFEM",
                                 is.na(optkeywords) ~ "Other",
                                 TRUE ~ optkeywords))

nam <- sort(unique(b$optkeywords))
nam2 <- nam
nam2[length(nam)] <- "Other"
val <- colormap(colormap = colormaps$jet, nshades = length(nam)-1)


p5 <- b %>% mutate(year = as.numeric(year)) %>% 
  count(optkeywords, year) %>% 
  ggplot(aes(year, n, fill = optkeywords)) +
  geom_col() + 
  #scale_fill_viridis_d() +
  labs(x = "Year", y = "Count", fill = "Journal") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(breaks = nam, labels = nam2, values = c(val, "grey50")) +
  scale_x_continuous(breaks = as.numeric(sort(unique(b$year))),
                     labels = sort(unique(b$year))) +
  guides(fill = guide_legend(nrow = 4))
p5

ggsave(p5, filename = "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_plots/annual_software_count.pdf", device = cairo_pdf, width = 8, height= 6)

# write the data in p1 to xlsx
b %>% mutate(year = as.numeric(year)) %>% 
  count(optkeywords, year) %>% 
  tidyr::spread(year, n) %>% 
  write.xlsx(., file = "C:/Users/rpauloo/Documents/GitHub/cig_nlp/rich_data/annual_software_count.xlsx")


