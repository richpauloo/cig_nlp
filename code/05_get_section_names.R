##########################################################################
# the purpose of this script is to take the output of 
# `04_read_pdf_in_chunks.R` and: 
# (1) identify which PDFs threw parsing errors on Read
# (2) for pdfs that successfully parsed, extract the text & section name
#     of each software mention. These will be input into the visualization
#     functions written in `01_extract_mention_windows.Rds` based on
#     incorrectly parsed PDFs via the `pdftools` package.
##########################################################################
# packages
library(XML)
library(dplyr)

# tell R where to find the pdftohtml executable:
options(PDFTOHTML = "/Users/richpauloo/Documents/GitHub/pdftohtml/src/pdftohtml")

# clone GH DSIProjects/ReadPDF/R for package functions, then
# source all `.R` files into R 
ff = list.files("/Users/richpauloo/Documents/Github/ReadPDF/R", 
                pattern = "\\.R$", 
                full = TRUE)
invisible(lapply(ff, source))

# read output of `getSectionText`: named lists
fp <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/data",
                 pattern = "t", full = TRUE)
d <- vector("list", length = length(fp))
for(i in 1:length(d)){
  d[[i]] <- readRDS(fp[i])
}
d <- do.call(c, d)


##########################################################################
# Locate errors from `readPDFXML` and `getSectionText`
##########################################################################

# failures were observed during `getSectionText` when list length is 0 or 1, 
# or the object size is prohibitively small to be a successful read
# these two conditions are true for all failures

# all pdf files
af  <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers")

# getSectionText and readPDFXML success/failures 
success <- af[!sapply(d, function(x){length(x) %in% 0:1})]
fail    <- af[sapply(d, function(x){length(x) %in% 0:1})]

##########################################################################
# View list element names (section names) for each of the successes
##########################################################################

# subset for successes
s <- d[!sapply(d, function(x){length(x) %in% 0:1})]
names(s) <- success # label these list elements

# some of the objects are very long lists, and break when you try to 
# collapse their elements into a vector. I did some tests, and lists
# with length > 75 cause this problem. This amounts to 7 elements
sum(sapply(s, function(x){length(x) > 75}))

s <- s[sapply(s, function(x){length(x) <= 75})]


# collapse all nested list elements into vectors
for(i in 1:length(s)){
  s[[i]] <- lapply(s[[i]], 
                   function(x){
                     paste(x, collapse = " ")
                     }
                   )
  }

# combine into dataframes
for(i in 1:length(s)){
  s[[i]] <- as.data.frame(s[[i]]) %>% 
    tidyr::gather(section_name, text)
  s[[i]]$paper <- names(s)[i]
  s[[i]] <- dplyr::select(s[[i]], paper, section_name, text)
}

df <- dplyr::bind_rows(s)
df <- df %>% 
  mutate(nchar = nchar(text),
         text = substr(text, 1, 500),
         section_name = tolower(section_name)) %>% 
  select(paper, section_name, nchar, text)


##########################################################################
# Find subset of papers from a big journal that `getSectionText` works on
##########################################################################

# identify and label tables
df <- mutate(df, section_name = ifelse(grepl("table", section_name), 
                                       "table", 
                                       section_name)) 

# drop tables
df <- filter(df, section_name != "table")


# join to journal names
library(readr)
pj_key <- read_csv("/Users/richpauloo/Documents/GitHub/cig_nlp/rich_data/paper_journal_key.csv")
df <- left_join(df, pj_key, by = c("paper" = "file"))

# Lorraine gave instructions on how to aggregate in bib_df_LJH.xlsx
nature <- c("Nature Communications", "Nature Geoscience")
geophy <- c("Journal of Geophysical Research: Solid Earth","Journal of Geophysical Research: Planets",
            "Journal of Geophysical Research B", "Journal of Geophysical Research: Oceans",
            "Journal of geophysical research.")
df <- df %>% 
  mutate(journal = case_when(journal %in% nature ~ "Nature",
                             journal %in% geophy ~ "Journal of Geophysical Research",
                             TRUE ~ journal))

# most common journals to explore 
data.frame(file = list.files("/Users/richpauloo/Desktop/2019 CItation/Papers/all_papers")) %>% 
  left_join(pj_key, by = "file") %>% 
  mutate(journal = case_when(journal %in% nature ~ "Nature",
                             journal %in% geophy ~ "Journal of Geophysical Research",
                             TRUE ~ journal)) %>% 
  count(journal) %>% 
  arrange(desc(n))

# journal subsets: are there any that all parse well in ReadPDF?
j1 <- filter(df, journal %in% "Geophysical Journal International")   # 54
j2 <- filter(df, journal %in% "Journal of Geophysical Research")     # 34
j3 <- filter(df, journal %in% "Earth and Planetary Science Letters") # 25
j4 <- filter(df, journal %in% "Geochemistry Geophysics Geosystems")  # 20
j5 <- filter(df, journal %in% "Geophysical Research Letters")        # 16
j6 <- filter(df, journal %in% "Physics of the Earth and Planetary Interiors") # 13 

# write
write_csv(filter(j1, nchar >= 50), "/Users/richpauloo/Documents/GitHub/cig_nlp/rich_data/j1.csv")
write_csv(filter(j2, nchar >= 50), "/Users/richpauloo/Documents/GitHub/cig_nlp/rich_data/j2.csv")
write_csv(filter(j3, nchar >= 50), "/Users/richpauloo/Documents/GitHub/cig_nlp/rich_data/j3.csv")


##########################################################################
# Result: j2 and j3 scrape well. Focus a subset analysis of section name
# on these 2 journals
##########################################################################







