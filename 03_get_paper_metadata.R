library(pdftools)
library(tokenizers)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(colormap)

fp <- "C:/Users/rpauloo/Desktop/2019 CItation/Papers/" # PC
# fp <- "/Users/richpauloo/Desktop/2019 CItation/Papers/" # Mac

# paper names
p <- list.files(paste0(fp, "all_papers"))

# doi REGEX
doi_regex <- "doi:(?=[:digit:])|doi: (?=[:digit:])|DOI:(?=[:digit:])|DOI: (?=[:digit:])|doi (?=[:digit:])|doi(?=[:digit:])|DOI (?=[:digit:])|DOI(?=[:digit:])"

# bring in raw text
raw <- read_rds("raw.rds")

# metadata
doi_loc <- vector("list", length = length(p))
doi_loc <- lapply(raw, str_locate, doi_regex)
bind_rows(doi_loc)

# add papers, bind to df
doi_df <- do.call(rbind.data.frame, doi_loc) %>% mutate(paper = p)

# sanity check
str_view(raw[[172]], doi_regex)

# remove papers unlikely to have true doi
doi_df <- doi_df %>% 
  mutate(start = ifelse(start <= 500, start, NA),
         end   = ifelse(start <= 500, end, NA))

# subset raw and doi_df by only the ones we have doi for
raw_sub    <- raw[!is.na(doi_df[,1])]
doi_df_sub <- doi_df %>% filter(!is.na(start))

temp <- vector("list", length = length(raw_sub))
for(i in 1:length(raw_sub)){
  temp[[i]] <- str_sub(string = raw_sub[[i]], start = doi_df_sub[i, 2], end = doi_df_sub[i, 2] + 50)
}

# send a list of paper dois to scopus and return journal names

# send a list of journal names to scopus and return impact factor