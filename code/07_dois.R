##################################################
# Grab dois for each PDF
##################################################

library(readr)
library(stringr)
library(pdftools)

p <- read_rds("C:/Users/rpauloo/Documents/Github/cig_nlp/rich_data/all_papers.rds")

raw <- read_rds("C:/Users/rpauloo/Documents/Github/cig_nlp/rich_data/raw2.rds")

dois <- sapply(raw, function(x) stringr::str_extract(x, pattern = "doi:[:digit:]{1,}.[:digit:]{1,}/[:alpha:]{1,}.[:digit:]{1,}"))


View(data.frame(p, dois))