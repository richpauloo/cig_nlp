##################################################
# Grab dois for each PDF
##################################################

library(readr)
library(stringr)
library(pdftools)

fp <- "F:/Box Sync/2019 CItation/Papers/" # PC

# paper names
p <- list.files(paste0(fp, "all_papers")) 

raw <- read_rds("C:/Users/rpauloo/Documents/Github/cig_nlp/rich_data/raw2.rds")

dois <- sapply(raw, function(x) stringr::str_extract(x, pattern = "doi:[:digit:]{1,}.[:digit:]{1,}/[:alpha:]{1,}.[:digit:]{1,}"))

data.frame(p, dois)
