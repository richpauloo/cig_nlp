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
# Determine if errors came from `readPDFXML` or `getSectionText`
##########################################################################

# readPDFXML failures 
fail_rpdf  <- list.files("/Users/richpauloo/Documents/Github/cig_nlp/readPDFXML_errors/pdf")

# failures were observed during `getSectionText` when list length is 0 or 1, 
# or the object size is prohibitively small to be a successful read
# these two conditions are true for all failures
sapply(d, function(x){ifelse(object.size(x) < 1000, TRUE, FALSE)}) ==  
  sapply(d, function(x){length(x) %in% 0:1})

# all pdf files
af  <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers")

# getSectionText failures 
fail_gst <- af[sapply(d, function(x){length(x) %in% 0:1})]

# getSectionText failures that are NOT readPDFXML failures
fail_gst_unique <- fail_gst[!fail_gst %in% fail_rpdf]





# read in output of `04_read_pdf_in_chunks.R`
fp <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/data", 
                 pattern = "t", full.names = TRUE)
d <- vector("list", length = length(fp))
for(i in 1:length(d)){
  d[[i]] <- read_rds(fp[i])
}
d <- do.call(c, d)

# find all 0 or 1 length lists
d[sapply(d, function(x){return(ifelse(length(x) %in% 0:1, TRUE, FALSE))})]
