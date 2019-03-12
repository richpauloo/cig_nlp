##########################################################################
# The purpose of this script is to log `getSectionText()` errors
# first, read in the output of `getSectionText()` and locate errors
##########################################################################

# packages
library(XML)

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



##########################################################################
# put these failures into folders for easy access
##########################################################################

# full file paths
all_fail <- paste0("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers/",
                   fail_gst_unique)

# copy all failed XML into a dir
file.copy(all_fail,
          "/Users/richpauloo/Documents/GitHub/cig_nlp/getSectionText_errors/pdf")

# repeat for the PDFs that generated them
all_fail <- gsub(all_fail, pattern = "pdf", replacement = "xml")
all_fail <- gsub(all_fail, pattern = "all_papers", replacement = "all_papers_xml")

file.copy(all_fail,
          "/Users/richpauloo/Documents/GitHub/cig_nlp/getSectionText_errors/xml")
