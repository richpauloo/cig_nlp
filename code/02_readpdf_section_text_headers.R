###########################################################################
# Implement Duncan's ReadPDF::getSectionText()
# this function takes a PDF that's been converted to XML and returns a list with n elems
# each element corresponds to a "section" in the PDF. Huersitics are used to 
# determine section headers, and text between heads is reconstructed.
# element names refer to the section titles.
###########################################################################

# tell R where to find the pdftohtml executable:
# see README at https://github.com/dsidavis/ReadPDF for instructions
options(PDFTOHTML = "/Users/richpauloo/Documents/GitHub/pdftohtml/src/pdftohtml")

# clone GH DSIProjects/ReadPDF/R

# Source all files from the repo to get functions in memory
ff = list.files("../richpauloo/Documents/Github/ReadPDF/R", pattern = "\\.R$", full = TRUE)
invisible(lapply(ff, source))

# all papers filepaths
library(stringr)
pf  <- list.files("../richpauloo/Desktop/2019 Citation/Papers 2/all_papers", full = TRUE)
pf2 <- list.files("../richpauloo/Desktop/2019 Citation/Papers 2/all_papers") %>% 
  str_remove_all(".pdf")

# convert all new PDFs into XML
for(i in 1:length(pf)){
  convertPDF2XML(file = pf[i], 
                 out = paste0("../richpauloo/Desktop/2019 Citation/Papers 2/all_papers_xml/", pf2[i], ".xml"))
}

# read all XML into a list and run getSectionText() on all files within an tryCatch


###########################################################################

###########################################################################
