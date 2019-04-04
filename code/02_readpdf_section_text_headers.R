###########################################################################
# Purpose of this script is to implement Duncan's ReadPDF::getSectionText()
# function takes a PDF that's been converted to XML and returns a list with n elems
# each element corresponds to a "section" in the PDF. Huersitics are used to 
# determine section headers, and text between heads is reconstructed.
# element names refer to the section titles.
###########################################################################

# ensure dev version of XML is installed
# devtools::install_github("omegahat/XML")
library(XML)

# tell R where to find the pdftohtml executable:
# see README at https://github.com/dsidavis/ReadPDF for instructions
# on how to build this via makefile
options(PDFTOHTML = "/Users/richpauloo/Documents/GitHub/pdftohtml/src/pdftohtml")

# clone GH DSIProjects/ReadPDF/R for package functions, then
# source all `.R` files from the repo to get functions in memory
ff = list.files("/Users/richpauloo/Documents/Github/ReadPDF/R", pattern = "\\.R$", full = TRUE)
invisible(lapply(ff, source))

# vector of all papers filepaths
library(stringr)
pf  <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers", full = TRUE)
pf2 <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers") %>% 
  str_remove_all(".pdf")

# convert all PDFs into XML
# for(i in 1:length(pf)){
#   convertPDF2XML(file = pf[i], 
#                  out = paste0("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers_xml/", pf2[i], ".xml"))
# }

# read all XML into a list 
# xml file paths
fpxml <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers_xml", full = TRUE)
x <- vector("list", length = length(fpxml))

# develop an error handling appraoch to reading in XML files
# because, for example, between 1:20, idices 10, 17 don't read in
for(i in 1:length(x)){
  x[[i]] <- tryCatch(
    {
      print(paste(i, "Now reading:", fpxml[i]))
      readPDFXML(fpxml[i])
    },
    error = function(cond){
      message(paste(i, "Error reading:", fpxml[i]))
      message(cond)
      return(fpxml[i])
    },
    function(cond){
      message(paste(i, "Warning reading:", fpxml[i]))
      message(cond)
      return(fpxml[i])
    },
    finally = {message(paste("Successfully read:", fpxml[i]))}
  )
}

###########################################################################
# MOVED CODE TO `04_read_pdf_in_chunks.R` because code below is memory limited
###########################################################################

temp <- vector("list", length = length(x))

for(i in 1:length(x)){
  temp[[i]] <- tryCatch(
    {
      print(paste("Now converting:", i))
      getSectionText(x[[i]])
    },
    error = function(cond){
      message(paste("Error convering:", i))
      message(cond)
      return(i)
    },
    function(cond){
      message(paste("Warning converting:", i))
      message(cond)
      return(i)
    },
    finally = {message(paste("Successfully converted:", i))}
  )
}

