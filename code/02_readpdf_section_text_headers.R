###########################################################################
# Implement Duncan's ReadPDF::getSectionText()
# this function takes a PDF that's been converted to XML and returns a list with n elems
# each element corresponds to a "section" in the PDF. Huersitics are used to 
# determine section headers, and text between heads is reconstructed.
# element names refer to the section titles.
###########################################################################

# ensure dev version of XML
# devtools::install_github("omegahat/XML")
library(XML)

# tell R where to find the pdftohtml executable:
# see README at https://github.com/dsidavis/ReadPDF for instructions
options(PDFTOHTML = "/Users/richpauloo/Documents/GitHub/pdftohtml/src/pdftohtml")

# clone GH DSIProjects/ReadPDF/R

# Source all files from the repo to get functions in memory
ff = list.files("/Users/richpauloo/Documents/Github/ReadPDF/R", pattern = "\\.R$", full = TRUE)
invisible(lapply(ff, source))

# all papers filepaths
library(stringr)
pf  <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers", full = TRUE)
pf2 <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers") %>% 
  str_remove_all(".pdf")

# convert all new PDFs into XML
# for(i in 1:length(pf)){
#   convertPDF2XML(file = pf[i], 
#                  out = paste0("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers_xml/", pf2[i], ".xml"))
# }

# read all XML into a list and run getSectionText() on all files within an tryCatch
x <- vector("list", length = length(pf2))

# xml file paths
fpxml <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers_xml", full = TRUE)

# function to read files
read_dat <- function(fp){
  out <- tryCatch(
    {
      print(paste("Now reading:", fp))
      readPDFXML(fp)
    },
    error = function(cond){
      message(paste("Error reading:", fp))
      message(cond)
      return(fp)
    },
    function(cond){
      message(paste("Warning reading:", fp))
      message(cond)
      return(fp)
    },
    finally = {message(paste("Successfully read:", fp))}
  )
}

# between 1:20, 10, 17 don't read
for(i in 1:length(fpxml)){
  x[[i]] <- tryCatch(
    {
      print(paste("Now reading:", fpxml[i]))
      readPDFXML(fpxml[i])
    },
    error = function(cond){
      message(paste("Error reading:", fpxml[i]))
      message(cond)
      return(fpxml[i])
    },
    function(cond){
      message(paste("Warning reading:", fpxml[i]))
      message(cond)
      return(fpxml[i])
    },
    finally = {message(paste("Successfully read:", fpxml[i]))}
  )
}

x <- lapply(fpxml, read_dat)

fail <- unlist(x[sapply(x, is.character)])
x    <- x[sapply(x, function(x) !is.character(x))]
###########################################################################

###########################################################################
