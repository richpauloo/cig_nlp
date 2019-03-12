##########################################################################
# The purpose of this script is to reproduce `readPDFXML()` errors
# the original PDFs that generated the XML read by this file are in 
# ./pdf
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

# XML file paths
x <- list.files("/Users/richpauloo/Documents/GitHub/cig_nlp/getSectionText_errors/xml",
                full.names = TRUE)

# generate errors
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

