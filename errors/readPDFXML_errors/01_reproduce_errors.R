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
fpxml <- list.files("/Users/richpauloo/Documents/GitHub/cig_nlp/readPDFXML_errors/xml",
                    full.names = TRUE)

# generate the errors
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
