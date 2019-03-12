##########################################################################
# The purpose of this script is to log `readPDFXML()` errors
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

# read all XML files
fpxml <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers_xml", full = TRUE)
x <- vector("list", length = 100)

# develop an error handling appraoch to reading in XML files
# because, for example, between 1:20, idices 10, 17 don't read in
for(i in 1:100){
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

fail <- unlist(x[sapply(x, is.character)])

x <- vector("list", length = 309)

# develop an error handling appraoch to reading in XML files
# because, for example, between 1:20, idices 10, 17 don't read in
for(i in 101:200){
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

fail2 <- unlist(x[sapply(x, is.character)])


x <- vector("list", length = 309)

# develop an error handling appraoch to reading in XML files
# because, for example, between 1:20, idices 10, 17 don't read in
for(i in 201:309){
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

fail3 <- unlist(x[sapply(x, is.character)])

all_fail <- c(fail, fail2, fail3)
all_fail


# copy all failed XML into a dir
file.copy(all_fail,
          "/Users/richpauloo/Documents/GitHub/cig_nlp/readPDFXML_errors/xml")

# repeat for the PDFs that generated them
all_fail <- gsub(all_fail, pattern = "xml", replacement = "pdf")
all_fail <- gsub(all_fail, pattern = "all_papers_pdf", replacement = "all_papers")

file.copy(all_fail,
          "/Users/richpauloo/Documents/GitHub/cig_nlp/readPDFXML_errors/pdf")
