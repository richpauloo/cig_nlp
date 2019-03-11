# the purpose of this script is to take the output of 
# `04_read_pdf_in_chunks.R` and: 
# (1) identify which PDFs threw parsing errors on Read
# (2) for pdfs that successfully parsed, extract the text & section name
#     of each software mention. These will be input into the visualization
#     functions written in `01_extract_mention_windows.Rds` based on
#     incorrectly parsed PDFs via the `pdftools` package.

# packages
library(readr)
library(dplyr)

x <- vector("list", length = 20)

# develop an error handling appraoch to reading in XML files
# because, for example, between 1:20, idices 10, 17 don't read in
for(i in 1:20){
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

# save failures
fail <- unlist(x[sapply(x, is.character)])





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
