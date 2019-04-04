###########################################################################
# run lines 1-37 in `02_read_pdf_section_text_headers.R` to begin
# This script's purpose is to develop an error handling appraoch to 
# reading XML files to avoid breaking code, and because of memory timeout
# on my 2015 8GB Macbook Pro. I spilt the job into chunks of ~50 files,
# save intermediate files as `.rds`, combine them at the end with `c()`,
# and saved those as `Papers 2/data/all.rds`
###########################################################################

st <- Sys.time()

# run getSectionText() on all files. expect errors and memory timeouts, so 
# evaluate wtihin a tryCatch

x <- vector("list", length = length(fpxml))
x <- x[1:50]                         
temp <- vector("list", length = 50)

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

for(i in 1:50){
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

readr::write_rds(temp, "/Users/richpauloo/Desktop/2019 Citation/Papers 2/data/t050.rds")

###########################################################################

x <- vector("list", length = length(fpxml))
x <- x[51:100]                         
temp <- vector("list", length = 50)

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

for(i in 1:50){
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

readr::write_rds(temp, "/Users/richpauloo/Desktop/2019 Citation/Papers 2/data/t100.rds")

###########################################################################


x <- vector("list", length = length(fpxml))
x <- x[101:150]                         
temp <- vector("list", length = 50)

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

for(i in 1:50){
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

readr::write_rds(temp, "/Users/richpauloo/Desktop/2019 Citation/Papers 2/data/t150.rds")

###########################################################################

x <- vector("list", length = length(fpxml))
x <- x[151:200]                         
temp <- vector("list", length = 50)

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

for(i in 1:50){
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

readr::write_rds(temp, "/Users/richpauloo/Desktop/2019 Citation/Papers 2/data/t200.rds")

###########################################################################

x <- vector("list", length = length(fpxml))
x <- x[201:250]                         
temp <- vector("list", length = 50)

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

for(i in 1:50){
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

readr::write_rds(temp, "/Users/richpauloo/Desktop/2019 Citation/Papers 2/data/t250.rds")

###########################################################################

x <- vector("list", length = length(fpxml))
x <- x[251:309]                         
temp <- vector("list", length = 50)

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

for(i in 1:59){
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

readr::write_rds(temp, "/Users/richpauloo/Desktop/2019 Citation/Papers 2/data/t309.rds")

Sys.time() - st # how long this whole thing took: needs to run overnight ~ 8 hrs 


