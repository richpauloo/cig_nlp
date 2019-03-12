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
# Locate errors from `readPDFXML` and `getSectionText`
##########################################################################

# failures were observed during `getSectionText` when list length is 0 or 1, 
# or the object size is prohibitively small to be a successful read
# these two conditions are true for all failures

# all pdf files
af  <- list.files("/Users/richpauloo/Desktop/2019 Citation/Papers 2/all_papers")

# getSectionText and readPDFXML success/failures 
success <- af[!sapply(d, function(x){length(x) %in% 0:1})]
fail    <- af[sapply(d, function(x){length(x) %in% 0:1})]

##########################################################################
# View list element names (section names) for each of the successes
##########################################################################

# subset for successes
s <- d[!sapply(d, function(x){length(x) %in% 0:1})]
names(s) <- success # label these list elements

# some of the objects are very long lists, and break when you try to 
# collapse their elements into a vector. I did some tests, and lists
# with length > 75 cause this problem. This amounts to 7 elements
sum(sapply(s, function(x){length(x) > 75}))

s <- s[sapply(s, function(x){length(x) <= 75})]


# collapse all nested list elements into vectors
for(i in 1:length(s)){
  s[[i]] <- lapply(s[[i]], 
                   function(x){
                     paste(x, collapse = " ")
                     }
                   )
  }

# combine into dataframes
for(i in 1:length(s)){
  s[[i]] <- as.data.frame(s[[i]]) %>% 
    tidyr::gather(section_name, text)
  s[[i]]$paper <- names(s)[i]
  s[[i]] <- dplyr::select(s[[i]], paper, section_name, text)
}

df <- dplyr::bind_rows(s)
df <- df %>% 
  mutate(nchar = nchar(text),
         text = substr(text, 1, 500),
         section_name = tolower(section_name)) %>% 
  select(paper, section_name, nchar, text)


##########################################################################
# Start cleaning
##########################################################################

# identify and label tables
df <- mutate(df, section_name = ifelse(grepl("table", section_name), 
                                       "table", 
                                       section_name)) 

# drop tables
df <- filter(df, section_name != "table")

# abstract/introduction
s_intro <- df %>% 
  group_by(paper) %>% 
  summarise(x = first(section_name)) %>% 
  pull(x) %>% 
  unique()

s_intro <- c("abstract","introduction","i.ntr.o.duction",
             "introduction.","introduction.regarding.the.seismological.aspects",
             "intr.oduction","motivation","introduction..","introduction.and.motivation",
             "doi.10.1038.nature11932", "doi.10.1038.nature10749", 
             "doi.10.1038.nature13728", "doi.", "doi.10.1038.nature12203")

# results/discussion


# references
s_ref <- df %>% 
  group_by(paper) %>% 
  summarise(x = last(section_name)) %>% 
  pull(x) %>% 
  unique()





